#	---------------------------------------------------------------------------
#
#	Data Loading
#
#	---------------------------------------------------------------------------

data <- read.csv("data/wdbc.data")

data$diagnosis <- factor(data$diagnosis)
data <- data[,c(2:ncol(data))]

dim(data)
head(data)
summary(data)

dir.create("figures", showWarnings=FALSE)

#	---------------------------------------------------------------------------
#
#	Working with caret
#
#	---------------------------------------------------------------------------

library("caret")

set.seed(2020)

trainSamplesIndexes <- createDataPartition(
	y = data$diagnosis, 
	p = 0.7, 
	list = FALSE
)

train <- data[trainSamplesIndexes, ]
test <- data[-trainSamplesIndexes, ]

# 5x10-CV configuration
ctrl <- trainControl(
	method = "repeatedcv",
    number = 10,
    repeats = 5
)

printTrainTestPredictions <- function(model, train, test) {
	# Train data predictions
	pred.train <- predict(model)
	print(confusionMatrix(data = pred.train, train$diagnosis))

	# Test data predictions
	pred.test <- predict(model, newdata = test)
	print(confusionMatrix(data = pred.test, test$diagnosis))
}


#	---------------------------------------------------------------------------
#
#	Decision Tree
#
#	---------------------------------------------------------------------------

tree <- train(
	diagnosis ~ .,
	data = train,
	method = "rpart1SE",
	trControl = trainControl(method = "none")
)

printTrainTestPredictions(tree, train, test)

#
# Look at the caret available models for other rpart methods:
#   https://rdrr.io/cran/caret/man/models.html
#
# For instance, using "rpart" it is possible to fit a tree controlling the
# complexity parameter (cp). In this case, the train method selects the best 
# cp value using a 10-fold cross validation repeated 5 times.
#

# Note: the random number seed is set before each model is trained to ensure 
# that each one gets the same data partitions and repeats in the cross validation

set.seed(2020)
tree.2 <- train(
	diagnosis ~ ., 
	data = train, 
	method = "rpart",
	trControl = ctrl, 
	tuneGrid = expand.grid(cp=c(0.0003, 0.003, 0.03, 0.3))
)

#	---------------------------------------------------------------------------
#
#	Random Forest
#
#	---------------------------------------------------------------------------

randomforest <- train(
	diagnosis ~ .,
	data = train,
	method = "rf",
	trControl = trainControl(method = "none")
)

printTrainTestPredictions(randomforest, train, test)

#
# In the following case, the train method selects the best value for the number 
# randomly selected predictors at each split ("mtry") using a 10-fold cross 
# validation repeated 5 times.
#

set.seed(2020)
randomforest.2 <- train(
	diagnosis ~ ., 
	data = train, 
	method = "rf",
	trControl = ctrl,
	tuneGrid = expand.grid(mtry=seq(5,30,5))
)

#	---------------------------------------------------------------------------
#
#	SVM
#
#	---------------------------------------------------------------------------

svm <- train(
	diagnosis ~ concave.points_worst + area_worst,
	data = train,
	method = "svmLinear2",
	trControl = trainControl(method = "none")
)

printTrainTestPredictions(svm, train, test)

svm.all <- train(
	diagnosis ~ .,
	data = train,
	method = "svmLinear2",
	trControl = trainControl(method = "none")
)

printTrainTestPredictions(svm.all, train, test)

#
# In the following case, the train method selects the best value for
# the cost parameter.
#

set.seed(2020)
svm.2 <- train(
	diagnosis ~ .,
	data = train,
	method = "svmLinear2",
	trControl = ctrl,
	tuneGrid = expand.grid(cost=2**(-3:6))
)

#	---------------------------------------------------------------------------
#
#	Naive Bayes
#
#	---------------------------------------------------------------------------

# The default grid of the nb model (getModelInfo("nb", regex = FALSE)[[1]]$grid)
# includes the two values of usekernel, thus a train control method must be
# specified

set.seed(2020)
nb.2 <- train(
	diagnosis ~ .,
	data = train,
	method = "nb",
	trControl = ctrl
)

printTrainTestPredictions(nb.2, train, test)

#	---------------------------------------------------------------------------
#
#	Artificial Neural Networks
#
#	---------------------------------------------------------------------------

# Note: in caret, the mlp model only allows to use one hidden layer. Take a 
# look at mlpML for a model with up to three layers.

nn <- train(
	diagnosis ~ .,
	data = train,
	method = "mlp",
    preProcess = c("center", "scale"),
	trControl = trainControl(method = "none")
)

printTrainTestPredictions(nn, train, test)

#
# Print the neural network graph
#

library("NeuralNetTools")

png("figures/neural-network-2-caret.png")
plotnet(nn)
dev.off()

#
# In the following case, the train method selects the best value for
# the the number of units in the hidden layer
#

set.seed(2020)
nn.2 <- train(
	diagnosis ~ .,
	data = train,
	method = "mlp",
	trControl = ctrl,
	preProcess = c("center", "scale"),
	tuneGrid = expand.grid(size=c(1,2,4,8,16))
)

#	---------------------------------------------------------------------------
#
#	Comparing Classifiers
#
#	---------------------------------------------------------------------------

# List of models that will be evaluated
modelFits <- list(rpart= tree.2, rf = randomforest.2, svmLinear2 = svm.2, nb = nb.2, mlp = nn.2)

# Resamples allow model fit comparison. To select each winning model, 50 results 
# have been evaluated (5 repeats of the 10-fold cross-validation). The goal here is
# to compare the accuracy and kappa distributions (50 values for each) between the 
# 5 models

results <- resamples(modelFits)

summary(results)

png("figures/classification-caret-bwplot.png")
bwplot(results)
dev.off()

png("figures/classification-caret-dotplot.png")
dotplot(results)
dev.off()

# Select the best model and get the final evaluation on the test data
# printTrainTestPredictions(<best_model>, train, test)
