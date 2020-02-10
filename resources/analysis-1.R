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

set.seed(2020)

trainSamples <- ceiling(0.7*nrow(data))
trainSamplesIndexes <- sample(1:nrow(data), trainSamples)

train <- data[trainSamplesIndexes,]
test <- data[-trainSamplesIndexes,]

dir.create("figures", showWarnings=FALSE)

#	---------------------------------------------------------------------------
#
#	Decision Tree
#
#	---------------------------------------------------------------------------

library(rpart)
library(rpart.plot)

tree <- rpart(diagnosis~., method='class', data=train)
tree

png("figures/tree-1.png")
plot(tree, uniform=TRUE, margin=.05)
text(tree, use.n=TRUE)
dev.off()

png("figures/tree-2.png")
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
dev.off()

trainTestDecisionTree <- function(tree, train, test) {
    # Train data predictions
    tree.pred.train <- predict(tree, type='class')
    print(table(tree.pred.train, train$diagnosis))

    # Test data predictions
    tree.pred.test <- predict(tree, newdata=test, type='class')
    print(table(tree.pred.test, test$diagnosis))
}

trainTestDecisionTree(tree, train, test)

#	---------------------------------------------------------------------------
#
#	Decision Tree parameters: minbucket and cp (complexity parameter)
#
#	---------------------------------------------------------------------------

# Type ?rpart.control to see the rpart tuning parameters.
# Also available at: https://stat.ethz.ch/R-manual/R-devel/library/rpart/html/rpart.control.html

tree.minBucket <- rpart(diagnosis~., method='class', data=train, minbucket=16)

trainTestDecisionTree(tree.minBucket, train, test)

# Default cp is 0.01. If cp is too small, the tree will tend to grow and produce overfitting.
# On the other hand, if cp is too long, the tree will be short (a few rules) and produce
# underfitting.

tree.cp1 <- rpart(diagnosis~., method='class', data=train, cp=0.07)

trainTestDecisionTree(tree.cp1, train, test)

tree.cp2 <- rpart(diagnosis~., method='class', data=train, cp=0.003)

trainTestDecisionTree(tree.cp2, train, test)

#	---------------------------------------------------------------------------
#
#	Exploratory Data Analysis: plot some variables used in the tree model
#
#	---------------------------------------------------------------------------

png("figures/concave.points_worst.png", width=1500, height=500)
par(mfrow=c(1,3))
boxplot(concave.points_worst ~ diagnosis, data=data, col=(c("gold","darkgreen")), main="All data")
boxplot(concave.points_worst ~ diagnosis, data=train, col=(c("gold","darkgreen")), main="Train")
boxplot(concave.points_worst ~ diagnosis, data=test, col=(c("gold","darkgreen")), main="Test")
dev.off()

png("figures/area_worst.png", width=1500, height=500)
par(mfrow=c(1,3))
boxplot(area_worst ~ diagnosis, data=data, col=(c("gold","darkgreen")), main="All data")
boxplot(area_worst ~ diagnosis, data=train, col=(c("gold","darkgreen")), main="Train")
boxplot(area_worst ~ diagnosis, data=test, col=(c("gold","darkgreen")), main="Test")
dev.off()

png("figures/area_se.png", width=1500, height=500)
par(mfrow=c(1,3))
boxplot(area_se ~ diagnosis, data=data, col=(c("gold","darkgreen")), main="All data")
boxplot(area_se ~ diagnosis, data=train, col=(c("gold","darkgreen")), main="Train")
boxplot(area_se ~ diagnosis, data=test, col=(c("gold","darkgreen")), main="Test")
dev.off()

#	---------------------------------------------------------------------------
#
#	Random Forest
#
#	---------------------------------------------------------------------------

library(randomForest)
library(ggplot2)

randomforest <- randomForest(diagnosis~., data=train)
randomforest

# Train data predictions
randomforest.pred.train <- predict(randomforest)
table(randomforest.pred.train, train$diagnosis)

# Test data predictions
randomforest.pred.test <- predict(randomforest, newdata=test)
table(randomforest.pred.test, test$diagnosis)

# Train a model with different numbers of trees
for(ntree in c(250, 500,1000,1500,2000)) {
	print(randomForest(diagnosis~., data=train, ntree=ntree))
}

# Out-of-bag (OOB) error
error_df <- data.frame(error_rate=randomforest$err.rate[,'OOB'], num_trees=1:randomforest$ntree)
png("figures/random-forest-oob-error.png")
ggplot(error_df, aes(x=num_trees, y=error_rate)) + geom_line()
dev.off()

# Variable importance
randomforest$importance[order(randomforest$importance, decreasing=TRUE),]

png("figures/random-forest-variable-importance.png", width=400, height=400)
varImpPlot(randomforest, type=2, n.var=10)
dev.off()

png("figures/perimeter_worst.png", width=1500, height=500)
par(mfrow=c(1,3))
boxplot(perimeter_worst ~ diagnosis, data=data, col=(c("gold","darkgreen")), main="All data")
boxplot(perimeter_worst ~ diagnosis, data=train, col=(c("gold","darkgreen")), main="Train")
boxplot(perimeter_worst ~ diagnosis, data=test, col=(c("gold","darkgreen")), main="Test")
dev.off()

#	---------------------------------------------------------------------------
#
#	SVM
#
#	---------------------------------------------------------------------------

library("e1071")
library("ggplot2")

# Plot the variables concave.points_worst and area_worst data from the train data
png("figures/svm-1-data-train.png")
ggplot(data=train, aes(x=area_worst, y=concave.points_worst, color=diagnosis, shape=diagnosis)) + 
  geom_point(size=2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position="none")
dev.off()

# Train the model based only in this two variables
svm <- svm(diagnosis ~ concave.points_worst + area_worst, data=train, kernel='linear') 
svm

trainTestSvm <- function(svm, train, test) {
    # Train data predictions
    svm.pred.train <- predict(svm)
    print(table(svm.pred.train, train$diagnosis))

    # Test data predictions
    svm.pred.test <- predict(svm, newdata=test)
    print(table(svm.pred.test, test$diagnosis))
}

trainTestSvm(svm, train, test)

png("figures/svm-1-classifier.png")
plot(svm, data=train, concave.points_worst ~ area_worst)
dev.off()

# Train the model using all variables
svm.all <- svm(diagnosis~., data=train, kernel='linear')
svm.all

trainTestSvm(svm.all, train, test)

#
# Important: by default, the svm function scales the data.
#

# See the mean and sd values of the two variables in the train data and in the train data scaled

colMeans(train[,c("concave.points_worst", "area_worst")])
apply(train[,c("concave.points_worst", "area_worst")], 2, sd)

colMeans(scale(train[,c("concave.points_worst", "area_worst")]))
apply(scale(train[,c("concave.points_worst", "area_worst")]), 2, sd)

# Train the models using scale=FALSE

svm.unscaled <- svm(diagnosis ~ concave.points_worst + area_worst, data=train, kernel='linear', scale=FALSE) 
svm.unscaled

trainTestSvm(svm.unscaled, train, test)

png("figures/svm-2-classifier-unscaled.png")
plot(svm.unscaled, data=train, concave.points_worst ~ area_worst)
dev.off()

svm.all.unscaled <- svm(diagnosis~., data=train, kernel='linear', scale=FALSE)
svm.all.unscaled

trainTestSvm(svm.all.unscaled, train, test)

#
# Changing the kernel: using kernel='radial' (with scale=TRUE, which is the default)
#

svm.radial <- svm(diagnosis ~ concave.points_worst + area_worst, data=train, kernel='radial') 
svm.radial

trainTestSvm(svm.radial, train, test)

png("figures/svm-4-classifier-radial.png")
plot(svm.radial, data=train, concave.points_worst ~ area_worst)
dev.off()

svm.all.radial <- svm(diagnosis~., data=train, kernel='radial')
svm.all.radial

trainTestSvm(svm.all.radial, train, test)

#
# => See what happens using kernel='radial' and scale='FALSE'
#

#	---------------------------------------------------------------------------
#
#	Naive Bayes (using the "e1071" library)
#
#	---------------------------------------------------------------------------

library("e1071")

naivebayes <- naiveBayes(diagnosis~., data=train)
naivebayes

# Train data predictions
naivebayes.pred.train <- predict(naivebayes, train)
table(naivebayes.pred.train, train$diagnosis)

# Test data predictions
naivebayes.pred.test <- predict(naivebayes, newdata=test)
table(naivebayes.pred.test, test$diagnosis)

# Get raw predictions
naivebayes.pred.raw.test <- predict(naivebayes, newdata=test, type="raw")
naivebayes.pred.raw.test

# Compare raw predictions with predicted and actual classes
equals <- (naivebayes.pred.test == test$diagnosis)
comparisons <- data.frame(naivebayes.pred.raw.test, naivebayes.pred.test, test$diagnosis, equals)
comparisons

#	---------------------------------------------------------------------------
#
#	Naive Bayes (using the "klaR" library)
#
#	---------------------------------------------------------------------------

library("klaR")

naivebayes.klar <- NaiveBayes(diagnosis~., data=train)
naivebayes.klar

# Train data predictions
naivebayes.pred.train.klar <- predict(naivebayes.klar, train)

# Now, naivebayes.pred.train is an object with two attributes: 
# - class: equivalent to the previous naivebayes.pred.train class predictions
# - posterior: equivalent to the previous raw predictions

table(naivebayes.pred.train.klar$class, train$diagnosis)

# Test data predictions
naivebayes.pred.test.klar <- predict(naivebayes.klar, newdata=test)
table(naivebayes.pred.test.klar$class, test$diagnosis)

# Compare raw predictions with predicted and actual classes
equals.klar <- (naivebayes.pred.test.klar$class == test$diagnosis)
comparisons.klar <- data.frame(naivebayes.pred.test.klar$posterior, naivebayes.pred.test.klar$class, test$diagnosis, equals.klar)
comparisons.klar

#	---------------------------------------------------------------------------
#
#	Artificial Neural Networks
#
#	---------------------------------------------------------------------------

library("RSNNS")

x <- train[,-1]
y <- ifelse(train[,1]=="B", 0, 1)

nn <- mlp(x, y, size=c(5))
nn

# Train data predictions
nn.pred.train <- predict(nn)
nn.pred.train <- ifelse(nn.pred.train> 0.5, 1, 0)
table(nn.pred.train, y)

# Test data predictions
nn.pred.test <- predict(nn, newdata=test[,-1])
nn.pred.test <- ifelse(nn.pred.test> 0.5, 1, 0)
y.test <- ifelse(test[,1]=="B", 0, 1)
table(nn.pred.test, y.test)

#
# Something weird in the confusion matrices? Let's scale the data and retrain
# the model
#

x <- scale(x)
test.scale <- scale(test[,-1])

nn.2 <- mlp(x, y, size=c(5))
nn.2

# Train data predictions
nn.pred.train <- predict(nn.2)
nn.pred.train <- ifelse(nn.pred.train> 0.5, 1, 0)
table(nn.pred.train, y)

# Test data predictions
nn.pred.test <- predict(nn.2, newdata=test.scale)
nn.pred.test <- ifelse(nn.pred.test> 0.5, 1, 0)
table(nn.pred.test, ifelse(test[,1]=="B", 0, 1))

#
# Print the neural network graph
#

library("NeuralNetTools")

png("figures/neural-network-1.png")
plotnet(nn)
dev.off()

#	---------------------------------------------------------------------------
#
#	Evaluating Classification Models
#
#	---------------------------------------------------------------------------

pred <- naivebayes.pred.test

# Confusion matrix

pred_y <- as.numeric(pred == "M")
true_y <- as.numeric(test$diagnosis == "M")
true_positive <- (true_y==1) & (pred_y==1)
true_negative <- (true_y==0) & (pred_y==0)
false_positive <- (true_y==0) & (pred_y==1)
false_negative <- (true_y==1) & (pred_y==0)

confusion_matrix <- matrix(c(
		sum(true_negative), sum(false_positive),
		sum(false_negative), sum(true_positive)
	),2,2
)
rownames(confusion_matrix) <- c("Predicted: B", "Predicted: M")
colnames(confusion_matrix) <- c("Actual: B", "Actual: M")

# Compare this confusion matrix with the one created using the table function
confusion_matrix
table(pred, test$diagnosis)

# Accuracy: measure of total error
(accuracy <- (sum(true_positive) + sum(true_negative)) / length(pred_y))

# Precision, Recall (Sensitivity) and Specificity

# Precision is the percentage of samples predicted as positive ("M") that are 
# actually positive.
(precision <- sum(true_positive) / (sum(true_positive) + sum(false_positive)))

# Recall (or Sensitivity) is the proportion of positive samples ("M") that are
# identified correctly (True Positive Rate)
(recall <- sum(true_positive) / (sum(true_positive) + sum(false_negative)))

# Specificity is the proportion of negative samples ("B") that are
# identified correctly (True Negative Rate)
(specificity <- sum(true_negative) / (sum(true_negative) + sum(false_positive)))

# ROC Curves

library(ggplot2)

pred <- naivebayes.pred.raw.test
pred <- pred[,2]

# To analyze the neural network test predictions simple use:
# pred <- predict(nn.2, newdata=test.scale)

idx <- order(-pred)
recall <- cumsum(true_y[idx]==1)/sum(true_y==1)
specificity <- (sum(true_y==0) - cumsum(true_y[idx]==0)) / sum(true_y==0)
roc_df <- data.frame(recall=recall, specificity=specificity)

png("figures/roc-1.png")
plot(roc_df$specificity, roc_df$recall, xlim=rev(range(roc_df$specificity)), type="l")
dev.off()

png("figures/roc-2.png")
ggplot(roc_df, aes(x=specificity, y=recall)) +
	geom_line(color='blue') +
	scale_x_reverse(expand=c(0, 0)) +
	scale_y_continuous(expand=c(0, 0)) +
	geom_line(data=data.frame(x=(0:100)/100), aes(x=x, y=1-x), linetype='dotted', color='red')
dev.off()

library("pROC")

png("figures/roc-3.png")
plot.roc(test$diagnosis, pred)
dev.off()

# Area Under the Curve (AUC)

auc <- sum(roc_df$recall[-1] * diff(1-roc_df$specificity))
