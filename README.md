# Machine Learning in Bioinformatics
> Máster en Bioinformática Aplicada a Medicina Personalizada y Salud (Curso 2019-2020)

# Scheduling
- Day 1 (5.02.2020):
	- 1/2 Theory
	- 1/2 Practice: Hands-On (Machine Learning Basics in R Part I)
- Day 2 (6.02.2020):
	- 1/2 Theory
	- 1/2 Practice: Hands-On (Machine Learning Basics in R Part II)
- Day 3 (7.02.2020):
	- 1/2 Theory
	- 1/2 Practice: Hands-On (Deep Learning for Image Classification)
	
# Theory

The theory slides are available [here](resources/theory-machinelearning.pdf).

# Practice

## Pulling the Docker images

Run the following comands to pull the Docker images for the hands-on practices:

```bash
docker pull singgroup/r-machine-learning:2019-2020
docker pull singgroup/jupyter-tensorflow-notebook
```

## Days 1 and 2 Practice: Hands-On (Machine Learning Basics in R)

We are going to use the `Breast Cancer Data` available at the [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Original)). More information about this dataset can be found [here](https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.names) and [here](https://www.kaggle.com/uciml/breast-cancer-wisconsin-data).

Go to an empty folder and run the following commands to download the data: 
```bash
mkdir data

wget https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data -O data/wdbc.data

sed -i '1iid,diagnosis,radius_mean,texture_mean,perimeter_mean,area_mean,smoothness_mean,compactness_mean,concavity_mean,concave points_mean,symmetry_mean,fractal_dimension_mean,radius_se,texture_se,perimeter_se,area_se,smoothness_se,compactness_se,concavity_se,concave_points_se,symmetry_se,fractal_dimension_se,radius_worst,texture_worst,perimeter_worst,area_worst,smoothness_worst,compactness_worst,concavity_worst,concave points_worst,symmetry_worst,fractal_dimension_worst' data/wdbc.data
```

Alternatively, the file is also available [here](data/wdbc.data).

Run the following command to use the Docker image with R and the required libraries already installed: 
```bash
docker run --rm -it -v $(pwd):$(pwd) -w $(pwd) singgroup/r-machine-learning:2019-2020 R
```

And now, run the following R instructions to load the data file:
```R
data <- read.csv("data/wdbc.data")
```

The full scripts to develop during this sessions are available [here](resources/analysis-1.R) and [here](resources/analysis-2.R).

## Day 3 Practice: Hands-On (Deep Learning for Image Classification)

The resources `resources/notebooks/` directory contains a script called `start-notebook.sh`. Simply run it to downlaod the test data and start the Jupyter notebook.

# References
- [Practical Statistics for Data Scientists: 50 Essential Concepts](https://www.oreilly.com/library/view/practical-statistics-for/9781491952955/)

# Additional Resources
- Ten quick tips for machine learning in computational biology [[10.1186/s13040-017-0155-3](https://dx.doi.org/10.1186%2Fs13040-017-0155-3)]
- [LIBSVM -- A Library for Support Vector Machines](https://www.csie.ntu.edu.tw/~cjlin/libsvm/)
- [The `caret` Package](http://topepo.github.io/caret/index.html)
- [A Short Introduction to the `caret` Package](https://rdrr.io/cran/caret/f/vignettes/caret.Rmd)
