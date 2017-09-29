\name{hum}
\alias{hum}
\title{Calculate HUM Value}
\usage{
hum(y, d, method="multinom",n=3)
}
\description{
compute the Hypervolume Under Manifold (HUM) value of three or four categories classifiers with an option to define the specific model or user-defined model.
}
\arguments{
  \item{y}{the tri-nomial or quandr-nomial response, i.e., a single vector taking three distinct values, can be nominal or numerical.}
  \item{d}{the continuous marker, can be a data frame or a matrix; if the method is "prob", then d should be the probablity matrix.}
  \item{method}{the simplied name of one of the following most popular regressions:"multinom": Multiple Logistic Regression which is the defalt method, requiring R package nnet;"tree": Classification Tree without pruning, requiring R package rpart;"svm": Support Vector Machine with parameters: C-classification and radial basis, requiring R package e1071;"lda": Linear Discriminant Analysis, requiring R package lda;"mlp": Multiply Layer Perception, requiring R package mxnet;"prob": Regression arbitrarily.}
  \item{n}{number of the categories, can be 3 or 4.}
}
\details{
The function tries to get the HUM value, thus we can compare these five regression methods’ performances. For binary outputs, we can use AUC value, however, there exists little R function to compute HUM value for three category outputs.

This function performs better than the package HUM, since we can specific the type of regression.

}
\value{
The HUM value of the chosen method.
}
\references{
Li, J. and Fine, J. P. (2008): ROC analysis with multiple tests and multiple classes: methodology and applications in microarray studies. Biostatistics. 9 (3): 566-576.

Li, J., Chow, Y., Wong, W.K., and Wong, T.Y. (2014). Sorting Multiple Classes in Multi-dimensional ROC Analysis: Parametric and Nonparametric Approaches. Biomarkers. 19(1): 1-8.
}
\author{
Gao Ming, Li jialiang
}
\note{
Pay attention to notice that in MLP, I just only give a typical parameter which, in consequence, may meets error if the data input changes. Hence, feel safe to change these parameters in the function yourself.

It allows to use your own regression by easily setting the method to be "prob". Notice that at this circumstance, the input d should be a probability matrix with dimension N*3 or N*4, which every RowSum should be 1.
}
\seealso{
pdi
}
\examples{
rm(list=ls())
str(iris)
data <- iris[, 1:4]
label <- iris[, 5]
hum(y = label, d = data,method="multinom",n=3)
## [1] 0.9972
hum(y = label, d = data, method = "tree",n=3)
## [1] 0.998

rm(list=ls())
table(mtcars$carb)
for (i in (1:length(mtcars$carb))) {
    if (mtcars$carb[i] == 3 | mtcars$carb[i] == 6 | mtcars$carb[i] == 8) {
        mtcars$carb[i] <- 9
    }
}
data <- data.matrix(mtcars[, c(1:10)])
mtcars$carb <- factor(mtcars$carb, labels = c(1, 2, 3, 4))
label <- as.numeric(mtcars$carb)
str(mtcars)
hum(y = label, d = data, method="tree",n=4)
## [1] 1
}
\keyword{ HUM }
