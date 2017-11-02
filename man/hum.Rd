\name{hum}
\alias{hum}
\title{Calculate HUM Value}
\usage{
hum(y, d, method="multinom", k=3)
}
\description{
compute the Hypervolume Under Manifold (HUM) value of three or four categories classifiers with an option to define the specific model or user-defined model.
}
\arguments{
  \item{y}{The multinomial response vector with three or four categories. It can be factor or numerical.}
  \item{d}{The set of candidate markers, including one or more columns. Can be a data frame or a matrix; if the method is "prob", then d should be the probablity matrix.}
  \item{method}{specifies what method is used to construct the classifier based on the marker set in d. Available option includes the following methods:"multinom": Multinomial Logistic Regression which is the default method, requiring R package nnet;"tree": Classification Tree method, requiring R package rpart;"svm": Support Vector Machine (C-classification and radial basis as default), requiring R package e1071;"lda": Linear Discriminant Analysis, requiring R package lda;"mlp": Multiply Layer Perception, requiring R package mxnet;"prob": d is a risk matrix resulted from any external classification algorithm obtained by the user.}
  \item{k}{number of the categories, can be 3 or 4.}
}
\details{
The function returns the HUM value for predictive markers based on a user-chosen machine learning method. Currently available methods include logistic regression (default), tree, lda, svm, deep learning (mlp) and user-computed risk values. For binary outcome, one can use AUC value (HUM reduces to AUC in such case). This function is more general than the package HUM, since we can evaluate the accuracy for marker combinations resulted from complicated classification algorithms.

}
\value{
The HUM value of the classification using a particular learning method on a set of marker(s).
}
\references{
Li, J. and Fine, J. P. (2008): ROC analysis with multiple tests and multiple classes: methodology and applications in microarray studies. Biostatistics. 9 (3): 566-576.

Li, J., Chow, Y., Wong, W.K., and Wong, T.Y. (2014). Sorting Multiple Classes in Multi-dimensional ROC Analysis: Parametric and Nonparametric Approaches. Biomarkers. 19(1): 1-8.
}
\author{
Gao Ming: gaoming960329@gmail.com

Li jialiang: stalj@nus.edu.sg
}
\note{
Users are advised to change the operating settings of various classifiers since it is well known that machine learning methods require extensive tuning. Currently only some common and intuitive options are set as default and they are by no means the optimal parameterization for a particular data analysis. A more flexible evaluation is to consider "method=prob" in which case the input d should be a matrix of membership probabilities with k columns and each row of d should sum to one.
}
\seealso{
pdi
}
\examples{
rm(list=ls())
str(iris)
data <- iris[, 1:4]
label <- iris[, 5]
hum(y = label, d = data,method = "multinom", k = 3)
## [1] 0.9972
hum(y = label, d = data, method = "tree", k = 3)
## [1] 0.998

data <- data.matrix(iris[, 1:4])
label <- as.numeric(iris[, 5])
# multinomial
require(nnet)
# model
fit <- multinom(label ~ data, maxit = 1000, MaxNWts = 2000)
predict.probs <- predict(fit, type = "probs")
pp<- data.frame(predict.probs)
# extract the probablity assessment vector
head(pp)
hum(y = label, d = pp, method = "prob", k = 3)
## [1] 0.9972

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
hum(y = label, d = data, method = "tree", k = 4)
## [1] 1
}
\keyword{ HUM }
