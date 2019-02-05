\name{estp}
\alias{estp}
\title{Inference for Accuracy Improvement Measures based on Bootstrap}
\usage{
estp(y, m1, m2, acc="idi", level=0.95, method="multinom", k=3, B=250, balance=FALSE, \dots)
}
\description{
compute the bootstrap standard error and confidence interval for the classification accuracy improvement for a pair of nested models.
}
\arguments{
  \item{y}{The multinomial response vector with two, three or four categories. It can be factor or integer-valued.}
  \item{m1}{The set of marker(s) included in the baseline model, can be a data frame or a matrix; if the method is "prob", then m1 should be the prediction probablity matrix of the baseline model.}
  \item{m2}{The set of additional marker(s) included in the improved model, can be a data frame or a matrix; if the method is "prob", then m2 should be the prediction probablity matrix of the improved model.}
  \item{acc}{Accuracy measure to be evaluated. Allow two choices: "idi", "nri".}
  \item{level}{The confidence level. Default value is 0.95.}
  \item{method}{Specifies what method is used to construct the classifier based on the marker set in m1 & m2. Available option includes the following methods:"multinom": Multinomial Logistic Regression which is the default method, requiring R package nnet;"tree": Classification Tree method, requiring R package rpart;"svm": Support Vector Machine (C-classification and radial basis as default), requiring R package e1071;"lda": Linear Discriminant Analysis, requiring R package lda;"prob": m1 & m2 are risk matrices resulted from any external classification algorithm obtained by the user.}
  \item{k}{Number of the categories, can be 2, 3 or 4.}
  \item{B}{Number of bootstrap resamples.}
  \item{balance}{Logical, if TRUE, the class prevalence of the bootstrap sample is forced to be identical to the class prevalence of the original sample. Otherwise the prevalence of the bootstrap sample may be random.}
  \item{\dots}{Additional arguments in the chosen method's function.}
}
\details{
The function returns the standard error and confidence interval for a paired model evaluation method. All the other arguments are the same as the function \code{\link{hum}}.

}
\value{
\item{value}{The specific value of the classification using a particular learning method on a set of marker(s).}
\item{se}{The standard error of the value.}
\item{interval}{The confidence interval of the value.}
}

\author{
Ming Gao: gaoming96@sjtu.edu.cn

Jialiang Li: stalj@nus.edu.sg
}
\note{
Users are advised to change the operating settings of various classifiers since it is well known that machine learning methods require extensive tuning. Currently only some common and intuitive options are set as default and they are by no means the optimal parameterization for a particular data analysis. Users can put machine learning methods' parameters after tuning. A more flexible evaluation is to consider "method=prob" in which case the input m1 & m2 should be a matrix of membership probabilities with k columns and each row of m1 & m2 should sum to one.

}
\seealso{
\code{\link{ests}}
}
\examples{
rm(list=ls())
table(mtcars$carb)
for (i in (1:length(mtcars$carb))) {
  if (mtcars$carb[i] == 3 | mtcars$carb[i] == 6 | mtcars$carb[i] == 8) {
    mtcars$carb[i] <- 9
  }
}
data <- data.matrix(mtcars[, c(1, 5)])
mtcars$carb <- factor(mtcars$carb, labels = c(1, 2, 3, 4))
label <- as.numeric(mtcars$carb)
str(mtcars)
estp(y = label, m1 = data[, 1], m2 = data[, 2], acc="idi",method="lda", k=4,B=10)

## $value
## [1] 0.1235644

## $se
## [1] 0.07053541

## $interval
## [1] 0.05298885 0.21915088

estp(y = label, m1 = data[, 1], m2 = data[, 2], acc="nri",method="tree", k=4,B=5)

## $value
## [1] 0.05

## $se
## [1] 0.09249111

## $interval
## [1] 0.0000000  0.1458333

}
\keyword{ IDI }
