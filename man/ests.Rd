\name{ests}
\alias{ests}
\title{Inference for Accuracy Measures based on Bootstrap}
\usage{
ests(y, d, acc="hum", level=0.95, method="multinom", k=3, B=250, balance=FALSE, \dots)
}
\description{
compute the bootstrap standard error and confidence interval for the classification accuracy for a single classification model.
}
\arguments{
  \item{y}{The multinomial response vector with two, three or four categories. It can be factor or integer-valued.}
  \item{d}{The set of candidate markers, including one or more columns. Can be a data frame or a matrix; if the method is "prob", then d should be the probability matrix.}
  \item{acc}{Accuracy measure to be evaluated. Allow four choices: "hum", "pdi", "ccp" and "rsq".}
  \item{level}{The confidence level. Default value is 0.95.}
  \item{method}{Specifies what method is used to construct the classifier based on the marker set in d. Available option includes the following methods:"multinom": Multinomial Logistic Regression which is the default method, requiring R package nnet;"tree": Classification Tree method, requiring R package rpart; "svm": Support Vector Machine (C-classification and radial basis as default), requiring R package e1071; "lda": Linear Discriminant Analysis, requiring R package lda; "label": d is a label vector resulted from any external classification algorithm obtained by the user, should be encoded from 1; "prob": d is a probability matrix resulted from any external classification algorithm obtained by the user.}
  \item{k}{Number of the categories, can be 2, 3 or 4.}
  \item{B}{Number of bootstrap resamples.}
  \item{balance}{Logical, if TRUE, the class prevalence of the bootstrap sample is forced to be identical to the class prevalence of the original sample. Otherwise the prevalence of the bootstrap sample may be random.}
  \item{\dots}{Additional arguments in the chosen method's function.}
}
\details{
The function returns the standard error and confidence interval for a single model evaluation method. All the other arguments are the same as the function \code{\link{hum}}.

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
Users are advised to change the operating settings of various classifiers since it is well known that machine learning methods require extensive tuning. Currently only some common and intuitive options are set as default and they are by no means the optimal parameterization for a particular data analysis. Users can put machine learning methods' parameters after tuning. A more flexible evaluation is to consider "method=prob" in which case the input d should be a matrix of membership probabilities with k columns and each row of d should sum to one.


}
\seealso{
\code{\link{estp}}
}
\examples{
rm(list=ls())
str(iris)
data <- iris[, 1:4]
label <- iris[, 5]
ests(y = label, d = data,acc="hum",level=0.95,method = "multinom", k = 3,B=10,trace=FALSE)

## $value
## [1] 0.9972

## $se
## [1] 0.002051529

## $interval
## [1] 0.9935662 1.0000000

ests(y = label, d = data,acc="pdi",level=0.85,method = "tree", k = 3,B=10)

## $value
## [1] 0.9213333

## $se
## [1] 0.02148812

## $interval
## [1] 0.9019608 0.9629630

rm(list=ls())
table(mtcars$carb)
for (i in (1:length(mtcars$carb))) {
  if (mtcars$carb[i] == 3 | mtcars$carb[i] == 6 | mtcars$carb[i] == 8) {
    mtcars$carb[i] <- 9
  }
}
data <- data.matrix(mtcars[, c(1:2)])
mtcars$carb <- factor(mtcars$carb, labels = c(1, 2, 3, 4))
label <- as.numeric(mtcars$carb)
str(mtcars)
ests(y = label, d = data,acc="hum",level=0.95,method = "multinom", k = 4,trace=FALSE,B=5)

## $value
## [1] 0.2822857

## $se
## [1] 0.170327

## $interval
## [1] 0.2662500 0.4494643

}
\keyword{ CCR }
