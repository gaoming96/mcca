\name{pdi}
\alias{pdi}
\title{Calculate PDI Value}
\usage{
pdi(y, d, method="multinom", k=3)
}
\description{
compute the Polytomous Discrimination Index (PDI) value of three or four categories classifiers with an option to define the specific model or user-defined model.
}
\arguments{
\item{y}{The multinomial response vector with three or four categories. It can be factor or numerical.}
  \item{d}{The set of candidate markers, including one or more columns. Can be a data frame or a matrix; if the method is "prob", then d should be the probablity matrix.}
  \item{method}{specifies what method is used to construct the classifier based on the marker set in d. Available option includes the following methods:"multinom": Multinomial Logistic Regression which is the default method, requiring R package nnet;"tree": Classification Tree method, requiring R package rpart;"svm": Support Vector Machine (C-classification and radial basis as default), requiring R package e1071;"lda": Linear Discriminant Analysis, requiring R package lda;"mlp": Multiply Layer Perception, requiring R package mxnet;"prob": d is a risk matrix resulted from any external classification algorithm obtained by the user.}
  \item{k}{number of the categories, can be 3 or 4.}
}
\details{
The function returns the PDI value for predictive markers based on a user-chosen machine learning method. Currently available methods include logistic regression (default), tree, lda, svm, deep learning (mlp) and user-computed risk values. This function is general since we can evaluate the accuracy for marker combinations resulted from complicated classification algorithms.


}
\value{
The PDI value of the classification using a particular learning method on a set of marker(s).
}
\references{
Van Calster B, Vergouwe Y, Looman CWN, Van Belle
V, Timmerman D and Steyerberg EW. Assessing the
discriminative ability of risk models for more than two outcome
categories. European Journal of Epidemiology 2012; 27: 761?C
770.

Li, J., Feng, Q., Fine, J.P., Pencina, M.J., Van Calster, B. (2017). Nonparametric estimation and inference for polytomous discrimination index. Statistical Methods in Medical Research. In Press.
}
\author{
Gao Ming: gaoming960329@gmail.com

Li jialiang: stalj@nus.edu.sg
}
\note{
Users are advised to change the operating settings of various classifiers since it is well known that machine learning methods require extensive tuning. Currently only some common and intuitive options are set as default and they are by no means the optimal parameterization for a particular data analysis. A more flexible evaluation is to consider "method=prob" in which case the input d should be a matrix of membership probabilities with k columns and each row of d should sum to one.
}
\seealso{
hum
}
\examples{
rm(list=ls())
str(iris)
data <- iris[, 3]
label <- iris[, 5]
pdi(y = label, d = data,method = "tree", k = 3)
## [1] 0.9082667
pdi(y = label, d = data, method = "mlp", k = 3)
## [1] 0.9845333

rm(list=ls())
table(mtcars$carb)
for (i in (1:length(mtcars$carb))) {
    if (mtcars$carb[i] == 3 | mtcars$carb[i] == 6 | mtcars$carb[i] == 8) {
        mtcars$carb[i] <- 9
    }
}
data <- data.matrix(mtcars[, c(1)])
mtcars$carb <- factor(mtcars$carb, labels = c(1, 2, 3, 4))
label <- as.numeric(mtcars$carb)
str(mtcars)
pdi(y = label, d = data, "mlp", k = 4)
## [1] 0.4790714
}
\keyword{ PDI }
