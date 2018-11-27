\name{nri}
\alias{nri}
\title{Calculate NRI Value}
\usage{
nri(y, m1, m2, method="multinom", k=3, \dots)
}
\description{
compute the net reclassification improvement (NRI) value of two or three or four categories classifiers with an option to define the specific model or user-defined model.
}
\arguments{
  \item{y}{The multinomial response vector with two, three or four categories. It can be factor or integer-valued.}
  \item{m1}{The set of marker(s) included in the baseline model, can be a data frame or a matrix; if the method is "prob", then m1 should be the prediction probablity matrix of the baseline model.}
  \item{m2}{The set of additional marker(s) included in the improved model, can be a data frame or a matrix; if the method is "prob", then m2 should be the prediction probablity matrix of the improved model.}
  \item{method}{Specifies what method is used to construct the classifier based on the marker set in m1 & m2. Available option includes the following methods:"multinom": Multinomial Logistic Regression which is the default method, requiring R package nnet;"tree": Classification Tree method, requiring R package rpart;"svm": Support Vector Machine (C-classification and radial basis as default), requiring R package e1071;"lda": Linear Discriminant Analysis, requiring R package lda;"label": m1 & m2 are label vectors resulted from any external classification algorithm obtained by the user;"prob": m1 & m2 are probability matrices resulted from any external classification algorithm obtained by the user.}
  \item{k}{Number of the categories, can be 2 or 3 or 4.}
  \item{\dots}{Additional arguments in the chosen method's function.}
}
\details{
The function returns the NRI value for predictive markers based on a user-chosen machine learning method. Currently available methods include logistic regression (default), tree, lda, svm and user-computed risk values. This function is general since we can evaluate the accuracy for marker combinations resulted from complicated classification algorithms.

}
\value{
The NRI value of the classification using a particular learning method on a set of marker(s).
}
\references{
Li, J., Jiang, B. and Fine, J. P. (2013). Multicategory reclassification statistics for assessing Improvements in diagnostic accuracy. Biostatistics. 14(2): 382—394.

Li, J., Jiang, B., and Fine, J. P. (2013). Letter to Editor: Response. Biostatistics. 14(4): 809-810.
}
\author{
Ming Gao: gaoming96@sjtu.edu.cn

Jialiang Li: stalj@nus.edu.sg
}
\note{
Users are advised to change the operating settings of various classifiers since it is well known that machine learning methods require extensive tuning. Currently only some common and intuitive options are set as default and they are by no means the optimal parameterization for a particular data analysis. Users can put machine learning methods' parameters after tuning. A more flexible evaluation is to consider "method=prob" in which case the input m1 & m2 should be a matrix of membership probabilities with k columns and each row of m1 & m2 should sum to one.
}
\seealso{
\code{\link{idi}}
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

nri(y = label, m1 = data[, 1], m2 = data[, 2], "lda", 4)
## [1] 0.1
nri(y = label, m1 = data[, 1], m2 = data[, 2], "tree", 4)
## [1] 0.05
nri(y = label, m1 = data[, 1], m2 = data[, 2], "tree", 4,control=rpart::rpart.control(minsplit=4))
## [1] 0.1357143
}
\keyword{ NRI }
