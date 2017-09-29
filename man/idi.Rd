\name{idi}
\alias{idi}
\title{Calculate IDI Value}
\usage{
idi(y, m1, m2, method="multinom",n=3)
}
\description{
compute the integrated discrimination improvement (IDI) value of three or four categories classifiers with an option to define the specific model or user-defined model.
}
\arguments{
  \item{y}{the tri-nomial or quandr-nomial response, i.e., a single vector taking three distinct values, can be nominal or numerical.}
  \item{m1}{model 1, the continuous marker, can be a data frame or a matrix; if the method is “prob”, then m1 should be the probablity matrix.}
    \item{m2}{model 2, the continuous marker, can be a data frame or a matrix; if the method is “prob”, then m2 should be the probablity matrix.}
  \item{method}{the simplied name of one of the following most popular regressions:“multinom”: Multiple Logistic Regression which is the defalt method, requiring R package nnet;“tree”: Classification Tree without pruning, requiring R package rpart;“svm”: Support Vector Machine with parameters: C-classification and radial basis, requiring R package m1071;“lda”: Linear Discriminant Analysis, requiring R package lda;“mlp”: Multiply Layer Perception, requiring R package mxnet;“prob”: Regression arbitrarily.}
  \item{n}{number of the categories, can be 3 or 4.}
}
\details{
The function tries to get the IDI value, thus we can compare these five regression methods’ performances. For binary outputs, we can use AUC value, however, there exists little R function to compute IDI value for three category outputs.


}
\value{
The IDI value of the chosen method.
}
\references{
Li, J., Jiang, B. and Fine, J. P. (2013). Multicategory reclassification statistics for assessing Improvements in diagnostic accuracy. Biostatistics. 14(2): 382—394.

        Li, J., Jiang, B., and Fine, J. P. (2013). Letter to Editor: Response. Biostatistics. 14(4): 809-810.
}
\author{
Gao Ming, Li jialiang
}
\note{
Pay attention to notice that in MLP, I just only give a typical parameter which, in consequence, may meets error if the data input changes. Hence, feel safe to change these parameters in the function yourself.

It allows to use your own regression by easily setting the method to be “prob”. Notice that at this circumstance, the input m1 and m2 should be probability matrices with dimension N*3, which every RowSum should be 1.
}
\seealso{
nri
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
idi(y = label, m1 = data[, 1], m2 = data[, 2], "tree",4)
##[1] 0.2288213
idi(y = label, m1 = data[, 1], m2 = data[, 2], "mlp",4)
##[1] 0.05446744
}
\keyword{ IDI }
