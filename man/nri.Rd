\name{nri}
\alias{nri}
\title{Calculate NRI Value}
\usage{
nri(y, m1, m2, method="multinom",n=3)
}
\description{
compute the net reclassification improvement (NRI) value of three or four categories classifiers with an method to define the specific model or user can use his own model.
}
\arguments{
  \item{y}{the tri-nomial or quandr-nomial response, i.e., a single vector taking three distinct values, can be nominal or numerical.}
  \item{m1}{the continuous marker, can be a data frame or a matrix;  if the method is “label”, then m1 should be a label.}
    \item{m2}{the continuous marker, can be a data frame or a matrix;  if the method is “label”, then m2 should be a label.}
  \item{method}{the simplied name of one of the following most popular regressions:“multinom”: Multinomial Regression which is the defalt method, requiring R package nnet;“tree”: Classification Tree without pruning, requiring R package rpart;“svm”: Support Vector Machine with parameters: C-classification and radial basis, requiring R package m1071;“lda”: Linear Discriminant Analysis, requiring R package lda;“mlp”: Multiply Layer Perception, requiring R package mxnet;“label”: Regression arbitrarily.}
  \item{n}{number of the categories, can be 3 or 4.}
}
\details{
The function tries to get the NRI value, thus we can compare these five regression methods’ performances. For binary outputs, we can use AUC value, however, there exists little R function to compute NRI value for three category outputs.


}
\value{
The NRI value of the chosen method.
}
\references{
Li, J., Jiang, B. and Fine, J. P. (2013). Multicategory reclassification statistics for assessing Improvements in diagnostic accuracy. Biostatistics. 14(2): 382—394.

        Li, J., Jiang, B., and Fine, J. P. (2013). Letter to Editor: Response. Biostatistics. 14(4): 809-810.
}
\author{
Gao Ming, Li jialiang
}
\note{
Pay attention to notice that in MLP, I just only give a typical parameter which, in consequence, may meet error if the data input changes. Hence, feel safe to change these parameters in the function yourself.

It allows to use your own regression by easily setting the method to be “label”. Notice that at this circumstance, the input m1 and m2 should be a tri-nomial label, which is a vector of the same length as y.
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
nri(y = label, m1 = data[, 1], m2 = data[, 2], "mlp",4)
## [1] 0.025
nri(y = label, m1 = data[, 1], m2 = data[, 2], "lda",4)
## [1] 0.1
}
\keyword{ NRI }
