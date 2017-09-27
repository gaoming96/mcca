\name{ccp}
\alias{ccp}
\title{Calculate CCP Value}
\usage{
ccp(y, d, method="multinom",n=3)
}
\description{
compute the Correct Classification Percentage (CCP) value of three or four categories classifiers with an method to define the specific model or user can use his own model.
}
\arguments{
  \item{y}{the tri-nomial or quandr-nomial response, i.e., a single vector taking three distinct values, can be nominal or numerical.}
  \item{d}{the continuous marker, can be a data frame or a matrix; if the method is “label”, then d should be a label.}
  \item{method}{the simplied name of one of the following most popular regressions:“multinom”: Multinomial Regression which is the defalt method, requiring R package nnet;“tree”: Classification Tree without pruning, requiring R package rpart;“svm”: Support Vector Machine with parameters: C-classification and radial basis, requiring R package e1071;“lda”: Linear Discriminant Analysis, requiring R package lda;“mlp”: Multiply Layer Perception, requiring R package mxnet;“label”: Regression arbitrarily.}
  \item{n}{number of the categories, can be 3 or 4.}
}
\details{
The function tries to get the CCP value, thus we can compare these five regression methods’ performances. For binary outputs, we can use AUC value, however, there exists little R function to compute CCP value for three category outputs.

}
\value{
The CCP value of the chosen method.
}
\references{
Li, J., Jiang, B. and Fine, J. P. (2013). Multicategory reclassification statistics for assessing Improvements in diagnostic accuracy. Biostatistics. 14(2): 382—394.

        Li, J., Jiang, B., and Fine, J. P. (2013). Letter to Editor: Response. Biostatistics. 14(4): 809-810.
}
\author{
Gao Ming, Li Jialiang
}
\note{
Pay attention to notice that in MLP, I just only give a typical parameter which, in consequence, may meet error if the data input changes. Hence, feel safe to change these parameters in the function yourself.

It allows to use your own regression by easily setting the method to be “label”. Notice that at this circumstance, the input d should be a tri-nomial or quandr-nomial label, which is a vector of the same length as y.
}
\seealso{
PDIHandle
}
\examples{
rm(list=ls())
str(iris)
data <- iris[, 1:4]
label <- iris[, 5]
ccp(y = label, d = data,method="multinom",n=3)
## [1] 0.9866667
ccp(y = label, d = data, method = "tree",n=3)
## [1] 0.998
p = as.numeric(label)
ccp(y = label, d = p, method = "label",n=3)

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
ccp(y = label, d = data, method="svm",n=4)
## [1] 0.3857143
}
\keyword{ CCP }
