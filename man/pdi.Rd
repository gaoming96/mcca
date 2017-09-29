\name{pdi}
\alias{pdi}
\title{Calculate PDI Value}
\usage{
pdi(y, d, method="multinom",n=3)
}
\description{
compute the Polytomous Discrimination Index (PDI) value of three or four categories classifiers with an option to define the specific model or user-defined model.
}
\arguments{
  \item{y}{the tri-nomial or quandr-nomial response, i.e., a single vector taking three distinct values, can be nominal or numerical.}
  \item{d}{the continuous marker, can be a data frame or a matrix; if the method is "prob", then d should be the probablity matrix.}
  \item{method}{the simplied name of one of the following most popular regressions:"multinom": Multiple Logistic Regression which is the defalt method, requiring R package nnet;"tree": Classification Tree without pruning, requiring R package rpart;"svm": Support Vector Machine with parameters: C-classification and radial basis, requiring R package e1071;"lda": Linear Discriminant Analysis, requiring R package lda;"mlp": Multiply Layer Perception, requiring R package mxnet;"prob": Regression arbitrarily.}
  \item{n}{number of the categories, can be 3 or 4.}
}
\details{
The function tries to get the PDI value, thus we can compare these five regression methods' performances. For binary outputs, we can use AUC value, however, there exists little R function to compute PDI value for three category outputs.


}
\value{
The PDI value of the chosen method.
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
Gao Ming, Li jialiang
}
\note{
Pay attention to notice that in MLP, I just only give a typical parameter which, in consequence, may meets error if the data input changes. Hence, feel safe to change these parameters in the function yourself.

It allows to use your own regression by easily setting the method to be "prob". Notice that at this circumstance, the input d should be a probability matrix with dimension N*3 or N*4, which every RowSum should be 1.
}
\seealso{
hum
}
\examples{
rm(list=ls())
str(iris)
data <- iris[, 3]
label <- iris[, 5]
pdi(y = label, d = data,method="tree",n=3)
## [1] 0.9082667
pdi(y = label, d = data, method = "mlp",n=3)
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
pdi(y = label, d = data, "mlp",n=4)
## [1] 0.4790714
}
\keyword{ PDI }
