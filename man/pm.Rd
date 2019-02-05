\name{pm}
\alias{pm}
\title{Calculate Probability Matrix}
\usage{
pm(y, d, method="multinom", k=3, \dots)
}
\description{
compute the probability matrix of two or three or four categories classifiers with an option to define the specific model or user-defined model.
}
\arguments{
  \item{y}{The multinomial response vector with two, three or four categories. It can be factor or integer-valued.}
  \item{d}{The set of candidate markers, including one or more columns. Can be a data frame or a matrix.}
  \item{method}{Specifies what method is used to construct the classifier based on the marker set in d. Available option includes the following methods:"multinom": Multinomial Logistic Regression which is the default method, requiring R package nnet;"tree": Classification Tree method, requiring R package rpart;"svm": Support Vector Machine (C-classification and radial basis as default), requiring R package e1071;"lda": Linear Discriminant Analysis, requiring R package lda.}
  \item{k}{Number of the categories, can be 2 or 3 or 4.}
  \item{\dots}{Additional arguments in the chosen method's function.}
}
\details{
The function returns the probability matrix for predictive markers based on a user-chosen machine learning method. Currently available methods include logistic regression (default), tree, lda, svm and user-computed risk values.

}
\value{
The probability matrix of the classification using a particular learning method on a set of marker(s).
}
\references{
Li, J. and Fine, J. P. (2008): ROC analysis with multiple tests and multiple classes: methodology and applications in microarray studies. Biostatistics. 9 (3): 566-576.

Li, J., Chow, Y., Wong, W.K., and Wong, T.Y. (2014). Sorting Multiple Classes in Multi-dimensional ROC Analysis: Parametric and Nonparametric Approaches. Biomarkers. 19(1): 1-8.
}
\author{
Ming Gao: gaoming96@sjtu.edu.cn

Jialiang Li: stalj@nus.edu.sg
}

\seealso{
\code{\link{pdi}}
}
\examples{
rm(list=ls())
str(iris)
data <- iris[, 1:4]
label <- iris[, 5]
pm(y = label, d = data,method = "multinom", k = 3)

}
\keyword{ PM }
