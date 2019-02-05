\name{mcca-package}
\alias{mcca-package}
\alias{mcca}
\docType{package}
\title{
Diagnostic accuracy methods for classifiers
}
\description{
  Six common multi-category classification accuracy evaluation measures are included i.e., Correct Classification Percentage (CCP), Hypervolume Under Manifold (HUM), Integrated Discrimination Improvement (IDI), Net Reclassification Improvement (NRI), Polytomous Discrimination Index (PDI) and R-squared (RSQ). It allows users to fit many popular classification procedures, such
as multinomial logistic regression, support vector machine, classification tree, and user computed
risk values.

}
\details{
\tabular{ll}{
Package: \tab mcca\cr
Type: \tab Package\cr
Version: \tab 0.5\cr
Date: \tab 2019-02-04\cr
License: \tab GPL\cr
}

}
\section{Functions}{
  \tabular{ll}{
    \code{\link{ccp}} \tab Calculate CCP Value \cr
    \code{\link{hum}} \tab Calculate HUM Value \cr
    \code{\link{idi}} \tab Calculate IDI Value \cr
    \code{\link{nri}} \tab Calculate NRI Value \cr
    \code{\link{pdi}} \tab Calculate PDI Value \cr
    \code{\link{rsq}} \tab Calculate RSQ Value \cr
    \code{\link{pm}} \tab Calculate Probability Matrix \cr
    \code{\link{ests}} \tab Estimated Information for Single Model Evaluation Value \cr
    \code{\link{estp}} \tab Estimated Information for Paired Model Evaluation Value \cr

  }
}

\section{Installing and using}{
  To install this package, make sure you are connected to the internet and issue the following command in the R prompt:

  \preformatted{
    install.packages("mcca")
  }

  To load the package in R:

  \preformatted{
    library(mcca)
  }
}

\section{Citation}{
Li J, Gao M, D'Agostino R. Evaluating classification accuracy for modern learning
approaches. Statistics in Medicine. 2019;1-27. https://doi.org/10.1002/sim.8103
}
\author{
Ming Gao, Jialiang Li

Maintainer: Ming Gao <gaoming@umich.edu>
}
\references{
Li, Ming G., D'Agostino. (2019). Evaluating Classification Accuracy for Modern Learning Approaches. Statistics in Medicine.

Li, J. and Fine, J. P. (2008): ROC analysis with multiple tests and multiple classes: methodology and applications in microarray studies. Biostatistics. 9 (3): 566-576.

Li, J., Chow, Y., Wong, W.K., and Wong, T.Y. (2014). Sorting Multiple Classes in Multi-dimensional ROC Analysis: Parametric and Nonparametric Approaches. Biomarkers. 19(1): 1-8.

Li, J., Jiang, B. and Fine, J. P. (2013). Multicategory reclassification statistics for assessing Improvements in diagnostic accuracy. Biostatistics. 14(2): 382-394.

Li, J., Jiang, B., and Fine, J. P. (2013). Letter to Editor: Response. Biostatistics. 14(4): 809-810.

Van Calster B, Vergouwe Y, Looman CWN, Van Belle
V, Timmerman D and Steyerberg EW. Assessing the
discriminative ability of risk models for more than two outcome
categories. European Journal of Epidemiology 2012; 27: 761 C
770.

Li, J., Feng, Q., Fine, J.P., Pencina, M.J., Van Calster, B. (2017). Nonparametric estimation and inference for polytomous discrimination index. Statistical Methods in Medical Research. In Press.

}

\keyword{CCP}
\keyword{HUM}
\keyword{IDI}


\seealso{
CRAN packages \pkg{HUM} for HUM.

CRAN packages \pkg{nnet}, \pkg{rpart}, \pkg{e1071},  \pkg{MASS} employed in this package.
}
\examples{

rm(list=ls())
str(iris)
data <- iris[, 1:4]
label <- iris[, 5]
ccp(y = label, d = data, method = "multinom", k = 3,maxit = 1000,MaxNWts = 2000,trace=FALSE)
## [1] 0.9866667
ccp(y = label, d = data, method = "multinom", k = 3)
## [1] 0.9866667
ccp(y = label, d = data, method = "svm", k = 3)
## [1] 0.9733333
ccp(y = label, d = data, method = "svm", k = 3,kernel="sigmoid",cost=4,scale=TRUE,coef0=0.5)
## [1] 0.8333333
ccp(y = label, d = data, method = "tree", k = 3)
## [1] 0.96
p = as.numeric(label)
ccp(y = label, d = p, method = "label", k = 3)
## [1] 1
hum(y = label, d = data,method = "multinom", k = 3)
## [1] 0.9972
hum(y = label, d = data,method = "svm", k = 3)
## [1] 0.9964
hum(y = label, d = data,method = "svm", k = 3,kernel="linear",cost=4,scale=TRUE)
## [1] 0.9972
hum(y = label, d = data, method = "tree", k = 3)
## [1] 0.998
\donttest{ests(y = label, d = data,acc="hum",level=0.95,method = "multinom", k = 3,trace=FALSE)}

## $value
## [1] 0.9972

## $sd
## [1] 0.002051529

## $interval
## [1] 0.9935662 1.0000000
}

