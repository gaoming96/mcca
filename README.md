# mcca
This is a package for Multi-category Diagnostic Accuracy.<br>

## Installation

[![CRAN Version](https://www.r-pkg.org/badges/version/mcca)](https://cran.r-project.org/package=mcca)
![Downloads](https://cranlogs.r-pkg.org/badges/mcca)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/40ua5l06jw0gjyjb?svg=true)](https://ci.appveyor.com/project/gaoming96/mcca)


The package is available on [CRAN](https://cran.r-project.org/package=mcca) and can be installed directly in R using `install.packages()`. You may want to run `install_formats()` after the first installation.

```R
install.packages("mcca")
install_formats()
```

The latest development version on GitHub can be installed using:

```R
if (!require("remotes")){
    install.packages("remotes")
}
remotes::install_github("gaoming/mcca")
```
## Description

It contains six common multi-category classification accuracy evaluation measures:

 Hypervolume Under Manifold (HUM), described in
 Li and Fine (2008) <doi:10.1093/biostatistics/kxm050>.
 
 Correct Classification Percentage (CCP), Integrated Discrimination Improvement (IDI), Net Reclassification Improvement (NRI), R-Squared Value (RSQ), described in
 Li, Jiang and Fine (2013) <doi:10.1093/biostatistics/kxs047>.
 
 Polytomous Discrimination Index (PDI), described in
 Van Calster et al. (2012) <doi:10.1007/s10654-012-9733-3>.
 Li et al. (2018) <doi:10.1177/0962280217692830>.

## Demo

Here is a little demo.<br>

```r
#install.packages("nnet")
#install.packages("rpart")
#install.packages("e1071")
### Basic Setup
#install.packages("MASS")
#install.packages("devtools")
library(devtools)
install_github("gaoming96/mcca")
library(mcca)

### Help File
?hum

### Example 1
rm(list=ls())
str(iris)
data <- iris[, 1:4]
label <- iris[, 5]
hum(y = label, d = data, method = "multinom", k = 3)
##[1] 0.9972
hum(y = label, d = data, method = "tree", k = 3)
##[1] 0.998
hum(y = label, d = data, method = "mlp", k = 3)
##[1] 0.9948
pdi(y = label, d = data, method = "mlp", k = 3)
##[1] 0.9976

### Example 2
rm(list=ls())
str(iris)
data <- data.matrix(iris[, 1:4])
label <- as.numeric(iris[, 5])
# multinomial
require(nnet)
# model
fit <- multinom(label ~ data, maxit = 1000, MaxNWts = 2000)
predict.probs <- predict(fit, type = "probs")
pp<- data.frame(predict.probs)
# extract the probablity assessment vector
head(pp)
##          X1           X2           X3
## 1 1.0000000 7.813676e-11 1.208325e-37
## 2 1.0000000 2.894707e-08 2.068738e-33
## 3 1.0000000 2.961086e-09 3.547252e-35
## 4 0.9999996 3.533713e-07 6.964265e-32
## 5 1.0000000 5.609367e-11 5.690705e-38
## 6 1.0000000 2.489396e-11 8.327501e-37
hum(y = label, d = pp, method = "prob", k = 3)
##[1] 0.9972
# the same result as the first example in Example 1


### Example 3
rm(list=ls())
str(iris)
data <- iris[, 3]
label <- iris[, 5]
pdi(y = label, d = data, method = "tree", k = 3)
##[1] 0.9082667
pdi(y = label, d = data, method = "mlp", k = 3)
##[1] 0.9845333
```
