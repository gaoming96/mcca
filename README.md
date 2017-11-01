# mcca
This is a package for Multi-category Diagnostic Accuracy.<br>
Before using this package, you need to install some R packages.<br>
Here is a little demo.<br>

```r
#install.packages("nnet")
#install.packages("rpart")
#install.packages("e1071")
#install.packages("MASS")
#install.packages("devtools")
library(devtools)
install_github("gaoming96/mcca")
library(mcca)

?hum
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

rm(list=ls())
str(iris)
data <- iris[, 3]
label <- iris[, 5]
pdi(y = label, d = data, method = "tree", k = 3)
##[1] 0.9082667
pdi(y = label, d = data, method = "mlp", k = 3)
##[1] 0.9845333
```
