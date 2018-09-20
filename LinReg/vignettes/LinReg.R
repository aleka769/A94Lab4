## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(ggplot2)
set.seed(1014)

## ---- echo = TRUE, message = FALSE---------------------------------------
linreg(formula = Sepal.Width ~ Sepal.Length, data = iris)

