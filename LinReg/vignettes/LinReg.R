## ---- echo = FALSE, message = FALSE, warning=FALSE-----------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(ggplot2)
library(gridExtra)
library(LinReg)
set.seed(1014)

## ---- echo = TRUE, message = FALSE---------------------------------------
mod <- linreg(formula = Sepal.Width ~ Sepal.Length, data = iris)

## ---- echo = TRUE, message = FALSE, warning=FALSE------------------------
print(mod)

## ---- echo = TRUE, message = FALSE---------------------------------------
plot(mod)

