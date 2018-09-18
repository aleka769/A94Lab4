#' @title Yep
#' 
#' @param formula an object of class "formula", a symbolic description of the model to be fitted.
#' 
#' @param data the data frame including variables to include in the model.
#' 
#' @return \code{linreg} does calculations for RC list specified outside of function and returns values for the list's \code{fields}
#' 
#' @export


linreg <- function(formula, data){
  
  # Set-up of vars, matrices and formula:
  f       <- formula(formula)
  yname   <- all.vars(f)[1]
  X       <- model.matrix(f, data)
  y       <- data[,yname]
  
  
  # Matrix algebra:
  bhat    <- solve(t(X) %*% X) %*% t(X) %*% y
  yhat    <- as.vector(X %*% bhat)
  resid   <- y - yhat
  df      <- nrow(X) - (ncol(X) - 1)
  resvar  <- as.vector((t(resid) %*% resid) / df)
  varb    <- resvar * solve(t(X) %*% X)
  tbeta   <- bhat / sqrt(diag(varb))
  pbeta   <- 1 - pt(q = tbeta, df = df)
  
  
  # Return list, input to RC list ("linreg_class"):
  retlist <- linreg_class$new(beta_hat  = bhat,
                              fits      = yhat,
                              residuals = resid,
                              df        = df,
                              call      = f)
  
  # Return
  return(retlist)
}

# Test:
# linreg(formula = Sepal.Length ~ Sepal.Width, data = iris)
