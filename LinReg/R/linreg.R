#' @title Yep
#'
#' @param formula an object of class "formula", a symbolic description of the model to be fitted.
#'
#' @param data the data frame including variables to include in the model.
#'
#' @return \code{linreg()} returns a linreg-object with common model-methods like coef, resid, plot etc.
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

  betadf  <- data.frame("Variance" = diag(varb),
                        "t_value"  = tbeta,
                        "p_value"  = pbeta)
  class(betadf)
  # Return list, input to RC list ("linreg_class"):
  retlist <- linreg_class$new(beta_hat  = t(bhat)[1,],
                              fits      = yhat,
                              residuals = resid,
                              resid_var = resvar, #
                              beta_info = betadf, #
                              df        = df,
                              formula   = f,
                              call      = deparse(sys.call()))

  # Return
  return(retlist)
}

# Test:
#a<- linreg(formula = Sepal.Length ~ Sepal.Width+Petal.Length, data = iris)
