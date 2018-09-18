linreg <- function(formula, data){
  # Set-up of matrices and formula:
  f      <- formula(formula)
  yname  <- all.vars(f)[1]
  X      <- model.matrix(f, data)
  y      <- data[,yname]
  
  # Matrix algebra:
  bhat   <- solve(t(X) %*% X) %*% t(X) %*% y
  yhat   <- X %*% bhat
  resid  <- y - yhat
  df     <- nrow(X) - (ncol(X) - 1)
  resvar <- as.vector((t(resid) %*% resid) / df)
  varb   <- resvar * solve(t(X) %*% X)
  tbeta  <- bhat / sqrt(diag(varb))
  pbeta <- 1 - pt(q = tbeta, df = df)
  
  # Return list, class RC:
  retlist <- setRefClass("linreg",
                         fields = list("beta_hat"  = bhat,
                                       "fits"      = yhat,
                                       "residuals" = resid,
                                       "df"        = df,
                                       "call"      = f,
                                       "p-values"  = pbeta))
  return(retlist)
}

linreg(formula = Petal.Length ~ Petal.Width, data = iris)
