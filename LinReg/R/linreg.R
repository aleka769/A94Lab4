linreg <- function(formula, data){
  f <- formula(formula)
  y <- all.vars(f)[1]
  X <- model.matrix(f, data)
  
  
  #return:
  retlist <- list("beta_hat"      = bhat,
                  "fits"          = yhat,
                  "residuals"     = resid,
                  "std_residuals" = stdresid,
                  "df"            = df,
                  "call"          = f)
}


