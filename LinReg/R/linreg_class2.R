#'  RC type object to represent linreg-data.
#'
#' @description linreg object holds data and methods for data calculated in \code{linreg()}-function.
#' 
#'
#' @field beta_hat Estimates for each beta
#' @field fits Contains fitted values
#' @field residuals Contains residuals, i.e. observed - fitted
#' @field resid_var Residual variance
#' @field beta_info Contains variance, t-value and p-value for each estimated beta value
#' @field df Degrees of freedom for the model
#' @field formula Formula used in model
#' @field call Call sent to \code{linreg()}
#'
#'
#' @export linreg

linreg <- setRefClass("linreg",
                      fields = list(beta_hat      = "numeric",
                                    fits          = "numeric",
                                    residuals     = "numeric",
                                    resid_var     = "numeric",
                                    beta_info     = "data.frame",
                                    df            = "numeric",
                                    formula       = "formula",
                                    call          = "character"
                      )
)

linreg$methods(
  "initialize"= function(formula, data){
    
    # Set-up of vars, matrices and formula:
    f       <- formula(formula)
    yname   <- all.vars(f)[1]
    X       <- model.matrix(f, data)
    y       <- data[,yname]
    
    
    # Matrix algebra:
    bhat    <- solve(t(X) %*% X) %*% t(X) %*% y
    yhat    <- as.vector(X %*% bhat)
    resid   <- y - yhat
    df_temp <- nrow(X) - ncol(X)
    resvar  <- as.vector((t(resid) %*% resid) / df_temp)
    varb    <- resvar * solve(t(X) %*% X)
    tbeta   <- bhat / sqrt(diag(varb))
    pbeta   <- 1 - pt(q = abs(tbeta), df = df_temp)
    
    betadf  <- data.frame("Variance" = diag(varb),
                          "t_value"  = tbeta,
                          "p_value"  = pbeta)
    
    .self$beta_hat <- t(bhat)[1,]
    .self$fits <- yhat
    .self$residuals <- resid
    .self$resid_var <- resvar
    .self$beta_info <- betadf
    .self$df <- df_temp
    .self$formula <- f
    .self$call <- deparse(sys.call(which = 1)) # get call from parent
  },
  
  #' @describeIn print Prints the cal and coefficient estimates
  "print" = function(){
    
    cat(noquote(call), "\n")
    cat(capture.output(beta_hat)[1], "\n", sep = " ")
    cat(capture.output(beta_hat)[2], "\n", sep = " ")
  },
  
  #' @describeIn coef Returns beta estimates
  "coef" = function(){
    beta_hat
  },
  
  #' @describeIn resid Returns residual vector from the model
  "resid" = function(){
    residuals
  },
  
  #' @describeIn pred Retrns fitted values from the model
  "pred" = function(){
    fits
  },
  
  #' @describeIn plot Produces plots of residuals and standardized residuals vs fitted values
  "plot" = function(){
    plot_data <- data.frame(residuals,
                            fits,
                            std_residuals = sqrt(abs(residuals/sqrt(resid_var))))
    
    res_vs_fitted <- ggplot2::ggplot(data = plot_data,
                                     ggplot2::aes(x = fits,
                                                  y = residuals)) +
      ggplot2::theme_bw() +
      ggplot2::labs("title" = "Residuals vs Fitted",
                    "x" = paste("Fitted values\nlinreg(",
                                deparse(formula),")"),
                    "y" = "Residuals") +
      ggplot2:: geom_point()
    
    std_vs_fitted <- ggplot2::ggplot(data = plot_data,
                                     ggplot2::aes(x = fits,
                                                  y = std_residuals)) +
      ggplot2::theme_bw() +
      ggplot2::labs("title" = "Scale-Location",
                    "x" = paste("Fitted values\nlinreg(",
                                deparse(formula),")"),
                    "y" = expression(sqrt("|Standardized residual|"))) +
      ggplot2::geom_point()
    
    gridExtra::grid.arrange(res_vs_fitted, std_vs_fitted, nrow = 2)
  },
  
  #' @describeIn summary Prints a summary of the model including esimates and their variances, t-values and p-values as well as the degrees of freedom and residual variance
  "summary" = function(){
    beta_data <- cbind("Estimate"= round(beta_hat,3),
                       "Std.Err" = round(sqrt(beta_info$Variance),3),
                       "t_value" = round(beta_info$t_value,3),
                       "p_value" = round(beta_info$p_value,3)
    )
    
    for (i in 1:nrow(beta_data)){
      # first captured output is colnames which we ignore
      cat(capture.output(beta_data)[i+1], sep = "\n")
    }
    
    cat("Residual standard error: ",
        signif(sqrt(resid_var),3),
        " on ",
        df,
        " degrees of freedom ", sep = ""
    )
  }
  
)
