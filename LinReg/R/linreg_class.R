#' @title RC type object to represent linreg-data.
#'
#' @description linreg object holds data and methods for data calculated in \code{linreg()}-function.
#'
#' @field beta_hat Estimates for each \beta
#' @field fits Contains fitted values
#' @field residuals Contains residuals, i.e. observed - fitted
#' @field resid_var Residual variance
#' @field beta_info Contains variance, t-value and p-value for each estimated \beta value
#' @field df Degrees of freedom for the model
#' @field formula Formula used in model
#' @field call Call sent to \code{linreg()}
#'
#' @describeIn print Prints the cal and coefficient estimates
#' @describeIn summary Prints a summary of the model including esimates and their variances, t-values and p-values as well as the degrees of freedom and residual variance
#' @describeIn plot Produces plots of residuals and standardized residuals vs fitted values
#' @describeIn coef Returns \beta estimates
#' @describeIn resid Returns residual vector from the model
#' @describeIn pred Retrns fitted values from the model
#'
#' @export

linreg_class <- setRefClass("linreg",
  fields = list(beta_hat      = "snumeric",
                fits          = "numeric",
                residuals     = "numeric",
                resid_var     = "numeric",
                beta_info     = "data.frame",
                df            = "numeric",
                formula       = "formula",
                call          = "character"
                )
)

linreg_class$methods(
  "initialize"= function(beta_hat, fits, residuals, resid_var, beta_info, df, formula, call){
      .self$beta_hat <- beta_hat
      .self$fits <- fits
      .self$residuals <- residuals
      .self$resid_var <- resid_var
      .self$beta_info <- beta_info
      .self$df <- df
      .self$formula <- formula
      .self$call <- call
      },

  "print" = function(){
      cat("Call:\n",
          deparse(call),
          "\n\n",
          "Coefficients:\n",
          sep = ""
      )
      format(signif(beta_hat,3))
      },

  "coef" = function(){
      beta_hat
      },

  "resid" = function(){
      residuals
      },

  "pred" = function(){
      fits
      },

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

  "summary" = function(){
      beta_data <- cbind("Estimate"=beta_hat,
                         "Std.Err" = sqrt(beta_info$Variance),
                         "t_value" = beta_info$t_value,
                         "p_value" = beta_info$p_value
                         )
      # Removed since df's/vectors are only printed of they are called last...
      # resid_data <- c(quantile(residuals, probs = c(0,0.25,0.5,0.75,1)))
      # names(resid_data) <- c("Min","Q1","Median","Q3","Max")

      cat("Call:\n",
          deparse(call),
          "\n\nVariance:",
          signif(resid_var,3),
          ",   Degrees of freedom: ",
          df,
          "\n\nCoefficients:\n",
          sep = ""
      )

      format(round(beta_data,2))
  }
)
