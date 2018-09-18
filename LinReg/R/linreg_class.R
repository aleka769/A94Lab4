#' @title Class formatting for RC type object.
#'
#' @description this list will be filled with objects calculated in \code{linreg()}-function
#'
#' @export

linreg_class <- setRefClass("linreg",
  fields = list(beta_hat      = "numeric",
                fits          = "numeric",
                residuals     = "numeric",
                df            = "numeric",
                formula       = "formula",
                call          = "character"
                )
)

linreg_class$methods(
  "initialize"= function(beta_hat, fits, residuals, df, formula, call){
      .self$beta_hat <- beta_hat
      .self$fits <- fits
      .self$residuals <- residuals
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
                              std_residuals = residuals)

      res_vs_fitted <- ggplot2::ggplot(data = plot_data,
                                       ggplot2::aes(x = fits,
                                                    y = residuals)) +
          # theme_liu() +
          ggplot2::labs("title" = "Residuals vs Fitted",
                        "x" = paste("Fitted values\nlinreg(",
                                    deparse(formula),")"),
                        "y" = "Residuals") +
          ggplot2:: geom_point()

      std_vs_fitted <- ggplot2::ggplot(data = plot_data,
                                       ggplot2::aes(x = fits,
                                                    y = std_residuals)) +
          # theme_liu() +
          ggplot2::labs("title" = "Scale-Location",
                        "x" = paste("Fitted values\nlinreg(",
                                    deparse(formula),")"),
                        "y" = expression(sqrt("Standardized residual"))) +
          ggplot2::geom_point()

      gridExtra::grid.arrange(res_vs_fitted, std_vs_fitted, ncol = 2)
      }

    # summary <- function(){}
)
