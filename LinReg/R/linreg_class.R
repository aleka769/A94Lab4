#' @title Class formatting for RC type object.  
#' 
#' @description this list will be filled with objects calculated in \code{linreg()}-function
#' 
#' @export

linreg_class <- setRefClass("linreg",
  fields = list(beta_hat      = "matrix",
                fits          = "numeric",
                residuals     = "numeric",
                df            = "numeric",
                call          = "formula"
                )
)

linreg_class$methods(
  "initialize"= function(beta_hat, fits, residuals, df, call){
      .self$beta_hat <- beta_hat
      .self$fits <- fits           
      .self$residuals <- residuals      
      .self$df <- df     
      .self$call <- call
      },
  "print" = function(){
      return("print")
    },
  "coef" = function(){
        beta_hat
      }
    #,
    #,
    # plot <- function(){
    #   return("plot")
    # },
    # resid <- function(){
    #   return(.self$resid)
    # },
    # pred <- function(){
    #   return(.self$y_hat)
    # },
    # coef <- function(){
    #   beta_hat
    # },
    # summary <- function(){}
)
