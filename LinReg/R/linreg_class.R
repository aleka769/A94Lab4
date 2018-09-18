linreg_class <- setRefClass("linreg",
  fields = list(beta_hat      = "numeric",
                #"fits"          = "numeric",
                #"residuals"     = "numeric",
                #"std_residuals" = "numeric",
                #"df"            = "numeric",
                call          = "character"
                )
)
linreg_class$methods(
  "initialize"= function(beta_hat, call){
      .self$beta_hat <- beta_hat
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
