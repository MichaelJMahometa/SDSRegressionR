#' Create a \emph{FULL} dataset based on a model object
#'
#' Simple function to produce a \emph{FULL} dataset (all variables) for the \emph{observations}  from a model run in \code{lm()}. This is useful in running a nested regression.
#'
#' @param obj Model object from an \code{lm()} fitted equation.
#' @param all.vars Logical. Should the new dataset contain \strong{all} the variables from the original, or just a the selection used in the model? Defaults to TRUE.
#' @param key.variable Name of the unique key variable (identifier variable). Only used if \code{all.vars=FALSE}.
#'
#' @examples
#'
#' mod <- lm(hp ~ disp, data=mtcars)
#' modelData(mod)
#'
#'@export
modelData <- function(obj, all.vars=TRUE, key.variable=NULL){
  thisdf <- get(paste(eval(obj)$call$data))
  thisdf <- thisdf %>%
    filter(row.names(thisdf) %in% row.names(model.frame(obj)))
  if(all.vars==FALSE){
    if(is.null(key.variable)){
      stop("Must supply unique key.variable")
    }
    n <- names(obj$model)
    thisdf <- thisdf %>%
      select(key.variable, one_of(n))
  }
  return(thisdf)
}
