#' Create a \emph{FULL} dataset based on a model object
#'
#' Simple function to produce a \emph{FULL} dataset (all variables) for the \emph{observations}  from a model run in \code{lm()}. This is useful in running a nested regression.
#'
#' @param obj Model object from an \code{lm()} fitted equation.
#'
#' @examples
#'
#' mod <- lm(hp ~ disp, data=mtcars)
#' modelData(mod)
#'
#'@export
modelData <- function(obj){
  thisdf <- get(paste(eval(obj)$call$data))
  thisdf <- thisdf %>%
    filter(row.names(thisdf) %in% row.names(model.frame(obj)))
  return(thisdf)
}
