#' Predict Values
#'
#' Basic function to find fitted values for items in an original data frame.
#'
#' @param obj Model object from an lm() fitted equation.
#'
#' @examples
#'  mod <- lm(hp ~ disp + wt, data=mtcars)
#'  mtcars$pred <- predictValues(mod)
#'
#' @export
predictValues <- function(obj){
  orig_data_rows <- row.names(get(paste(obj$call$data)))
  vect <- rep(NA, length(orig_data_rows))
  names(vect) <- orig_data_rows
  fit_names <- names(fitted(obj))
  vect[which(orig_data_rows %in% fit_names)] <- fitted(obj)
  return(vect)
}

