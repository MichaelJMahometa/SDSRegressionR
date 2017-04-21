#' Create Dummy Codes
#'
#' Simple function to create Dummy Codes from a single variable. All "k" Dummy Variables will be created.
#'
#' @param x Vector to be Dummy Coded.
#'
#' @seealso
#' \code{\link{makeEffects}}
#'
#' @examples
#'
#' makeDummies(mtcars$cyl)
#' mtcars <- data.frame(mtcars, makeDummies(mtcars$cyl))
#'
#'@export
makeDummies <- function(x){
  t <- table(x)
  lt <- length(t)
  n.obs <- length(x)
  new <- matrix(0, nrow = n.obs, ncol = lt)
  xlev <- as.factor(x)
  for (i in 1:n.obs) {
    new[i, xlev[i]] <- 1
  }
  new[which(is.na(xlev)), ] <- NA #Added
  colnames(new) <- names(t)
  return(new)
}
