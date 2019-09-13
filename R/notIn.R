#' Negate the \code{\%in\%} function.
#'
#' Use this for the OPPOSITE of \code{\%in\%}.
#'
#' @export
"%not_in%" <- function(x, y) !x %in% y
