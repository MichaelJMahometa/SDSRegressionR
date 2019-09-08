#' Negate the \code{\%in\%} function.
#'
#' Use this for the OPPOSITE of \code{\%in\%}.
#'
`%not in%` <- function(x, y) !x %in% y
