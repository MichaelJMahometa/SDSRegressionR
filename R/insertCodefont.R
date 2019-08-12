#' Insert break in chunk.
#'
#' Call this function as an addin to insert an *R code chunk break* at the cursor position.
#'
#' @keywords internal
insertCodefont <- function() {
  rstudioapi::insertText("{.codefont}")
}
