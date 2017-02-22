#' Insert break in chunk.
#'
#' Call this function as an addin to insert an *R code chunk break* at the cursor position.
#'
#' @export
insertChunkBreak <- function() {
  rstudioapi::insertText("``` \n
```{r}")
}
