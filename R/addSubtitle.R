#' addSubtitle
#'
#' A simple function to add a title and subtitle to an existing ggplot object.
#'
#' @param title Title to be added to the existing ggplot object.
#' @param subtitle Subtitle to be added to existing ggplot object.
#' ... Further agruements passed to or from other methods.
#' @seealso \code{\link{simpleScatter}}
#' @examples
#' m <- mtcars
#' s <- simpleScatter(m, disp)
#' s + addSubtitle("Horsepower from Displacement", "From mtcars dataset")
#' @export
addSubtitle <- function(title, subtitle){
  ggtitle(bquote(atop(.(title), atop(italic(.(subtitle)), ""))))
}
