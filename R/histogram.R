#' Create a Histogram
#'
#' Functional wrapper to hist() in graphics. Adds functionality (frequency table) and standard cut options (right=FALSE) to hist().
#'
#' @param x Variable to be plotted.
#' @param breaks one of:
#' \itemize{
#'   \item a vector giving the breakpoints between histogram cells,
#'   \item a function to compute the vector of breakpoints,
#'   \item a single number giving the number of cells for the histogram,
#'   \item a character string naming an algorithm to compute the number of cells,
#'   \item a function to compute the number of cells.
#'   }
#' @param table Logical: Should a frequency distribution table be produced?
#' @param right Logical; if FALSE, the histogram cells are left-closed (right open) intervals.
#' @param xlab Label of the x-axis. Default is the object name of x above.
#' @param title Title (optional) of the chart.
#' @param ... Further arguments passed to or from other methods.
#'
#' @examples
#' histogram(mtcars$hp)
#' histogram(mtcars$hp, breaks=seq(50, 350, by=25))
#' histogram(mtcars$hp, breaks=seq(50, 350, by=25), table=TRUE)
#'
#' @export
histogram <- function(x, breaks=NULL, table=FALSE, right=FALSE, xlab=deparse(substitute(x)), title=NULL,...){
  if(is.null(title)){
    lab <- deparse(substitute(x))
    lab2 <- paste("Histogram of ", lab, sep="")
  } else {
    lab2 <- title
  }
  if(is.null(breaks)){
    h <- hist(x, right=right, xaxt="n", xlab=xlab, main=lab2,...)
    axis(1, at=h$breaks, labels=h$breaks)
    if(table == TRUE){
      span.cut <- cut(x, h$breaks, right=right)
      span.freq <- table(span.cut)
      t <- cbind(span.freq)
      return(t)
    }
    return(invisible(h$breaks))
  } else {
    h2 <- hist(x, breaks=breaks, right=right, xaxt="n", xlab=xlab, main=lab2, ...)
    axis(1, at=h2$breaks, labels=h2$breaks)
    if(table == TRUE){
      span.cut <- cut(x, h2$breaks, right=right)
      span.freq <- table(span.cut)
      t <- cbind(span.freq)
      return(t)
    }
    return(invisible(h2$breaks))
  }
}
