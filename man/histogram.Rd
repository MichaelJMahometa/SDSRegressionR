\name{histogram}
\alias{histogram}
\docType{package}
\title{ Draw a Histogram.}
\description{ Functional wrapper to hist() in graphics. Adds functionality (table) and standard cut options (right=FALSE) to hist(). }
\usage{
  histogram(x, breaks=NULL, table=FALSE, right=FALSE, xlab=deparse(substitute(x)), title=NULL,...)
}
\arguments{
  \item{x}{ x variable to be plotted. }
  \item{breaks}{ one of:
    \itemize{
    \item a vector giving the breakpoints between histogram cells,
    \item a function to compute the vector of breakpoints,
    \item a single number giving the number of cells for the histogram,
    \item a character string naming an algorithm to compute the number of cells,
    \item a function to compute the number of cells.
    }
  }
  \item{table}{ logical: Should a frequency distribution table be produced? }
  \item{right}{ logical; if FALSE, the histogram cells are left-closed (right open) intervals.}
  \item{xlab}{ label of the x-axis. Default is the object name of x above. }
  \item{title}{ title (optional) of the chart. }
  \item{...}{ further arguments passed to or from other methods. }
}
\seealso{\code{\link{hist}}}
\keyword{ package }
\examples{
  histogram(mtcars$hp)
  histogram(mtcars$hp, breaks=seq(50, 350, by=25))
  histogram(mtcars$hp, breaks=seq(50, 350, by=25), table=TRUE)
}
