\name{simpleScatter}
\alias{simpleScatter}
\docType{package}
\title{Simple Scatter Plots}
\description{A minimal function to create a ggplot object based on two continuous varaibles.}
\usage{
  simpleScatter(data, x, y, xlab=deparse(substitute(x)), ylab=deparse(substitute(y)), title=NULL, subtitle=NULL, xlim=NULL, ylim=NULL, line=FALSE, interval="none", ...)
}
\arguments{
  \item{data}{ dataframe object to be used. }
  \item{x}{ x variable to be plotted. }
  \item{y}{ y variable to be plotted. }
  \item{xlab}{ label (optional) to be placed on the x axis. }
  \item{ylab}{ label (optional) to be placed on the y axis. }
  \item{title}{ title (optional) of the chart. }
  \item{subtitle}{ subtitle (optional) of the chart. }
  \item{xlim}{ typical xlim option to limit graphed regions of the x axis. }
  \item{ylim}{ typical xlim option to limit graphed regions of the y axis. }
  \item{line}{ logical: add linear fit line to plot? }
  \item{interval}{ option denoting the type of interval to be plotted around linear prediction line. Option "none" is default. Other options include "confidence" and "prediction". Use of any option other than "none" will automatically change line=TRUE.}
  \item{...}{ further arguments passed to or from other methods. }
}
\keyword{ package }
\note{simpleScatter produces a ggplot object that may be used in conjunction with ggplot followup commands.}
\examples{
  m <- mtcars
  simpleScatter(m, disp, hp, title="Horsepower from Displacement")
}
