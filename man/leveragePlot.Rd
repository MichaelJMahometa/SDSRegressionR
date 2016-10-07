\name{leveragePlot}
\alias{leveragePlot}
\docType{package}
\title{Produce a leverage (Hat Value) plot.}
\description{ Simple function to produce a plot of the hat values (leverage) after a model run in lm(). }
\usage{
  leveragePlot(model, ylim=NULL, print=FALSE, id=FALSE)
}
\arguments{
  \item{model}{ model object from an lm() fiitted equation. }
  \item{ylim}{ option to alter the ylim value of the r plot object. }
  \item{id}{ logical: Should the identy() function be used to describe selected observations? Will automatically invoke print=TRUE for selected observations. }
  \item{print}{ logical: Should observations outside the specified sigma level be printed to the console? }
}
\seealso{\code{\link{studResidPlot}} \code{\link{cooksPlot}}}
\keyword{ package }
\examples{
  mod <- lm(hp ~ disp, data=mtcars)
  summary(mod)
  leveragePlot(mod)
}
