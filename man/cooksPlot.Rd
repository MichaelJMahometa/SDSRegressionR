\name{cooksPlot}
\alias{cooksPlot}
\docType{package}
\title{Produce a Cook's Distance plot.}
\description{ Simple function to produce a plot of the Cook's Distance for each observtion after a model run in lm(). }
\usage{
  cooksPlot(model, ylim=NULL, print=FALSE, id=FALSE)
}
\arguments{
  \item{model}{ model object from an lm() fiitted equation. }
  \item{ylim}{ option to alter the ylim value of the r plot object. }
  \item{id}{ logical: Should the identy() function be used to describe selected observations? Will automatically invoke print=TRUE for selected observations. }
  \item{print}{ logical: Should observations outside the specified sigma level be printed to the console? }
}
\seealso{\code{\link{studResidPlot}} \code{\link{leveragePlot}}}
\keyword{ package }
\examples{
  mod <- lm(hp ~ disp, data=mtcars)
  summary(mod)
  cooksPlot(mod)
}
