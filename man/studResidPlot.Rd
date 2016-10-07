\name{studResidPlot}
\alias{studResidPlot}
\docType{package}
\title{Produce a studentized deleted residual plot.}
\description{ Simple function to produce a plot of the studentized deleted residual after a model run in lm(). }
\usage{
  studResidPlot(model, id=FALSE, print=FALSE)
}
\arguments{
  \item{model}{ model object from an lm() fiitted equation. }
  \item{id}{ logical: Should the identy() function be used to describe selected observations? Will automatically invoke print=TRUE for selected observations. }
  \item{print}{ logical: Should observations outside the specified sigma level be printed to the console? }
}
\seealso{\code{\link{leveragePlot}} \code{\link{cooksPlot}}}
\keyword{ package }
\examples{
  mod <- lm(hp ~ disp, data=mtcars)
  summary(mod)
  studResidPlot(mod)
}
