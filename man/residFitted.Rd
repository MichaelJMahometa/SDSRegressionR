\name{residFitted}
\alias{residFitted}
\docType{package}
\title{Residual vs. Fitted Plot}
\description{ Create a residual versus fitted values plot. }
\usage{
  residFitted(model, sigma=FALSE, id=FALSE, print=FALSE, ...)
}
\arguments{
  \item{model}{ model object from an lm() fiitted equation. }
  \item{sigmna}{ level of residual Standard Deviation to plot. Default is none (FALSE). }
  \item{id}{ logical: Should the identy() function be used to describe selected observations? Will automatically invoke print=TRUE for selected observations. }
  \item{print}{ logical: Should observations outside the specified sigma level be printed to the console? }
  \item{...}{ further arguments passed to or from other methods. }
}
\keyword{ package }
\examples{
  mod <- lm(hp ~ disp, data=mtcars)
  summary(mod)
  residFitted(mod, sigma=2, print=TRUE)
}
