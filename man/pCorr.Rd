\name{pCorr}
\alias{pCorr}
\docType{package}
\title{Partial Correlation Coefficients.}
\description{ Simple function to produce partial and part (semipartial) correlation coefficients after a model run in lm(). Also returns the "squares" of each metric.}
\usage{
  pCorr(model)
}
\arguments{
  \item{model}{ model object from an lm() fiitted equation. }
}
\seealso{\code{\link{spCorr}}}
\keyword{ package }
\examples{
  mod <- lm(hp ~ disp + wt, data=mtcars)
  summary(mod)
  pCorr(mod)
}
