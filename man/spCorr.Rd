\name{spCorr}
\alias{spCorr}
\docType{package}
\title{Semipartial Correlation Coefficients.}
\description{ DEPRECIATED into pCorr Simple function to produce semipartial correlation coefficients after a model run in lm(). }
\usage{
  spCorr(model)
}
\arguments{
  \item{model}{ model object from an lm() fiitted equation. }
}
\seealso{\code{\link{pCorr}}}
\keyword{ package }
\examples{
  mod <- lm(hp ~ disp + wt, data=mtcars)
  summary(mod)
  spCorr(mod)
}
