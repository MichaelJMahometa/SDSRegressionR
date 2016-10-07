\name{lmBeta}
\alias{lmBeta}
\docType{package}
\title{Standardized Betas.}
\description{ Simple function to produce standardized beta estimates after a model run in lm(). }
\usage{
  lmBeta(model)
}
\arguments{
  \item{model}{ model object from an lm() fiitted equation. }
}
\keyword{ package }
\examples{
  mod <- lm(hp ~ disp + wt, data=mtcars)
  summary(mod)
  pCorr(mod)
}
