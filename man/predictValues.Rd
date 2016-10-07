\name{predictValues}
\alias{predictValues}
\docType{package}
\title{Predict estimated values}
\description{ Predict estimated values based on a lm() model and place back into original dataframe. NAs will be produced if the observation was not included in the lm() model. }
\usage{
  predictValues(model, se.fit=FALSE, interval="none", name=NULL)
}
\arguments{
  \item{model}{ model object from an lm() fiitted equation. }
  \item{se.fit}{ logical: Should standard errors be reported? }
  \item{interval}{ option denoting the type of interval to be plotted around linear prediction line. Option "none" is default. Other options include "confidence" and "prediction". }
  \item{name}{ optional name of the model dependent variable. If not specified, will be the formula dependent variable from the model. }
}
\keyword{ package }
\examples{
  mod <- lm(hp ~ disp, data=mtcars)
  summary(mod)
  predictValues(mod)
}
