% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictValues.R
\name{predictValues}
\alias{predictValues}
\title{Predict Values}
\usage{
predictValues(obj)
}
\arguments{
\item{obj}{Model object from an lm() fitted equation.}
}
\description{
Basic function to find fitted values for items in an original data frame.
}
\examples{
 mod <- lm(hp ~ disp + wt, data=mtcars)
 mtcars$pred <- predictValues(mod)

}
