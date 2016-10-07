\name{allOuts}
\alias{allOuts}
\docType{package}
\title{Produce a data frame that combines all three outlier types}
\description{ Simple function to produce a plots and a final data frame from studentized deleted residuals, hat values (leverage), and Cook's distance, after a model run in lm(). Resulting dataframe will be sorted by the inAll variable (observation is considered an outlier on all three plots) and then the Cooks_D variable. }
\usage{
  allOuts(model))
}
\arguments{
  \item{model}{ model object from an lm() fiitted equation. }
}
\seealso{\code{\link{studResidPlot}} \code{\link{leveragePlot}} \code{\link{cooksPlot}}}
\keyword{ package }
\examples{
  mod <- lm(hp ~ disp, data=mtcars)
  summary(mod)
  allOuts(mod)
}
