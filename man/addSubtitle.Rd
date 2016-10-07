\name{addSubtitle}
\alias{addSubtitle}
\docType{package}
\title{Add a Subtitle to a ggplot Object}
\description{A simple function to add a title and subtitle to an existing ggplot object.}
\usage{
  addSubtitle(title, subtitle, ...)
}
\arguments{
  \item{title}{ title to be added to the existing ggplot. }
  \item{subtitle}{ subtitle to be added to the existing ggplot (optional. }
  \item{...}{ further arguments passed to or from other methods. }
}
\keyword{ package }
\note{In order for addSubtitle to be effective, the ggplot graph that you wish to modify must be in the working environment.}
\seealso{\code{\link{simpleScatter}}}
\examples{
  m <- mtcars
  s <- simpleScatter(m, disp)
  s +
    addSubtitle("Horsepower from Displacement", "From mtcars dataset")
}
