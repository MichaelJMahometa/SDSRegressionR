\name{lmDecomp}
\alias{lmDecomp}
\docType{package}
\title{Decomposition of interaction in a linear model}
\description{ Function to help in the decomposition of interactions within a linear model (lm()). Techniques follow the suggestions of Bauer and Curran (2015), including simple slopes and regions of significance.}
\usage{
  lmDecomp(obj, interest, moderator, mod.type=1, alpha.level=0.05,
  show.points=FALSE, ros=TRUE)
}
\arguments{
  \item{obj}{ model object from an lm() fiitted equation. }
  \item{interest}{ variable of interest as called with the lm() object. }
  \item{moderator}{ moderator variable (continuous or dichotomous) as called with the lm() object. }
  \item{mod.type}{ type of moderation. 1 == continuous by continuous; 2 == continuous by dichotomous. }
  \item{mod.levels}{ levels or values of moderator at which calculate simple slopes. }
  \item{mod.range}{ optional. Alters the range of the moderator variable in the Region of Significance plot. Defaults to the range of the moderator in the original lm()  model object data.}
  \item{int.range}{ optional. Alters the range of the variable of interest in the Region of Significance plot. Defaults to the range of the variable in the original lm()  model object data.}
  \item{alpha.level}{ alpha level for the simple slope and region of significance. Defualts to 0.05. }
  \item{show.points}{ logical. Should the simple slopes graph contain the original data points? Defualts to FALSE. }
  \item{ros}{ logical. Should the Region of Significance be computed and displayed? Defaults to TRUE.}
}
\keyword{ package }
\examples{
  mod <- lm(mpg ~ wt * am, data=mtcars)
  summary(mod)
  lmDecomp(mod, "wt", "am", mod.type = 2, mod.levels=c(0,1))
}
