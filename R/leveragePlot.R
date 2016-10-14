#' leveragePlot
#'
#' Simple function to produce a plot of the hat values (leverage) after a model run in lm().
#'
#' @inheritParams studResidPlot
#' @param ylim Optional ylim for the plot.
#'
#' @seealso
#' \code{\link{studResidPlot}}
#' \code{\link{cooksPlot}}
#' \code{\link{threeOuts}}
#'
#' @examples
#' mod <- lm(hp ~ disp, data=mtcars)
#' summary(mod)
#' leveragePlot(mod)
#'
#' @export
leveragePlot <- function(obj, ylim=NULL, id=FALSE, print.obs=FALSE, print.plot=TRUE){
  thisdf <- get(paste(eval(obj)$call$data))
  K <- length(coef(obj))
  N <- length(hatvalues(obj))
  hat.avg <- K/N
  if (print.plot == TRUE){
  plot(hatvalues(obj), ylim=ylim, pch=16, main="Leverage", ylab="Leverage (Hat Values)")
  abline(h=c(2,3)*hat.avg, lty=2, col=c("orange", "red"))
  }
  if(print.obs & !id) {
    i <- names(hatvalues(obj))[hatvalues(obj) > hat.avg*2]
    n <- names(obj$model)
    rep_df <- data.frame(thisdf[names(hatvalues(obj)[i]),n], obj$fitted.values[names(hatvalues(obj)[i])], hatvalues(obj)[names(hatvalues(obj)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Hat_Values")
    return(rep_df)
  }
  else if(id){
    i <- identify(hatvalues(obj), labels=names(hatvalues(obj)))
    n <- names(obj$model)
    rep_df <- data.frame(thisdf[names(hatvalues(obj)[i]),n], obj$fitted.values[names(hatvalues(obj)[i])], hatvalues(obj)[names(hatvalues(obj)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Hat_Values")
    return(rep_df)
  }
}
