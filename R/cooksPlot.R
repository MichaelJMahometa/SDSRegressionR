#' Create a Cook's Distance Plot
#'
#' Simple function to produce a plot of the Cook's Distance for each observation after a model run in lm().
#'
#' @inheritParams leveragePlot
#' @inheritParams studResidPlot
#' @param save.cutoff Logical: Should the Cook's Distance cutoff be saved to the Global Environment?
#'
#' @seealso
#' \code{\link{studResidPlot}}
#' \code{\link{leveragePlot}}
#' \code{\link{threeOuts}}
#'
#' @examples
#'
#' mod <- lm(hp ~ disp, data=mtcars)
#' summary(mod)
#' cooksPlot(mod)
#'
#'@export
cooksPlot <- function(obj, ylim=NULL, id=FALSE, print.obs=FALSE, print.plot=TRUE, save.cutoff=FALSE){
  thisdf <- get(paste(eval(obj)$call$data))
  cutoff <- 4/(obj$df.residual)
  if(print.plot == TRUE){
  plot(cooks.distance(obj), ylim=ylim, pch=16, main="Cook's Distance", ylab="Cook's Distance")
  #mtext(paste("Cutoff = ", round(cutoff, 6), sep=""), side=3)
  abline(h=cutoff, lty=2, col="red")
  }
  if(print.obs & !id){
    i <- names(cooks.distance(obj))[cooks.distance(obj) > cutoff]
    n <- names(obj$model)
    rep_df <- data.frame(thisdf[names(cooks.distance(obj)[i]),n], obj$fitted.values[names(cooks.distance(obj)[i])], cooks.distance(obj)[names(cooks.distance(obj)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Cooks_D")
    return(rep_df)
  }
  else if(id){
    i <- identify(cooks.distance(obj), labels=names(cooks.distance(obj)))
    n <- names(obj$model)
    rep_df <- data.frame(thisdf[names(cooks.distance(obj)[i]),n], obj$fitted.values[names(cooks.distance(obj)[i])], cooks.distance(obj)[names(cooks.distance(obj)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Cooks_D")
    return(rep_df)
  }
  if(save.cutoff){
  assign("cooksCutOff", cutoff, envir = .GlobalEnv)
  }
}
