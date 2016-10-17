#' Create a Residual vs. Fitted Plot
#'
#' Create a residual versus fitted values plot using base plot.
#'
#' @param obj Model object form an lm() fitted equation.
#' @param sigma Level of residual Standard Deviation to plot. Default is none (FALSE).
#' @param id Logical: Should the identy() function be used to describe selected observations? Will automatically invoke print=TRUE for selected observations. Default is FALSE.
#' @param print Logical: Should observations outside the specified sigma level be printed to the console? Default is FALSE.
#' @param ... Further arguments passed to and from other methods.
#' @examples
#' mod <- lm(hp ~ disp, data=mtcars)
#' summary(mod)
#' residFitted(mod, sigma=2, print=TRUE)
#' @export
#'
residFitted <- function(obj, sigma=FALSE, id=FALSE, print=FALSE, ...){
  thisdf <- get(paste(eval(obj)$call$data))
  plot(fitted.values(obj), residuals(obj), pch=16, ylab="Residuals", xlab="Fitted Values", main="Residuals vs. Fitted",...)
  abline(h=0, lty=2, col="green")
  if (!sigma & print){
    warning("Sigma not initiated")
  }
  if (sigma & !id & !print) {
    abline(h=c(-summary(obj)$sigma*2, summary(obj)$sigma*2), col="red", lty=2)
  }
  else if (sigma & !id & print){
    abline(h=c(-summary(obj)$sigma*2, summary(obj)$sigma*2), col="red", lty=2)
    i <- names(rstudent(obj))[abs(residuals(obj)) > summary(obj)$sigma*2]
    n <- names(obj$model)
    rep_df <- data.frame(thisdf[names(fitted.values(obj)[i]),n], obj$fitted.values[names(fitted.values(obj)[i])], obj$residuals[names(fitted.values(obj)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Residual_Value")
    return(rep_df)
  }
  else if (sigma & id) {
    abline(h=c(-summary(obj)$sigma*2, summary(obj)$sigma*2), col="red", lty=2)
    i <- identify(fitted.values(obj), residuals(obj), labels=names(fitted.values(obj)))
    n <- names(obj$model)
    rep_df <- data.frame(thisdf[names(fitted.values(obj)[i]),n], obj$fitted.values[names(fitted.values(obj)[i])], obj$residuals[names(fitted.values(obj)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Residual_Value")
    return(rep_df)
  }
  else if(id){
    i <- identify(fitted.values(obj), residuals(obj), labels=names(fitted.values(obj)))
    n <- names(obj$model)
    rep_df <- data.frame(thisdf[names(fitted.values(obj)[i]),n], obj$fitted.values[names(fitted.values(obj)[i])], obj$residuals[names(fitted.values(obj)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Residual_Value")
    return(rep_df)
  }
}
