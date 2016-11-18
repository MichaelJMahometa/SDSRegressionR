#' Create a Standardized Residual Plot
#'
#' Produce a Standardized Residual plot. NOT recommended. Use Studentized Deleted instead
#'
#' @param obj Object from an lm() fiitted equation.
#' @param id Logical: Should the identy() function be used to describe selected observations? Will automatically invoke print=TRUE for selected observations.
#' @param print.obs Logical: Should observations outside the specified sigma level be printed to the console?
#' @param print.plot Logical: Should plot be created?
#'
#' @seealso
#' \code{\link{studResidPlot}}
#'
#' @examples
#' mod <- lm(hp ~ disp, data=mtcars)
#' summary(mod)
#' standResidPlot(mod)
#'
#' @keywords internal
#'
#' @export
standResidPlot <- function(obj, id=FALSE, print.obs=FALSE, print.plot=TRUE){
  #thisdf <- get(paste(eval(obj)$call$data))
  thisdf <- obj$model
  mx <- max(abs(rstandard(obj)))
  if (print.plot == TRUE){
    plot(rstandard(obj), pch=16, ylab="Standardized Residuals", main="Standardized Residuals", ylim=c(min(c(-mx, -2)), max(c(mx, 2)))) #STANDARDIZED.
    abline(h=0, lty=2)
    abline(h=c(-2,2), lty=2, col="red")
  }
  if (print.obs & !id) {
    i <- names(rstandard(obj))[abs(rstandard(obj)) > 2]
    n <- names(obj$model)
    rep_df <- data.frame(thisdf[names(rstandard(obj)[i]),n], obj$fitted.values[names(rstandard(obj)[i])], rstandard(obj)[names(fitted.values(obj)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Standard_Resid")
    return(rep_df)
  }
  else if (id) {
    i <- identify(rstandard(obj), labels=names(rstandard(obj)))
    n <- names(obj$model)
    rep_df <- data.frame(thisdf[names(rstandard(obj)[i]),n], obj$fitted.values[names(rstandard(obj)[i])], rstandard(obj)[names(fitted.values(obj)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Standard_Resid")
    return(rep_df)
  }
}
