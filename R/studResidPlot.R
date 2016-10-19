#' Create a Studentized Deleted Residual Plot
#'
#' Produce a studentized deleted residual plot.
#'
#' @param obj Object from an lm() fiitted equation.
#' @param id Logical: Should the identy() function be used to describe selected observations? Will automatically invoke print=TRUE for selected observations.
#' @param print.obs Logical: Should observations outside the specified sigma level be printed to the console?
#' @param print.plot Logical: Should plot be created?
#'
#' @seealso
#' \code{\link{levPlot}}
#' \code{\link{cooksPlot}}
#' \code{\link{threeOuts}}
#'
#' @examples
#' mod <- lm(hp ~ disp, data=mtcars)
#' summary(mod)
#' studResidPlot(mod)
#' @export
studResidPlot <- function(obj, id=FALSE, print.obs=FALSE, print.plot=TRUE){
  thisdf <- get(paste(eval(obj)$call$data))
  mx <- max(abs(rstudent(obj)))
  if (print.plot == TRUE){
  plot(rstudent(obj), pch=16, ylab="Studentized Residuals", main="Studentized Deleted Residuals", ylim=c(min(c(-mx, -2)), max(c(mx, 2)))) #studentized.
  abline(h=0, lty=2)
  abline(h=c(-2,2), lty=2, col="red")
  }
  if (print.obs & !id) {
    i <- names(rstudent(obj))[abs(rstudent(obj)) > 2]
    n <- names(obj$model)
    rep_df <- data.frame(thisdf[names(rstudent(obj)[i]),n], obj$fitted.values[names(rstudent(obj)[i])], rstudent(obj)[names(fitted.values(obj)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Student_Resid")
    return(rep_df)
  }
  else if (id) {
    i <- identify(rstudent(obj), labels=names(rstudent(obj)))
    n <- names(obj$model)
    rep_df <- data.frame(thisdf[names(rstudent(obj)[i]),n], obj$fitted.values[names(rstudent(obj)[i])], rstudent(obj)[names(fitted.values(obj)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Student_Resid")
    return(rep_df)
  }
}
