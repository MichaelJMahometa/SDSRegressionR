#' Create a simple plane
#'
#' Useful in describing a trivariate model. Used only in slides...
#'
#' @param obj Model object from lm().
#'
#' @keywords internal
#'
#' @export
simplePlane <- function(obj){
  require(rgl) #? Do I need this if I require rgl in the package?
  coefs <- coef(obj)
  plot3d(unlist(obj$model[2]),unlist(obj$model[3]),unlist(obj$model[1]), type="p", col="red", xlab=names(coefs[2]), ylab=names(coefs[3]), zlab=names(obj$model[1]), sixe=5, lwd=15)
  a <- coefs[2]
  b <- coefs[3]
  c <- -1
  d <- coefs["(Intercept)"]
  planes3d(a, b, c, d, alpha=0.2)
  print("Remember: detach('package:rgl', unload=TRUE)")
}
