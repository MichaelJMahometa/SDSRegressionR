#' Standardized Betas
#'
#' Simple function to produce standardized beta estimates after a model run in \code{lm()}.
#'
#' @param obj Model object from an \code{lm()} fitted equation.
#'
#' @examples
#' mod <- lm(hp ~ disp + wt, data=mtcars)
#' summary(mod)
#' lmBeta(mod)
#'
#' @export
lmBeta <- function(obj){
b <- summary(obj)$coef[-1, 1]
sx <- sapply(data.frame(model.matrix(obj)[,-1], stringsAsFactors = FALSE), sd, na.rm=TRUE)
sy <- sd(as.numeric(unlist(data.frame(model.frame(obj)[,1]))), na.rm=TRUE)
beta <- b * sx/sy
names(beta) <- names(data.frame(model.matrix(obj))[-1])
return(beta)
}
