#' Return an R2 value for lm model variable
#'
#' Simple function to produce a single \eqn{R^2} value for a specified variable (including factors and interaction terms) from a lm() object. Uses anova() to generate type 1 Sums of Squares (sequential) for variable.
#'
#' @param obj Object from an lm() fiitted equation.
#' @param test.var Required. Variable name from lm() object.
#'
#' @seealso
#' \code{\link{pCorr}}
#'
#' @examples
#'
#'mtcars$gear <- factor(mtcars$gear)
#'mod <- lm(hp ~ disp + gear, data=mtcars)
#'summary(mod)
#'lmSingleR2(mod, "gear")
#'
#' @export
lmSingleR2 <- function(obj, test.var){
  "%not in%" <- Negate("%in%")
  thisdf <- model.frame(obj)
  pred_var <- names(thisdf)[1]
  av <- attr(obj$terms, "term.labels")
  if (test.var %not in% av) {
    stop("Variable of interest not in supplied model object.")
  }
  av <- av[which(av %not in% test.var)]
  thismod <- lm(as.formula(paste0(pred_var, " ~ ",
                                  paste0(av, collapse=" + "), " + ", test.var)), data=thisdf)
  ano <- anova(thismod)
  rsq <- ano$`Sum Sq`[row.names(ano) %in% test.var] / sum(ano$`Sum Sq`)
  rsq
}
