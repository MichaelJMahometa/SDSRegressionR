#' Combine All Three Common Outlier Types
#'
#' Simple function to produce a plots and a final data frame from studentized deleted residuals, hat values (leverage), and Cook's distance, after a model run in lm(). Resulting dataframe will be sorted by the inThree variable (observation is considered an outlier on all three plots) and then the Cooks_D variable.
#'
#' @param obj Model object from an lm() fiitted equation.
#' @param key.variable Used if lm() data object is of tibble class. Name of the unique key variable (identifier variable). If data object is of data.frame class, row.names will be used instead.
#' @param print.plots Logical: Should plots for all three outlier tests be shown?
#'
#' @seealso
#' \code{\link{studResidPlot}}
#' \code{\link{levPlot}}
#' \code{\link{cooksPlot}}
#'
#' @examples
#' mod <- lm(hp ~ disp, data=mtcars)
#' summary(mod)
#' threeOuts(mod)
#'
#'@export
threeOuts <- function (obj, key.variable=NULL, print.plot=FALSE){
  s <- studResidPlot(obj, print.obs = TRUE, print.plot = print.plot, key.variable=key.variable)
  l <- levPlot(obj, print.obs = TRUE, print.plot = print.plot, key.variable=key.variable)
  c <- cooksPlot(obj, print.obs = TRUE, print.plot = print.plot, key.variable=key.variable)
  if(!is.null(key.variable)){
    all <- join_all(list(s, l, c), by = key.variable, type = "full")
  } else {
    all <- join_all(list(s, l, c), by = "row.names", type = "full")
  }
  all$inThree[!is.na(all$Student_Resid) & !is.na(all$Hat_Values) & !is.na(all$Cooks_D)] <- 1
  all <- all[order(-all$inThree, -all$Cooks_D), ]
  row.names(all) <- NULL
  all
}
