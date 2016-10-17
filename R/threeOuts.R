#' Combine All Three Common Outlier Types
#'
#' Simple function to produce a plots and a final data frame from studentized deleted residuals, hat values (leverage), and Cook's distance, after a model run in lm(). Resulting dataframe will be sorted by the inThree variable (observation is considered an outlier on all three plots) and then the Cooks_D variable.
#'
#' @param obj Model object from an lm() fiitted equation.
#' @param print.plots Logical: Should plots for all three outlier tests be shown?
#'
#' @seealso
#' \code{\link{studResidPlot}}
#' \code{\link{leveragePlot}}
#' \code{\link{cooksPlot}}
#'
#' @examples
#' mod <- lm(hp ~ disp, data=mtcars)
#' summary(mod)
#' threeOuts(mod)
#'
#'@export
threeOuts <- function (obj, print.plot=FALSE){
  s <- studResidPlot(obj, print = TRUE, print.plot = print.plot)
  l <- leveragePlot(obj, print = TRUE, print.plot = print.plot)
  c <- cooksPlot(obj, print = TRUE, print.plot = print.plot)
  s$rn <- row.names(s)
  l$rn <- row.names(l)
  c$rn <- row.names(c)
  all <- join_all(list(s, l, c), by = "rn", type = "full")
  all$inThree[!is.na(all$Student_Resid) & !is.na(all$Hat_Values) & !is.na(all$Cooks_D)] <- 1
  all <- all[order(-all$inThree, -all$Cooks_D), ]
  rownames(all) <- all$rn
  all <- subset(all, select = !names(all) %in% "rn")
  all
}
