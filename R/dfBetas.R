#' Return DFBetas or DFBeta values
#'
#' Simple function to produce a listing of all DFBetas (or DFBeta) values for the coefficients in a lm() object. Observations to investigate (tagged as Investigate) are row sums of absolute coefficients greater than 2 / sqrt(N). Only observations with a Investigte value of 1 or greater are printed by default.
#'
#' @param obj Object from an lm() fiitted equation.
#' @param key.variable Required if lm() data object is of tibble class. Name of the unique key variable (identifier variable). If data object is of data.frame class, this is optional and row.names will be used instead.
#' @param sort.obs Logical: Should the returned dataframe be sorted by the number of tagged coefficients?
#' @param standardize Logicial: Should standardized DFBeta values (DFBetas) be returned in the dataframe?
#' @param truncate Logical: Should the output be truncated to show ONLY observations that meet the cut-off standardized difference betas of \eqn{2/sqrt(N)} (the default) or ALL observations in the model?
#'
#' @seealso
#' \code{\link{studResidPlot}}
#' \code{\link{levPlot}}
#' \code{\link{cooksPlot}}
#' \code{\link{threeOuts}}
#'
#' @examples
#'
#' mod <- lm(hp ~ disp, data=mtcars)
#' summary(mod)
#' dfBetas(mod)
#'
#' @export
dfBetas <- function(obj, key.variable = NULL, sort.obs=FALSE, standardize=TRUE, truncate=TRUE){
  thisdf <- get(paste(eval(obj)$call$data))
  tbetas <- data.frame(dfbetas(obj))
  tbeta <- data.frame(dfbeta(obj))
  if (any(class(thisdf) == "tbl_df") & is.null(key.variable)) {
    stop("Data is of tibble class -- key.variable must be supplied.")
  } else if (!is.null(key.variable)) {
    thisdf <- add_column(thisdf, rn = row.names(thisdf), .before = key.variable)
    i <- row.names(tbetas)
    N <- nrow(thisdf)
    n <- names(tbetas)
    n <- c("(Intercept)", n[2:length(n)])
    mtdf <- data.frame(thisdf[thisdf$rn %in% i, key.variable],
                       tbetas,
                       row.names = NULL)
    names(mtdf) <- c(key.variable, paste0(n, "_std"))
    sl_mtdf <- as.data.frame(mtdf[,3:length(mtdf)])
    s <- rowSums(abs(sl_mtdf) > 2/sqrt(N))
    mtdf$Investigate <- s
    if(standardize == FALSE){
      mtdf <- data.frame(mtdf[,1],
                         tbeta,
                         mtdf$Investigate)
      names(mtdf) <- c(key.variable, n, "Investigate")
    }
  } else {
    i <- row.names(tbetas)
    N <- nrow(thisdf)
    n <- names(tbetas)
    n <- c("(Intercept)", n[2:length(n)])
    mtdf <- data.frame(i,
                       tbetas,
                       row.names = NULL)
    names(mtdf) <- c("row.names", paste0(n, "_std"))
    sl_mtdf <- as.data.frame(mtdf[,3:length(mtdf)])
    s <- rowSums(abs(sl_mtdf) > 2/sqrt(N))
    mtdf$Investigate <- s
    if(standardize == FALSE){
      mtdf <- data.frame(mtdf[,1],
                         tbeta,
                         mtdf$Investigate)
      names(mtdf) <- c("row.names", n, "Investigate")
    }
  }
  if(sort.obs){
    mtdf <- mtdf[order(-mtdf$Investigate), ]
  }
  if(truncate){
  num_invest <- sum(mtdf$Investigate)
  if(num_invest == 0){
    stop("No observations reach cut-off.")
  } else {
    mtdf <- mtdf[which(mtdf$Investigate >= 1),]
  }
  }
  return(mtdf)
}
