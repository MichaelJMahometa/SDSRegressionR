#' Return DFBetas or DFBeta values
#'
#' Simple function to produce a listing of all DFBetas (or DFBeta) values for the coefficients in a lm() object. Observations to investigate (tagged as Investigate) are row sums of absolute coefficients greater than 2 / sqrt(N).
#'
#' @param obj Object from an lm() fiitted equation.
#' @param key.variable Used if lm() data object is of tibble class. Name of the unique key variable (identifier variable). If data object is of data.frame class, row.names will be used instead.
#' @param sort Logical: Should the returned dataframe be sorted by the number of tagged coefficients?
#' @param standardize Logicial: Should standardized DFBeta values (DFBetas) be returned in the dataframe?
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
dfBetas <- function(obj, key.variable = NULL, sort=FALSE, standardize=TRUE){
  thisdf <- get(paste(eval(obj)$call$data))
  tbetas <- data.frame(dfbetas(obj))
  tbeta <- data.frame(dfbeta(obj))
  if (any(class(thisdf) == "tbl_df") & is.null(key.variable)) {
    stop("Data is of tibble class -- key.variable must be supplied.")
  } else if (any(class(thisdf) == "tbl_df") & !is.null(key.variable)) {
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
  if(sort){
    mtdf <- mtdf[order(-mtdf$Investigate), ]
  }
  return(mtdf)
}
