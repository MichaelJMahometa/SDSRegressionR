#' Create a Leverage (hat values) Plot
#'
#' Simple function to produce a plot of the hat values (leverage) after a model run in lm().
#'
#' @inheritParams studResidPlot
#' @param ylim Optional ylim for the plot.
#'
#' @seealso
#' \code{\link{studResidPlot}}
#' \code{\link{cooksPlot}}
#' \code{\link{threeOuts}}
#'
#' @examples
#' mod <- lm(hp ~ disp, data=mtcars)
#' summary(mod)
#' leveragePlot(mod)
#'
#' @export
levPlot <- function(obj, ylim=NULL, key.variable=NULL, print.obs=FALSE, print.plot=TRUE){
  #thisdf <- get(paste(eval(obj)$call$data))
  thisdf <- obj$model
  K <- length(coef(obj))
  N <- length(hatvalues(obj))
  hat.avg <- K/N
  if (print.plot == TRUE){
  plot(hatvalues(obj), ylim=ylim, pch=16, main="Leverage", ylab="Leverage (Hat Values)")
  abline(h=c(2,3)*hat.avg, lty=2, col=c("orange", "red"))
  }
  if(print.obs) {
    if (any(class(thisdf) == "tbl_df") & is.null(key.variable)){
      #if tibble & NULL key.variable = NULL
      stop("Data is of tibble class -- key.variable must be supplied.")
    } else if (any(class(thisdf) == "tbl_df") & !is.null(key.variable)){
      #if tibble & NULL key.variable != NULL (key.variable="Subject_ID")
      thisdf <- add_column(thisdf, rn = row.names(thisdf), .before=key.variable)
      i <- names(hatvalues(obj))[hatvalues(obj) > hat.avg * 2]
      n <- names(obj$model)
      rep_df <- data.frame(thisdf[thisdf$rn %in% i, key.variable], #I think i like this indexing better...
                           thisdf[thisdf$rn %in% i, n],
                           obj$fitted.values[names(hatvalues(obj)[i])],
                           hatvalues(obj)[names(fitted.values(obj)[i])])
      names(rep_df) <- c(key.variable, n, "Predicted_Y", "Hat_Values")
      return(rep_df)
    } else {
      #if data.frame (or maybe data.table?)
      i <- names(hatvalues(obj))[hatvalues(obj) > hat.avg * 2]
      n <- names(obj$model)
      rep_df <- data.frame(i,
                           thisdf[row.names(thisdf) %in% i, n],
                           obj$fitted.values[names(hatvalues(obj)[i])],
                           hatvalues(obj)[names(fitted.values(obj)[i])])
      names(rep_df) <- c("row.names", n, "Predicted_Y", "Hat_Values")
      return(rep_df)
    }
  }
}
