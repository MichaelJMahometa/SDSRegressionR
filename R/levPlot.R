#' Create a Leverage (hat values) Plot
#'
#' Simple function to produce a plot of the hat values (leverage) after a model run in lm().
#'
#' @inheritParams studResidPlot
#' @param ylim Optional ylim for the plot.
#' @param save.cutoff Logical: Should the used cutoff of the hat value be saved. The standard cut-offs (graphed) at 2 and 3 times the average hat value \eqn{((k+1) / n)}.
#' @param cut.level Level to be used in *printing* the list of Leverage outliers. Defaults to 3 times the average hat value \eqn{((k+1) / n)}.
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
levPlot <- function(obj, ylim=NULL, key.variable=NULL, print.obs=FALSE, print.plot=TRUE, sort.obs=TRUE, all.obs=FALSE, save.cutoff=FALSE, cut.level=3){
  thisdf <- get(paste(eval(obj)$call$data))
  #thisdf <- obj$model
  P <- length(coef(obj))
  N <- length(hatvalues(obj))
  hat.avg <- P/N
  if (print.plot == TRUE){
  plot(hatvalues(obj), ylim=ylim, pch=16, main="Leverage", ylab="Leverage (Hat Values)")
  abline(h=c(2,3)*hat.avg, lty=2, col=c("orange", "red"))
  }
  if(save.cutoff){
    hatCutoffs <- list(2*hat.avg, 3*hat.avg)
    assign("hatCutoffs", hatCutoffs, envir = .GlobalEnv)
  }
  if(print.obs) {
    if (any(class(thisdf) == "tbl_df") & is.null(key.variable)){
      #if tibble & NULL key.variable = NULL
      stop("Data is of tibble class -- key.variable must be supplied.")
    } else if (!is.null(key.variable)){
      #if tibble & NULL key.variable != NULL (key.variable="Subject_ID")
      thisdf <- add_column(thisdf, rn = row.names(thisdf), .before=key.variable)
      i <- names(hatvalues(obj))[hatvalues(obj) > hat.avg * cut.level]
      if(all.obs == TRUE){
        i <- names(hatvalues(obj))
      }
      n <- names(obj$model)
      rep_df <- data.frame(thisdf[which(thisdf$rn %in% i), key.variable],
                           thisdf[which(thisdf$rn %in% i), n],
                           obj$fitted.values[names(hatvalues(obj)[i])],
                           hatvalues(obj)[names(fitted.values(obj)[i])], row.names = NULL)
      names(rep_df) <- c(key.variable, n, "Predicted_Y", "Hat_Values")
    } else {
      #if data.frame (or maybe data.table?)
      i <- names(hatvalues(obj))[hatvalues(obj) > hat.avg * cut.level]
      if(all.obs == TRUE){
        i <- names(hatvalues(obj))
      }
      n <- names(obj$model)
      rep_df <- data.frame(i,
                           thisdf[which(row.names(thisdf) %in% i), n],
                           obj$fitted.values[names(hatvalues(obj)[i])],
                           hatvalues(obj)[names(fitted.values(obj)[i])], row.names = NULL)
      names(rep_df) <- c("row.names", n, "Predicted_Y", "Hat_Values")
    }
    if(sort.obs){
      rep_df <- rep_df[order(-rep_df$Hat_Values), ]
      row.names(rep_df) <- NULL
    }
    return(rep_df)
  }
}
