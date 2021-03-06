#' Create a Standardized Residual Plot
#'
#' Produce a Standardized Residual plot. NOT recommended. Use Studentized Deleted Residuals instead.
#'
#' This functions creates Standardized Residual values for an observation within an lm() model based on the following: \eqn{(e_i / S_e)}
#'
#' @param obj Object from an lm() fiitted equation.
#' @param key.variable Required if lm() data object is of tibble class. Name of the unique key variable (identifier variable). If data object is of data.frame class, this is optional and row.names will be used instead.
#' @param print.obs Logical: Should observations outside the specified sigma level be printed to the console?
#' @param print.plot Logical: Should plot be created?
#' @param sort.obs Logical: Should observations (if print.obs=TRUE) be sorted?
#' @param all.obs Logical: Should \strong{all} observations be printed?
#'
#' @seealso
#' \code{\link{studResidPlot}}
#'
#' @examples
#' mod <- lm(hp ~ disp, data=mtcars)
#' summary(mod)
#' standResidPlot(mod)
#'
#'
#' @export
standResidPlot <- function(obj, key.variable = NULL, print.obs=FALSE, print.plot=TRUE, sort.obs=TRUE, all.obs=FALSE){
  thisdf <- get(paste(eval(obj)$call$data)) #get ORIGINAL data (for tibble key.variable)
  #thisdf <- obj$model
  standRes <- resid(obj) / sigma(obj)
  # mx <- max(abs(rstandard(obj)))
  mx <- max(abs(standRes))
  if (print.plot == TRUE){
    # plot(rstandard(obj), pch=16, ylab="Standardized Residuals", main="Standardized Residuals", ylim=c(min(c(-mx, -2)), max(c(mx, 2)))) #STANDARDIZED.
    plot(standRes, pch=16, ylab="Standardized Residuals", main="Standardized Residuals", ylim=c(min(c(-mx, -2)), max(c(mx, 2)))) #STANDARDIZED.
    abline(h=0, lty=2)
    abline(h=c(-2,2), lty=2, col="red")
  }
  if (print.obs) {
    if (any(class(thisdf) == "tbl_df") & is.null(key.variable)){
      #if tibble & NULL key.variable = NULL
      stop("Data is of tibble class -- key.variable must be supplied.")
    } else if (!is.null(key.variable)){
      #if tibble & NULL key.variable != NULL (key.variable="Subject_ID")
      thisdf <- add_column(thisdf, rn = row.names(thisdf), .before=key.variable)
      i <- names(standRes)[abs(standRes) > 2]
      if(all.obs == TRUE){
        i <- names(standRes)
      }
      n <- names(obj$model)
      rep_df <- data.frame(thisdf[which(thisdf$rn %in% i), key.variable],
                           thisdf[which(thisdf$rn %in% i), n],
                           obj$fitted.values[names(standRes[i])],
                           standRes[names(fitted.values(obj)[i])], row.names = NULL)
      names(rep_df) <- c(key.variable, n, "Predicted_Y", "Standard_Resid")
    } else {
      #if data.frame (or maybe data.table?)
      i <- names(standRes)[abs(standRes) > 2]
      if(all.obs == TRUE){
        i <- names(standRes)
      }
      n <- names(obj$model)
      rep_df <- data.frame(i,
                           thisdf[which(row.names(thisdf) %in% i), n],
                           obj$fitted.values[names(standRes[i])],
                           standRes[names(fitted.values(obj)[i])], row.names = NULL)
      names(rep_df) <- c("row.names", n, "Predicted_Y", "Standard_Resid")
    }
    if(sort.obs){
      rep_df <- rep_df[order(rep_df$Standard_Resid), ]
      row.names(rep_df) <- NULL
    }
    return(rep_df)
  }
}
