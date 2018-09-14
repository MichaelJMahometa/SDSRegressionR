#' Create a Studentized Deleted Residual Plot
#'
#' Produce a studentized deleted residual plot.
#'
#' @param obj Object from an lm() fiitted equation.
#' @param key.variable Required if lm() data object is of tibble class. Name of the unique key variable (identifier variable). If data object is of data.frame class, this is optional and row.names will be used instead.
#' @param print.obs Logical: Should observations outside the specified sigma level be printed to the console?
#' @param print.plot Logical: Should plot be created?
#' @param sort.obs Logical: Should observations (if print.obs=TRUE) be sorted?
#' @param all.obs Logical: Should \strong{all} observations be printed?
#'
#' @seealso
#' \code{\link{levPlot}}
#' \code{\link{cooksPlot}}
#' \code{\link{threeOuts}}
#'
#' @examples
#' mod <- lm(hp ~ disp, data=mtcars)
#' summary(mod)
#' studResidPlot(mod)
#' @export
studResidPlot <- function(obj, key.variable=NULL, print.obs=FALSE, print.plot=TRUE, sort.obs=FALSE, all.obs=FALSE){
  thisdf <- get(paste(eval(obj)$call$data)) #get ORIGINAL data (for tibble key.variable)
  #thisdf <- obj$model
  mx <- max(abs(rstudent(obj)))
  if (print.plot == TRUE){
  plot(rstudent(obj), pch=16, ylab="Studentized Residuals", main="Studentized Deleted Residuals", ylim=c(min(c(-mx, -2)), max(c(mx, 2)))) #studentized.
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
      i <- names(rstudent(obj))[abs(rstudent(obj)) > 2]
      if(all.obs == TRUE){
        i <- names(rstudent(obj))
      }
      n <- names(obj$model)
      rep_df <- data.frame(thisdf[which(thisdf$rn %in% i), key.variable],
                           thisdf[which(thisdf$rn %in% i), n],
                           obj$fitted.values[names(rstudent(obj)[i])],
                           rstudent(obj)[names(fitted.values(obj)[i])], row.names = NULL)
      names(rep_df) <- c(key.variable, n, "Predicted_Y", "Student_Resid")
    } else {
      #if data.frame (or maybe data.table?)
      i <- names(rstudent(obj))[abs(rstudent(obj)) > 2]
      if(all.obs == TRUE){
        i <- names(rstudent(obj))
      }
      n <- names(obj$model)
      rep_df <- data.frame(i,
                           thisdf[which(row.names(thisdf) %in% i), n],
                           obj$fitted.values[names(rstudent(obj)[i])],
                           rstandard(obj)[names(fitted.values(obj)[i])], row.names = NULL)
      names(rep_df) <- c("row.names", n, "Predicted_Y", "Student_Resid")
    }
    if(sort.obs){
      rep_df <- rep_df[order(-rep_df$Student_Resid), ]
      row.names(rep_df) <- NULL
    }
    return(rep_df)
  }
}

#Using which() for data.frames - one student couldn't get it to work without....
