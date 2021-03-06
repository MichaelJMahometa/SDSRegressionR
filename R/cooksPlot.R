#' Create a Cook's Distance Plot
#'
#' Simple function to produce a plot of the Cook's Distance for each observation after a model run in lm().
#'
#' @inheritParams levPlot
#' @inheritParams studResidPlot
#' @param save.cutoff Logical: Should the Cook's Distance cutoff be saved to the Global Environment?
#'
#' @seealso
#' \code{\link{studResidPlot}}
#' \code{\link{levPlot}}
#' \code{\link{threeOuts}}
#'
#' @examples
#'
#' mod <- lm(hp ~ disp, data=mtcars)
#' summary(mod)
#' cooksPlot(mod)
#'
#'@export
cooksPlot <- function(obj, ylim=NULL, key.variable=NULL, print.obs=FALSE, print.plot=TRUE, sort.obs=TRUE, all.obs=FALSE, save.cutoff=FALSE){
  thisdf <- get(paste(eval(obj)$call$data))
  #thisdf <- obj$model
  cutoff <- 4/(obj$df.residual)
  if(print.plot == TRUE){
  plot(cooks.distance(obj), ylim=ylim, pch=16, main="Cook's Distance", ylab="Cook's Distance")
  #mtext(paste("Cutoff = ", round(cutoff, 6), sep=""), side=3)
  abline(h=cutoff, lty=2, col="red")
  }
  if(save.cutoff){
    assign("cooksCutOff", cutoff, envir = .GlobalEnv)
  }
  if(print.obs){
    if (any(class(thisdf) == "tbl_df") & is.null(key.variable)){
      #if tibble & NULL key.variable = NULL
      stop("Data is of tibble class -- key.variable must be supplied.")
    } else if (!is.null(key.variable)){
      #if tibble & NULL key.variable != NULL (key.variable="Subject_ID")
      thisdf <- add_column(thisdf, rn = row.names(thisdf), .before=key.variable)
      i <- names(cooks.distance(obj))[cooks.distance(obj) > cutoff]
      i <- i[!is.na(i)] #Fix for Nan in above
      if(all.obs == TRUE){
        i <- names(cooks.distance(obj))
      }
      n <- names(obj$model)
      rep_df <- data.frame(thisdf[which(thisdf$rn %in% i), key.variable],
                           thisdf[which(thisdf$rn %in% i), n],
                           obj$fitted.values[names(cooks.distance(obj)[i])],
                           cooks.distance(obj)[names(fitted.values(obj)[i])], row.names = NULL)
      names(rep_df) <- c(key.variable, n, "Predicted_Y", "Cooks_Distance")
    } else {
      #if data.frame (or maybe data.table?)
      i <- names(cooks.distance(obj))[cooks.distance(obj) > cutoff]
      i <- i[!is.na(i)] #Fix for Nan in above
      if(all.obs == TRUE){
        i <- names(cooks.distance(obj))
      }
      n <- names(obj$model)
      rep_df <- data.frame(i,
                           thisdf[which(row.names(thisdf) %in% i), n],
                           obj$fitted.values[names(cooks.distance(obj)[i])],
                           cooks.distance(obj)[names(fitted.values(obj)[i])], row.names = NULL)
      names(rep_df) <- c("row.names", n, "Predicted_Y", "Cooks_Distance")
    }
    if(sort.obs){
      rep_df <- rep_df[order(-rep_df$Cooks_Distance), ]
      row.names(rep_df) <- NULL
    }
    df1 <- summary(obj)$df[3]
    df2 <- summary(obj)$df[2]
    rep_df$F_per <- pf(rep_df$Cooks_Distance, df1, df2)
    return(rep_df)
  }
}
