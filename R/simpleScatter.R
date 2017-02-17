#' Create a Simple ggplot Scatter Plot
#'
#' A minimal function to produce a ggplot object from two continuous varaibles.
#'
#' @param data frame to be used.
#' @param x X (independent) variable to be plotted.
#' @param y Y (dependent) variable to be plotted.
#' @param xlab Label (optional) to be placed on the x axis.
#' @param ylab Label (optional) to be placed on the y axis.
#' @param title Title (optional) of the chart.
#' @param subtitle Subtitle (optional) of the chart.
#' @param xlim Typical xlim option to limit the graphed regions of the chart.
#' @param ylim Typical ylim option to limit the graphed regions of the chart.
#' @param line Logical: add linear fit line to plot?
#' @param interval Option denoting the type of interval to be plotted around linear prediction line. Option "none" is default. Other options include "confidence" and "prediction". Use of any option other than "none" will automatically change line=TRUE.
#' @param ptalpha Alpha value (opacity) of the points graphed. Defaults to 0.8.
#' @param ... Further arguments passed to or from other methods.
#' @return ggplot object
#' @examples
#' m <- mtcars
#' simpleScatter(m, disp, hp, title="Horsepower from Displacement")
#' @export
simpleScatter <- function(data, x, y, xlab=deparse(substitute(x)), ylab=deparse(substitute(y)), title=NULL, subtitle=NULL, xlim=NULL, ylim=NULL, line=FALSE, interval="none", ptalpha = 0.8, ...){
  #require(ggplot2) #? Do I need this if I require ggplot2 in the package?
  pars <- as.list(match.call()[-1])
  xvar <- as.character(pars$x)
  yvar <- as.character(pars$y)
  g <- ggplot(data, aes_string(x=xvar, y=yvar)) +
    geom_point(size=4, alpha=ptalpha, ...) +
    labs(title=title, x=xlab, y=ylab) +
    coord_cartesian(xlim=xlim, ylim=ylim) +
    theme_bw()
  if(line & interval=="none"){
    g +
      geom_smooth(method="lm", se=FALSE)
  } else if(interval=="confidence"){
    g +
      geom_smooth(method="lm", se=TRUE, ...)
  } else if(interval=="prediction") {
    xv <- data[,xvar]
    yv <- data[,yvar]
    m <- lm(yv~xv, data=data)
    p <- predict(m, data, interval="prediction", ...)
    clear <- setdiff(rownames(p), names(fitted.values(m)))
    p[clear,] <- NA
    newdf <- merge(data, p, by=0, all=TRUE, sort=FALSE)
    gp <- ggplot(newdf, aes_string(x=xvar, y=yvar)) +
      geom_point(size=4) +
      labs(title=title, x=xlab, y=ylab) +
      coord_cartesian(xlim=xlim, ylim=ylim) +
      theme_bw() +
      geom_smooth(method="lm", se=FALSE) +
      geom_ribbon(data=newdf, aes(ymin=newdf[,(length(newdf)-1)], ymax=newdf[,(length(newdf)-0)]) , alpha=0.2)
    gp
  } else {
    g
  }
}

#Add subplot options?
