## stripChunk function

in_file <- "/Users/mjmahometa/ mjmahometa/ Work/ Division/ Summer Institute/  2017/ ForClass/ClassSyntax.Rmd"
chunk_line=FALSE
stripChunk <- function(in_file, out_file, chunk_line=FALSE){
  require(stringr)

  sl <- unlist(str_locate_all(in_file, "/")) #stringr
  dir <- substr(in_file, 1 ,max(sl))
  keep <- chunk_line

  rmd <- readLines(in_file, warn=FALSE)

  start <- str_match(rmd,"\\```\\{") #stringr
  end <- str_match(rmd,"^```$") #stringr
  choose <- data.frame(!is.na(start), !is.na(end))
  s <- which(choose[,1] == 1)
  e <- which(choose[,2] == 1)
  code <- rmd[(s[1]+1-keep):(e[1]-1+keep)]
  for(i in 2:length(s)){
    temp <- rmd[(s[i]+1-keep):(e[i]-1+keep)]
    code <- c(code, " ", temp)
  }

  fileConn<-file(paste(dir, out_file, sep=""))
  writeLines(code, fileConn)
  close(fileConn)
}

stripChunk("/Users/mjmahometa/ mjmahometa/ Work/ Division/ Summer Institute/  2017/ ForClass/ClassSyntax.Rmd", "ClassSyntax.R")

## From knitr
library(knitr)
purl("/Users/mjmahometa/ mjmahometa/ Work/ Division/ Summer Institute/  2017/ ForClass/ClassSyntax_Day4.Rmd", documentation = 0)

#histogram with right=FALSE (COPY to SDSFoundations)
# histogram <- function(x, breaks=NULL, table=FALSE, right=FALSE, xlab=deparse(substitute(x)), title=NULL,...){
#   if(is.null(title)){
#     lab <- deparse(substitute(x))
#     lab2 <- paste("Histogram of ", lab, sep="")
#   } else {
#     lab2 <- title
#   }
#   if(is.null(breaks)){
#     h <- hist(x, right=right, xaxt="n", xlab=xlab, main=lab2,...)
#     axis(1, at=h$breaks, labels=h$breaks)
#     if(table == TRUE){
#       span.cut <- cut(x, h$breaks, right=right)
#       span.freq <- table(span.cut)
#       t <- cbind(span.freq)
#       return(t)
#     }
#     return(invisible(h$breaks))
#   } else {
#     h2 <- hist(x, breaks=breaks, right=right, xaxt="n", xlab=xlab, main=lab2, ...)
#     axis(1, at=h2$breaks, labels=h2$breaks)
#     if(table == TRUE){
#       span.cut <- cut(x, h2$breaks, right=right)
#       span.freq <- table(span.cut)
#       t <- cbind(span.freq)
#       return(t)
#     }
#     return(invisible(h2$breaks))
#   }
# }

#Make a simple plane for 2 independent variables
# simplePlane <- function(model){
#   require(rgl) #? Do I need this if I require rgl in the package?
#   coefs <- coef(model)
#   plot3d(unlist(model$model[2]),unlist(model$model[3]),unlist(model$model[1]), type="p", col="red", xlab=names(coefs[2]), ylab=names(coefs[3]), zlab=names(model$model[1]), sixe=5, lwd=15)
#   a <- coefs[2]
#   b <- coefs[3]
#   c <- -1
#   d <- coefs["(Intercept)"]
#   planes3d(a, b, c, d, alpha=0.2)
#   print("Remember: detach('package:rgl', unload=TRUE)")
# }

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
