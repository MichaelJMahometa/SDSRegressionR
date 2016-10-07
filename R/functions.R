# Functions for Regression
# require ggplot2
# require rgl (3dplot)

#Add the subtitle option????
simpleScatter <- function(data, x, y, xlab=deparse(substitute(x)), ylab=deparse(substitute(y)), title=NULL, subtitle=NULL, xlim=NULL, ylim=NULL, line=FALSE, interval="none", ...){
  #require(ggplot2) #? Do I need this if I require ggplot2 in the package?
  pars <- as.list(match.call()[-1])
  xvar <- as.character(pars$x)
  yvar <- as.character(pars$y)
  g <- ggplot(data, aes_string(x=xvar, y=yvar)) +
    geom_point(size=4) +
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

#Add subtitle to ggplot object
addSubtitle <- function(title, subtitle){
  ggtitle(bquote(atop(.(title), atop(italic(.(subtitle)), ""))))
}

#Residual vs. Fitted Plot
residFitted <- function(model, sigma=FALSE, id=FALSE, print=FALSE, ...){
  thisdf <- get(paste(eval(model)$call$data))
  plot(fitted.values(model), residuals(model), pch=16, ylab="Residuals", xlab="Fitted Values", main="Residuals vs. Fitted",...)
  abline(h=0, lty=2, col="green")
  if (!sigma & print){
    warning("Sigma not initiated")
  }
  if (sigma & !id & !print) {
    abline(h=c(-summary(model)$sigma*2, summary(model)$sigma*2), col="red", lty=2)
  }
  else if (sigma & !id & print){
    abline(h=c(-summary(model)$sigma*2, summary(model)$sigma*2), col="red", lty=2)
    i <- names(rstudent(model))[abs(residuals(model)) > summary(model)$sigma*2]
    n <- names(model$model)
    rep_df <- data.frame(thisdf[names(fitted.values(model)[i]),n], model$fitted.values[names(fitted.values(model)[i])], model$residuals[names(fitted.values(model)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Residual_Value")
    return(rep_df)
  }
  else if (sigma & id) {
    abline(h=c(-summary(model)$sigma*2, summary(model)$sigma*2), col="red", lty=2)
    i <- identify(fitted.values(model), residuals(model), labels=names(fitted.values(model)))
    n <- names(model$model)
    rep_df <- data.frame(thisdf[names(fitted.values(model)[i]),n], model$fitted.values[names(fitted.values(model)[i])], model$residuals[names(fitted.values(model)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Residual_Value")
    return(rep_df)
  }
  else if(id){
    i <- identify(fitted.values(model), residuals(mod), labels=names(fitted.values(model)))
    n <- names(model$model)
    rep_df <- data.frame(thisdf[names(fitted.values(model)[i]),n], model$fitted.values[names(fitted.values(model)[i])], model$residuals[names(fitted.values(model)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Residual_Value")
    return(rep_df)
  }
}

#Tag observations used in a model...
tagObs <- function(model, tag=1, name=NULL){
  thisdf <- get(paste(eval(model)$call$data))
  pars <- as.list(match.call()[-1])
  md <- as.character(pars$model)
  n <- names(thisdf)
  i <- names(model$fitted.values)

  #thisdf <- data.frame(thisdf[names(fitted.values(model)[i]),], tag) #Possible for use to create "Used obs and variables" dataframe....
  #thisdf[, length(thisdf)+1] <- 0
  thisdf[i, length(thisdf)+1] <- tag
  if(is.null(name)){
    names(thisdf) <- c(n, paste("In_Model_", md, sep=""))
  } else {
    names(thisdf) <- c(n, paste("In_Model_", name, sep=""))
  }
  thisdf
}


#Funciton to add fitted values to dataframe...(with NAs for non-uesd obseravtions)
predictValues <- function(model, se.fit=FALSE, interval="none", name=NULL){
  thisdf <- get(paste(eval(model)$call$data))
  #thisdf <- tagObs(model) #Mark observations
  last <- length(thisdf) #Locate the mark varaible for later removal
  if (is.null(name)){
    dv <- formula(model)[[2]]
  }
  else{
    dv <- name
  }
  n <- names(thisdf)
  p <- data.frame(predict(model, thisdf, interval=interval, se.fit=se.fit))
  if(se.fit){
    l <- length(p)
    p <- p[,1:(l-2)]
  }
  clear <- setdiff(rownames(p), names(fitted.values(model)))
  p[clear,] <- NA #Make obs NOT in model as NA
  np <- names(p) #Use to mark columns that need to be altered
  #p[np][thisdf[,last] == 0,] <- NA #Drop those predictions with a non-tagged observation
  newdf <- merge(thisdf, p, by=0, all=TRUE, sort=FALSE)
  #newdf <- newdf[order(as.numeric(newdf$Row.names)),]
  row.names(newdf) <- newdf$Row.names
  newdf <- newdf[,-1]
  if (length(p) == 1){
    names(newdf) <- c(n, paste(dv, "_fit", sep=""))
  }
  else {
    #np <- names(p)
    names(newdf) <- c(n, paste(dv, np, sep="_"))
  }
  #newdf <- newdf[,-last] #Get rid of the tagging column
  newdf
}

#Studentized Deleted Residual Plot
studResidPlot <- function(model, id=FALSE, print=FALSE){
  thisdf <- get(paste(eval(model)$call$data))
  mx <- max(abs(rstudent(model)))
  plot(rstudent(model), pch=16, ylab="Studentized Residuals", main="Studentized Deleted Residuals", ylim=c(min(c(-mx, -2)), max(c(mx, 2)))) #studentized.
  abline(h=0, lty=2)
  abline(h=c(-2,2), lty=2, col="red")
  if (print & !id) {
    i <- names(rstudent(model))[abs(rstudent(model)) > 2]
    n <- names(model$model)
    rep_df <- data.frame(thisdf[names(rstudent(model)[i]),n], model$fitted.values[names(rstudent(model)[i])], rstudent(model)[names(fitted.values(model)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Student_Resid")
    return(rep_df)
  }
  else if (id) {
    i <- identify(rstudent(model), labels=names(rstudent(model)))
    n <- names(model$model)
    rep_df <- data.frame(thisdf[names(rstudent(model)[i]),n], model$fitted.values[names(rstudent(model)[i])], rstudent(model)[names(fitted.values(model)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Student_Resid")
    return(rep_df)
  }
}

#Leverage Plot (Hat Values)
leveragePlot <- function(model, ylim=NULL, print=FALSE, id=FALSE){
  thisdf <- get(paste(eval(model)$call$data))
  plot(hatvalues(model), ylim=ylim, pch=16, main="Leverage", ylab="Leverage (Hat Values)")
  K <- length(coef(model))
  N <- length(hatvalues(model))
  hat.avg <- K/N
  abline(h=c(2,3)*hat.avg, lty=2, col=c("orange", "red"))
  if(print & !id) {
    i <- names(hatvalues(model))[hatvalues(model) > hat.avg*2]
    n <- names(model$model)
    rep_df <- data.frame(thisdf[names(hatvalues(model)[i]),n], model$fitted.values[names(hatvalues(model)[i])], hatvalues(model)[names(hatvalues(model)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Hat_Values")
    return(rep_df)
  }
  else if(id){
    i <- identify(hatvalues(model), labels=names(hatvalues(model)))
    n <- names(model$model)
    rep_df <- data.frame(thisdf[names(hatvalues(model)[i]),n], model$fitted.values[names(hatvalues(model)[i])], hatvalues(model)[names(hatvalues(model)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Hat_Values")
    return(rep_df)
  }
}

#Cooks Distance Plot
cooksPlot <- function(model, ylim=NULL, id=FALSE, print=FALSE){
  thisdf <- get(paste(eval(model)$call$data))
  plot(cooks.distance(model), ylim=ylim, pch=16, main="Cook's Distance", ylab="Cook's Distance")
  cutoff <- 4/(model$df.residual)
  abline(h=cutoff, lty=2, col="red")
  if(print & !id){
    i <- names(cooks.distance(model))[cooks.distance(model) > cutoff]
    n <- names(model$model)
    rep_df <- data.frame(thisdf[names(cooks.distance(model)[i]),n], model$fitted.values[names(cooks.distance(model)[i])], cooks.distance(model)[names(cooks.distance(model)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Cooks_D")
    return(rep_df)
  }
  else if(id){
    i <- identify(cooks.distance(model), labels=names(cooks.distance(model)))
    n <- names(model$model)
    rep_df <- data.frame(thisdf[names(cooks.distance(model)[i]),n], model$fitted.values[names(cooks.distance(model)[i])], cooks.distance(model)[names(cooks.distance(model)[i])])
    names(rep_df) <- c(n, "Predicted_Y", "Cooks_D")
    return(rep_df)
  }
}


#Create an "all" outliers dataframe
allOuts <- function (model){
  s <- studResidPlot(model, print = TRUE)
  l <- leveragePlot(model, print = TRUE)
  c <- cooksPlot(model, print = TRUE)
  s$rn <- row.names(s)
  l$rn <- row.names(l)
  c$rn <- row.names(c)
  all <- join_all(list(s, l, c), by = "rn", type = "full")
  all$inAll[!is.na(all$Student_Resid) & !is.na(all$Hat_Values) & !is.na(all$Cooks_D)] <- 1
  all <- all[order(-all$inAll, -all$Cooks_D), ]
  rownames(all) <- all$rn
  all <- subset(all, select = !names(all) %in% "rn")
  all
}

#Create dataset with just used observations/variables from a model
#inModel <- function(model, thisdf){
#  thisdf[names(fitted.values(model)),colnames(model$model)]
#}

#Semi-partial (Part) Correlations from model
# spCorr <- function(model){
#   l <- summary(model)$thisdf[3]
#   k <- l-1
#   N <- summary(model)$thisdf[2] + summary(model)$thisdf[3]
#   c <- rep(NA, (l-1))
#   for(i in 1:(l-1)){
#     t <- summary(model)$coefficients[(i+1),3] #t-values
#     rsq <- summary(model)$r.squared
#
#     c[i] <- (t * sqrt(1-rsq)) / sqrt(N - k - 1)
#   }
#   names(c) <- (names(model$model)[2:l])
#   c
# }

#Partial Correlations from model
# pCorr <- function(model){
#   l <- summary(model)$thisdf[3]
#   k <- l-1
#   N <- summary(model)$thisdf[2] + summary(model)$thisdf[3]
#   c <- rep(NA, (l-1))
#   for(i in 1:(l-1)){
#     t <- summary(model)$coefficients[(i+1),3] #t-values
#
#     c[i] <- (t) / sqrt(t^2 + (N - k - 1))
#   }
#   names(c) <- (names(model$model)[2:l])
#   c
# }

#A Combined partial and part correlation output...
pCorr <- function(model){
  thisdf <- model$model #model's data (NOT orginal as n's may change...)
  b <- names(model$coeff[2:length(model$coeff)]) #all betas
  # m <- as.formula(model$call$formula) #original formula from model
  # main <- lm(m , data=thisdf) #main model
  y <- model$terms[[2]] #outcome for the original model
  opens <- matrix(rep(NA, length(b)*4), ncol=4) #vector for all cofficients
  for(i in 1:length(b)){
    tol <- (1 - summary(lm(as.formula(paste(b[i],  "~" , paste(b[-i], collapse= "+"))), data=thisdf))$r.squared) #tolerance for i coefficient
    sq_tol <- sqrt(tol) #square root of tolerance
    beta <- lmBeta(model)[i] #ith std_beta from the main model
    opens[i,3] <- beta * sq_tol #compute ith part corr
    opens[i,4] <- (opens[i,3])^2
    den <- sqrt((1 - summary(lm(as.formula(paste(y,  "~" , paste(b[-i], collapse= "+"))), data=thisdf))$r.squared))
    opens[i,1] <- (opens[i,3]) / den
    opens[i,2] <- (opens[i,1])^2
  }
  colnames(opens) <- c("Partial_Corr","Partial_Corr_sq", "Part_Corr", "Part_Corr_sq") #names of cols
  rownames(opens) <- b #assign the coef names
  data.frame(opens) #hand them out
}

#Standardized Betas (from QuantPsych lm.beta, but repackaged (and renamed here)
lmBeta <- function(model){
  b <- summary(model)$coef[-1, 1]
  sx <- sapply(model$model[-1], sd)
  sy <- sapply(model$model[1], sd)
  beta <- b * sx/sy
  beta
}

