#Original finctions (NOT USED)
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
    i <- hatvalues(model) > hat.avg*2
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
  cutoff <- 4/(model$thisdf.residual)
  abline(h=cutoff, lty=2, col="red")
  if(print & !id){
    i <- cooks.distance(model) > cutoff
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
