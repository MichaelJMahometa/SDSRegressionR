
tagObs <- function(obj, tag=1, name=NULL){
  thisdf <- get(paste(eval(obj)$call$data))
  pars <- as.list(match.call()[-1])
  md <- as.character(pars$obj)
  n <- names(thisdf)
  i <- names(obj$fitted.values)

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
