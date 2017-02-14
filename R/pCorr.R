#' Partial and Semipartial (Part) Correlations
#'
#' Simple function to produce partial and part (semipartial) correlation coefficients after a model run in lm(). Also returns the "squares" of each metric.
#'
#' @note Requires at least \strong{two} independent variables in the lm() object.
#'
#' @param obj Model object from an lm() fitted equation.
#'
#' @examples
#'  mod <- lm(hp ~ disp + wt, data=mtcars)
#'  summary(mod)
#'  pCorr(mod)
#'
#' @export
pCorr <- function(obj){
  thisdf <- data.frame(as.numeric(unlist(data.frame(model.frame(obj)[,1]))),
                       data.frame(model.matrix(obj)[,-1]))
  names(thisdf) <- c(obj$terms[[2]], names(data.frame(model.matrix(obj))[-1]))
  b <- names(data.frame(model.matrix(obj))[-1]) #all betas
  if(length(b) < 2){
    stop("pCorr requires two or more independent variables in lm() object.")
  }
  # m <- as.formula(obj$call$formula) #original formula from model
  # main <- lm(m , data=thisdf) #main model
  y <- obj$terms[[2]] #outcome for the original model
  opens <- matrix(rep(NA, length(b)*4), ncol=4) #vector for all cofficients
  for(i in 1:length(b)){
    tol <- (1 - summary(lm(as.formula(paste(b[i],  "~" , paste(b[-i], collapse= "+"))), data=thisdf))$r.squared) #tolerance for i coefficient
    sq_tol <- sqrt(tol) #square root of tolerance
    beta <- lmBeta(obj)[i] #ith std_beta from the main model
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
