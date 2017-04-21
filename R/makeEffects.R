#' Create Effect Codes
#'
#' Simple function to create Effect Codes from a single variable, given some base reference category. Only "k-1" Effect Variables will be created - excluding the base reference group.
#'
#' @param x Vector to be Dummy Coded.
#' @param ref Name of the base reference category.
#'
#' @seealso
#' \code{\link{makeDummies}}
#'
#' @examples
#'
#' makeEffects(mtcars$cyl, "4")
#' mtcars <- data.frame(mtcars, makeEffects(mtcars$cyl, "4"))
#'
#'@export
makeEffects <- function(x, ref){
  t <- table(x)
  lt <- length(t)
  n.obs <- length(x)
  new <- matrix(0, nrow = n.obs, ncol = lt)
  xlev <- as.factor(x)
  if(!ref %in% levels(xlev)){ #Added
    stop("Reference group name is not in original variable.")
  }
  base <- which(xlev == ref)
  for (i in 1:n.obs) {
    new[i, xlev[i]] <- 1
  }
  new[which(is.na(xlev)), ] <- NA #Added
  new[base, ] <- -1 #Added
  colnames(new) <- paste0("Eff.", names(t), "_base", ref) #Added
  remcol <- which(colnames(new) == paste0("Eff.", ref, "_base", ref)) #Added
  new <- new[,-remcol] #Added
  return(new)
}
