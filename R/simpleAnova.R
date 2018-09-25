#' Simple Anova Table
#'
#' Simple function to produce an ANOVA table (Model and Error) after a model run in \code{lm()}. This replicates the F-test in the \code{summary.lm()} function.
#'
#' @param obj Model object from an \code{lm()} fitted equation.
#' @param ... Additional \code{anova()} options if needed.
#'
#' @seealso
#' \code{\link{lm}}
#'
#' @examples
#'
#' mod <- lm(hp ~ disp, data=mtcars)
#' simpleAnova(mod)
#' summary(mod)
#'
#' @section References:
#'
#' Found via the interwebs \href{https://stats.stackexchange.com/questions/145790/anova-table-for-model-in-r}{here}.
#'
#'@export
simpleAnova <- function(obj, ...) {

  # Compute anova table
  tab <- anova(obj, ...)

  # Obtain number of predictors
  k <- nrow(tab) - 1

  # Add predictors row
  predictorsRow <- colSums(tab[1:k, 1:2])
  predictorsRow <- c(predictorsRow, predictorsRow[2] / predictorsRow[1])

  # F-quantities
  Fval <- predictorsRow[3] / tab[k + 1, 3]
  pval <- pf(Fval, df1 = k, df2 = tab$Df[k + 1], lower.tail = FALSE)
  predictorsRow <- c(predictorsRow, Fval, pval)

  # Simplified table
  tab <- rbind(predictorsRow, tab[k + 1, ])
  row.names(tab)[1] <- "Predictors"
  return(tab)

}

