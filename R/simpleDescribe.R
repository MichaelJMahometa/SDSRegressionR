#' Simple Descriptive Statistics for Numerical Data
#'
#' Create a table of descriptive statistics for a \emph{single} numerical variable. This function is \code{dplyr} friendly (accepts piping).
#'
#' @param df Dataframe of variable location.
#' @param vari Variable of numerical nature.
#' @param rnd Rounding of output table (data.frame). Defaults to 3.
#'
#' @examples
#' mtcars %>%
#'   group_by(cyl) %>%
#'   simpleDescribe(mpg)
#'
#' @export
simpleDescribe <- function(df, vari, rnd = 3) {
  #dplyr for the SINGLE quantitative
  expr <- enquo(vari)

  #summarize
  t <- summarise(df,
                 Mean = mean(!! vari, na.rm = TRUE),
                 SD = sd(!! vari, na.rm = TRUE),
                 Median = median(!! vari),
                 IQR = IQR(!! vari),
                 N = n(),
                 Miss = sum(is.na(!! vari)),
                 ValidN = length(!! vari) - sum(is.na(!! vari)),
                 SE = SD/sqrt(ValidN)
  )

  #round JUST the numeric variables
  t <- t %>%
    mutate_if(is.numeric, round, digits=rnd)

  #return the DATAFRAME---not tibble
  data.frame(t)
}
