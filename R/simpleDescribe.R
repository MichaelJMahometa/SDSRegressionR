#' Simple Descriptive Statistics for Numerical Data
#'
#' Create a table of descriptive statistics for a \emph{single} numerical variable. This function is \code{dplyr} friendly (accepts piping).
#'
#' @param df Dataframe of variable location.
#' @param oneVar Variable of numerical nature.
#' @param rnd Rounding of output table (data.frame). Defaults to 3.
#' @param range Logical. Should the range of the variable be included along with Min and Max.
#'
#' @examples
#' mtcars %>%
#'   group_by(cyl) %>%
#'   simpleDescribe(mpg)
#'
#' @export
simpleDescribe <- function(df, oneVar, rnd = 3, range=FALSE) {
  #dplyr for the SINGLE quantitative
  expr <- enquo(oneVar)

  #summarize
  t <- summarise(df,
                 Mean = mean(!! expr, na.rm = TRUE),
                 SD = sd(!! expr, na.rm = TRUE),
                 Median = median(!! expr),
                 IQR = IQR(!! expr),
                 N = n(),
                 Miss = sum(is.na(!! expr)),
                 ValidN = length(!! expr) - sum(is.na(!! expr)),
                 SE = SD/sqrt(ValidN)
  )

  if(range==TRUE){
    t <- summarise(df,
                   Mean = mean(!! expr, na.rm = TRUE),
                   SD = sd(!! expr, na.rm = TRUE),
                   Median = median(!! expr),
                   IQR = IQR(!! expr),
                   Min = min(!! expr),
                   Max = max(!! expr),
                   Range = Max - Min,
                   N = n(),
                   Miss = sum(is.na(!! expr)),
                   ValidN = length(!! expr) - sum(is.na(!! expr)),
                   SE = SD/sqrt(ValidN)
    )
  }
  #round JUST the numeric variables
  t <- t %>%
    mutate_if(is.numeric, round, digits=rnd)

  #return the DATAFRAME---not tibble
  data.frame(t)
}
