#' @title Summary Statistics
#' @description Produce summary statistics on the numeric variable/column in the data
#' @param data is the numeric data in use
#' @param Minimum is the minimum of the data
#' @param Maximum is the maximum of the data
#' @param Q1 is the 25% percentile of the data
#' @param Median is the median of the data
#' @param Q3 is the 75% percentile of the data
#' @param Range is the range of the data
#' @param IQR is the interquantile range of the data
#' @param SD is the standard deviation of the data
#'
#' @return the numeric minimum, first quartile (Q1), median, third quartile (Q3), maximum, range, and interquartile range (IQR), and standard deviation of the data
#'
#' @examples
#' library(gapminder)
#' Population_Stat <- stat_sum(gapminder$pop)
#' @export

stat_sum <- function(data) {
  if (!is.numeric(data)){
    stop("I am so sorry, but this function only works for numeric input")
  }
   Minimum <- min(data, na.rm=T)
   Maximum <- max(data, na.rm=T)
   Q1 <- stats::quantile(data, 0.25, na.rm=T)
   Median <- stats::median(data, na.rm=T)
   Q3 <- stats::quantile(data, 0.75, na.rm=T)
   Range <- range(data, na.rm=T)
   IQR <- stats::IQR(data, na.rm=T)
   SD <- stats::sd(data, na.rm=T)
   print(c(Minimum, Maximum, Q1, Median, Q3, Range, IQR, SD))}
