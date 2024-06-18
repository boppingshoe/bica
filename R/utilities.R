
# Yukon River INSEASON FORECAST - Integrated Bayesian Inseason Model
# Utility functions

#' Getting day of year
#'
#' @param Month Numeric month
#' @param Day Numeric day
#'
#' @return
#' Julian date (non-leap year) for the Inseason Bayesian Projection Model
#' 
#' @export
#'
#' @examples
#' my_day_func(6, 10)
#'
my_day_func <- function(Month, Day) {
  # Df for storing dates
  date.df <- data.frame("dayofyear" = c(148:260),
                        "date" = seq(from = as.Date("2022-05-28"),
                                     to = as.Date("2022-9-17"), by = 1) )

  # Extract month and day
  date.df$month <- lubridate::month(date.df$date)
  date.df$day <- lubridate::day(date.df$date)

  my_day <- as.double(date.df$dayofyear[date.df$month == Month & date.df$day == Day])

  return(my_day)
}


