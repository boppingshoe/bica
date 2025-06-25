
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


#' Make trace plots
#'
#' @param obj Posterior trace from model output
#' @param vars Variables being plotted
#'
#' @return
#' Trace plots of parallel MCMC chains in ggplot
#' 
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' tr_plot(bica_out$pars, c("alpha", "beta", "sigma", "ln_run_size", "lp__"))
#' }
#'
#' @export
#'
tr_plot <- function (obj, vars) {
  vars <- c(vars, "chain", "itr")
  
  tibble::as_tibble({{ obj }}) %>%
    dplyr::rename(chain = `.chain`, itr = `.iteration`) %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    tidyr::pivot_longer(cols = -c(chain, itr)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = itr, y = value, color = factor(chain))) +
    ggplot2::facet_grid(name ~ ., scales = "free") +
    ggplot2::labs(color = "MC chain")
}

