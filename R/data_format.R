
# Yukon River INSEASON FORECAST - Integrated Bayesian Inseason Model

#' Format data for BICA model
#'
#' @param my_year Year the projection is made
#' @param my_day Day the projection is made
#' @param end_year Last Complete Year
#' @param pf_hist Pre-season forecasts from 2007 to present
#' @param can_hist EOS total Canadian-origin Chinook estimates
#' @param pss_hist Daily Pilot Station Sonar passage estimates
#' @param eagle_hist Eagle sonar passage estimates
#' @param prior_df_log Priors for logistic model parameters
#' @param prior_df_norm Priors for normal model parameters
#' @param gsi_by_year Genetic apportionment of Canadian-origin Chinook
#' @param pss_sd Variance estimates for PSS
#' @param start_day_pss First day of PSS calculation
#' @param start_year_pss First year to include in PSS
#'
#' @return
#' A list of input data for the BICA model
#' 
#' @importFrom magrittr %>%
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' wd <- getwd() # path to working folder
#' dir.data <- file.path(wd, "data")
#' my_year <- 2023
#' my_day <- bica::my_day_func(Month = 8, Day = 9)
#' end_year <- my_year - 1
#' pf_hist <- readRDS(file.path(dir.data, "Yukon Canadian Chinook PF 29May24.RDS"))
#' can_hist <- readRDS(file.path(dir.data, "Canadian Chinook Passage RR 2Apr24.RDS"))
#' pss_hist <- read_xlsx(file.path(dir.data, "ADFG PSS Daily Reports/Yukon Escapement Daily 9Aug23.xlsx"), skip = 3)
#' eagle_hist <- read_xlsx(file.path(dir.data, "ADFG Eagle Daily Reports/Yukon Escapement Daily Eagle 9Aug23.xlsx"), skip = 3)
#' # gsi_by_year <- readRDS(file = file.path(dir.data, "GSI by year unadj 4Apr24.RDS")) # optional
#' # pss_sd <- readRDS(file = file.path(dir.data, "PSS SD 1995_2021.RDS")) # optional
#' # prior_df_log <- read.csv(file = file.path(dir.data, "logistic curve parameters All Chinook 1995_2022.csv")) # optional
#' prior_df_norm <- readRDS(file = file.path(dir.data, "normal curve parameters All Chinook 1995_2023.RDS"))
#'
#' bica_data <- bica::format_bica_data(my_year, my_day, end_year, pf_hist, can_hist, pss_hist, eagle_hist, prior_df_norm, prior_df_log = NULL, gsi_by_year = NULL, pss_sd = NULL, start_day_pss = 148, start_year_pss = 1995)
#' }
#'
format_bica_data <- function(
    my_year, my_day, end_year,
    pf_hist, can_hist, pss_hist, eagle_hist,
    prior_df_norm, prior_df_log = NULL,
    gsi_by_year = NULL, pss_sd = NULL,
    start_day_pss = 148, start_year_pss = 1995
) {

  # Wont typicaly change;
  # day 152 = June 1
  # start_day_pss <- 148
  end_day_pss <- 250

  # Preseason Forecast ###############################
  # Current Preseason forecast CAN-orig Chinook
  pf <- log(pf_hist$mean[pf_hist$Year == my_year]) # for stan

  # Vector of historical preseason forecasts for to compute sd for prior in stan model
  pf_vect <- pf_hist$mean[
    pf_hist$Year <= end_year &
      pf_hist$Year != my_year]

  names(pf_vect) <- pf_hist$Year[
    pf_hist$Year <= end_year &
      pf_hist$Year != my_year]

  # EOS reconstructed runsize for historic years that have a preseason forecast
  eos <- can_hist %>%
    dplyr::filter(Year != my_year & Year >= 2007 & Year <= end_year) %>%
    dplyr::pull(can.mean) %>%
    purrr::set_names({
      dplyr::filter(can_hist,
                    Year != my_year & Year >= 2007 & Year <= end_year) %>%
        dplyr::pull(Year)
    }) # for stan

  # SD for preseason forecast prior
  pf_sigma <- sd(log(pf_vect / eos)) # for stan

  # Years in PF
  year_pf <- unique(pf_hist$Year[pf_hist$Year != my_year
                                & pf_hist$Year <= end_year])

  n_years_pf <- length(year_pf) # for stan


  # Historic EOS Reconstructed Can-origin Run Size ######################
  # Vector of run sizes excluding the year of interest
  total_eos <- can_hist %>%
    dplyr::filter(Year != 1996 & # No PSS 1996
                    Year != my_year &
                    Year >= start_year_pss &
                    Year <= end_year) %>%
    dplyr::pull(can.mean) %>%
    purrr::set_names({
      dplyr::filter(can_hist,
                    Year != 1996 & # No PSS 1996
                      Year != my_year &
                      Year >= start_year_pss &
                      Year <= end_year) %>%
        dplyr::pull(Year)
    }) # for stan

  # Number of years included in EOS
  n_total_eos <- length(total_eos) # for stan


  # Pilot Station Sonar Data ###########################
  # Vector of historic PSS years excluding my_year
  pss_hist <- pss_hist %>%
    dplyr::mutate(Day = 148:252) %>% # julian day column from 5/28 to 9/9 (non-leap years)
    dplyr::select(-`Month/Day`) %>%
    tidyr::pivot_longer(cols = -c(Day),
                        names_to = "Year",
                        values_to = "count",
                        names_transform = list(Year = as.numeric)) %>%
    tidyr::replace_na(list(count = 0)) %>% # replace NA with zero
    dplyr::arrange(Year)
  
  year_pss <- unique(pss_hist$Year[pss_hist$Year != my_year &
                                    pss_hist$Year != 1996 &
                                    pss_hist$Year >= start_year_pss &
                                    pss_hist$Year <= end_year]) # for stan

  # Number of years used in model
  n_year_pss <- length(year_pss) # for stan

  # PSS days included up to my_day
  day_pss <- unique(pss_hist$Day[pss_hist$Day <= my_day &
                                  pss_hist$Day >= start_day_pss]) # for stan

  # Number of days used
  n_day_pss <- length(day_pss) # for stan

  # Number of total days in the season
  day_pss_all <- start_day_pss:end_day_pss

  n_day_pss_all <- length(day_pss_all)

  loc_all_days_my_day <- which(day_pss_all %in% my_day)

  # PSS daily passage estimate for days up to my_day for the year of interest (my_year)
  curr_pss <- pss_hist %>%
    dplyr::filter(Year == my_year & Day <= my_day & Day >= start_day_pss) %>%
    dplyr::pull(count) %>%
    purrr::set_names({
      dplyr::filter(pss_hist,
                    Year == my_year & Day <= my_day & Day >= start_day_pss) %>%
        dplyr::pull(Day)
    }) # for stan

  # Number of days used
  n_curr_pss <- length(curr_pss) # for stan

  # Cumulative current PSS counts
  cum_curr_pss <- cumsum(curr_pss)

  # Create matrix with dimensions days x historic Year
  pss_mat <- pss_hist %>%
    dplyr::filter(Year != my_year & Year <= max(can_hist$Year) & Year >= start_year_pss & Year <= end_year & Day <= my_day & Day >= start_day_pss) %>%
    tidyr::pivot_wider(values_from = count, names_from = Year) %>%
    as.data.frame() %>%
    magrittr::set_rownames(.$Day) %>%
    dplyr::select(-Day) %>%
    as.matrix() # for stan

  # Cumulative PSS matrix
  cum_pss_mat <- apply(pss_mat, 2, cumsum)

  # Total up to day of interest
  cum_pss <- colSums(pss_mat)
  
  # PSS martix of all the passage for historic/retro years
  pss_mat_all <- pss_hist %>%
    dplyr::filter(Year != my_year & Year >= start_year_pss & Year <= end_year & Day <= end_day_pss & Day >= start_day_pss) %>%
    tidyr::pivot_wider(values_from = count, names_from = Year) %>%
    as.data.frame() %>%
    magrittr::set_rownames(.$Day) %>%
    dplyr::select(-Day) %>%
    as.matrix() # for stan

  cum_pss_all <- colSums(pss_mat_all)

  cum_pss_mat_all <- apply(pss_mat_all, 2, cumsum)

  # observed proportions by day and year
  pss_mat_prop_all <- t(t(cum_pss_mat_all) / cum_pss_all)


  # Eagle Sonar preprocessing ###############################
  # Vector of historic Eagle Sonar passage excluding my_year
  eagle_hist <- eagle_hist %>%
    dplyr::mutate(Day = 178:243) %>% # julian day column from 6/27 to 8/31 (non-leap years)
    dplyr::select(-`Month/Day`) %>%
    tidyr::pivot_longer(cols = -c(Day),
                        names_to = "Year",
                        values_to = "count",
                        names_transform  = list(Year = as.numeric)) %>%
    tidyr::replace_na(list(count = 0)) %>% # replace NA with zero
    dplyr::arrange(Year)
  
  year_eagle <- unique(eagle_hist$Year[eagle_hist$Year != my_year
                                      & eagle_hist$Year <= end_year])

  # Number of Eagle sonar years used in model
  n_year_eagle <- length(year_eagle)

  day_eagle <- unique(eagle_hist$Day[eagle_hist$Day <= my_day &
                                       eagle_hist$Day >= start_day_pss])

  # Number of days used
  n_day_eagle <- length(day_eagle)

  # Eagle daily passage estimate for days up to my_day for the year of interest (my_year)
  curr_eagle_vect <- eagle_hist$count[eagle_hist$Year == my_year &
                                         eagle_hist$Day <= my_day &
                                         eagle_hist$Day >= start_day_pss]

  curr_eagle <- array(data = curr_eagle_vect,
                      dim = length(curr_eagle_vect))

  # Number of days used
  n_curr_eagle <- length(curr_eagle)

  # Cumulative Eagle counts for my_year
  cum_curr_eagle <- cumsum(curr_eagle)

  # Location pointers to index years in common for variance calculations
  loc_eagle_years <- which(year_pss %in% year_eagle)

  loc_pf_years_eagle <- which(year_eagle %in% year_pf)

  loc_pf_years_pss <- which(year_pss %in% year_pf)

  # Create matrix with dimensions my_day and my_year
  eagle_mat <- matrix(0, nrow = n_day_pss,
                      # start 1996 to account for missing year 1996
                      ncol = n_year_eagle)

  # Give names to matrix for accounting
  colnames(eagle_mat) <- year_eagle
  rownames(eagle_mat) <- day_pss

  # index where to start filling the Eagle matrix
  SE <- n_day_pss - n_day_eagle + 1

  if (my_day >= 178) {
    eagle_mat[SE:n_day_pss, ] <- eagle_hist %>%
      dplyr::filter(Year != my_year & Year <= end_year & Day <= my_day & Day >= start_day_pss) %>%
      tidyr::pivot_wider(values_from = count, names_from = Year) %>%
      as.data.frame() %>%
      magrittr::set_rownames(.$Day) %>%
      dplyr::select(-Day) %>%
      as.matrix() # for stan
  }

  # Total counts by year up to my_day for plotting
  cum_eagle <- colSums(eagle_mat)

  # Calculate the cumulative proportion for day D for each year Y
  prop_eagle <- cum_eagle / total_eos[loc_eagle_years]

  # Mean proportion
  mean_prop_eagle <- mean(prop_eagle)
  
  # Normal distribution parameters for informative priors (PSSnormal_ESprop)
  ps_mu <- prior_df_norm$mid[prior_df_norm$year != my_year &
                               prior_df_norm$year >=start_year_pss &
                               prior_df_norm$year <= end_year]
  
  ps_sd <- prior_df_norm$sd[prior_df_norm$year != my_year&
                              prior_df_norm$year >=start_year_pss &
                              prior_df_norm$year <= end_year]
  
  ps_alpha_norm <- prior_df_norm$alpha[prior_df_norm$year != my_year&
                                         prior_df_norm$year >=start_year_pss &
                                         prior_df_norm$year <= end_year]
  
  n_ps_mu <- length(ps_mu)
  n_ps_sd <- length(ps_sd)
  n_ps_alpha_norm <- length(ps_alpha_norm)
  

  # avg GSI proportions ###################################
  # for each day ACROSS ALL YEARS (PSSreg_GSI)
  if (!is.null(gsi_by_year)) {
    gsi_tbl <-
      sapply(seq.int(length(day_pss)), function(i) {
        while (all(gsi_by_year$startday > day_pss[i])) {
          i <- i + 1
        }
        gsi_by_year %>%
          dplyr::filter(startday <= day_pss[i], endday >= day_pss[i],
                        !year %in% c(my_year, 2013), year <= end_year) %>%
          dplyr::summarise(mean_gsi = mean(propCan),
                           sd_gsi = sd(propCan)) %>% unlist()
      }) %>% t() %>% as.data.frame()
    
    mean_gsi_vect <- gsi_tbl$mean_gsi %>%
      purrr::set_names(day_pss)
    sd_gsi_vect <- gsi_tbl$sd_gsi %>%
      purrr::set_names(day_pss)
    
    gsi_all_tbl <-
      sapply(seq.int(length(day_pss_all)), function(i) {
        while (all(gsi_by_year$startday > day_pss_all[i])) {
          i <- i + 1
        }
        while (all(gsi_by_year$endday < day_pss_all[i])) {
          i <- i - 1
        }
        gsi_by_year %>%
          dplyr::filter(startday <= day_pss_all[i], endday >= day_pss_all[i],
                        !year %in% c(my_year, 2013), year <= end_year) %>%
          dplyr::summarise(mean_gsi = mean(propCan),
                           sd_gsi = sd(propCan)) %>% unlist()
      }) %>% t() %>% as.data.frame()
    
    mean_gsi_vect_all <- gsi_all_tbl$mean_gsi %>%
      purrr::set_names(day_pss_all)
    sd_gsi_vect_all <- gsi_all_tbl$sd_gsi %>%
      purrr::set_names(day_pss_all)
    
    # Create matrix with dimensions my_day and my_year
    pss_mat_adj <- matrix(nrow = n_day_pss,
                          ncol = n_year_pss)
    
    # Give names to matrix
    colnames(pss_mat_adj) <- year_pss
    rownames(pss_mat_adj) <- day_pss
    
    # Can be used for plotting outputs (not used in model)
    for (y in 1:n_year_pss) {
      for (d in 1:n_day_pss) {
        # y = 1, d = 15
        pss_mat_adj[d, y] <- pss_mat[d, y] * mean_gsi_vect[d]
      }
    }
    
    # Cumulative GSI adjusted historic counts
    cum_pss_adj <- colSums(pss_mat_adj)
    
    # Create matrix with dimensions my_day and my_year
    pss_mat_all_adj <- matrix(nrow = length(start_day_pss:end_day_pss),
                              # start 1996 to account for missing year 1996
                              ncol = n_year_pss)
    
    # Give names to matrix for accounting
    colnames(pss_mat_all_adj) <- year_pss
    rownames(pss_mat_all_adj) <- c(start_day_pss:end_day_pss)
    
    # Use loop to populate matrix with counts by days
    # Set counter
    counter <- 1
    
    for (y in 1:n_year_pss){
      for (d in 1:(length(start_day_pss:end_day_pss))) {
        pss_mat_all_adj[d,y] <- pss_mat_all[d,y] * mean_gsi_vect_all[d]
      }
    }
    
    curr_pss <- pss_hist %>%
      dplyr::filter(Year == my_year & Day <= my_day & Day >= start_day_pss) %>%
      dplyr::pull(count) %>%
      purrr::set_names({
        dplyr::filter(pss_hist,
                      Year == my_year & Day <= my_day & Day >= start_day_pss) %>%
          dplyr::pull(Day)
      })
    
    adj_curr_pss <- vector(length = n_day_pss)
    for (d in 1:n_day_pss) {
      adj_curr_pss[d] <- curr_pss[d] * mean_gsi_vect[d]
    }
  } # end if (!is.null(gsi_by_year))

  # Normal distribution parameters for informative priors (PSSnormal_ESprop)
  ps_mu <- prior_df_norm$mid[prior_df_norm$year != my_year &
                               prior_df_norm$year >=start_year_pss &
                               prior_df_norm$year <= end_year]
  
  ps_sd <- prior_df_norm$sd[prior_df_norm$year != my_year&
                              prior_df_norm$year >=start_year_pss &
                              prior_df_norm$year <= end_year]
  
  ps_alpha_norm <- prior_df_norm$alpha[prior_df_norm$year != my_year&
                                         prior_df_norm$year >=start_year_pss &
                                         prior_df_norm$year <= end_year]
  
  n_ps_mu <- length(ps_mu)
  n_ps_sd <- length(ps_sd)
  n_ps_alpha_norm <- length(ps_alpha_norm)
  
  # Logistic parameters for informative prior (PSSlogistic_ESprop)
  if (!is.null(prior_df_log)) {
    ps_m <- prior_df_log$mid[prior_df_log$year != my_year &
                               prior_df_log$year >= start_year_pss &
                               prior_df_log$year <= end_year]
    
    ps_s <- prior_df_log$sd[prior_df_log$year != my_year&
                              prior_df_log$year >= start_year_pss &
                              prior_df_log$year <= end_year]
    
    ps_alpha_log <- prior_df_log$alpha[prior_df_log$year != my_year&
                                         prior_df_log$year >= start_year_pss &
                                         prior_df_log$year <= end_year]
    
    n_ps_m <- length(ps_m)
    n_ps_s <- length(ps_s)
    n_ps_alpha_log <- length(ps_alpha_log)
  }
  
  # PSS SD (Uncertainty)
  if (!is.null(pss_sd)) {
    temp_sd <- pss_sd %>%
      dplyr::filter(Day <= my_day, Year != my_year, Year >= start_year_pss) %>%
      dplyr::summarise(sd = sqrt(sum(Var)), .by = Year)
    
    full_temp_sd <- dplyr::tibble(Year = year_pss) %>%
      dplyr::left_join(temp_sd, by = "Year") %>%
      tidyr::replace_na(list(sd = 0))
    
    pss_year_sd <- full_temp_sd$sd %>% purrr::set_names(year_pss)
    
    curr_pss_year_sd <-
      sqrt(sum(pss_sd$Var[pss_sd$Day <= my_day & pss_sd$Year == my_year]))
  }
  
  
  # data list that goes to Stan model ######################################
  dat_out <-
    list(
      "pf" = pf,
      "pf_sigma" = pf_sigma,
      "n_years_pf" = n_years_pf,
      "n_total_eos" = n_total_eos,
      "total_eos" = total_eos,
      "n_day_pss" = n_day_pss,
      "day_pss" = day_pss,
      "n_year_pss" = n_year_pss,
      "year_pss" = year_pss,
      "pss_mat" = pss_mat,
      "n_curr_pss" = n_curr_pss,
      "curr_pss" = curr_pss,
      "loc_pf_years_pss" = loc_pf_years_pss,
      # append for pss_norm_es_prop
      "n_day_pss_all" = n_day_pss_all,
      "day_pss_all" = day_pss_all,
      "pss_mat_all" = pss_mat_all,
      "n_ps_mu" = n_ps_mu,
      "n_ps_sd" = n_ps_sd,
      "n_ps_alpha_norm" = n_ps_alpha_norm,
      "ps_mu" = ps_mu,
      "ps_sd" = ps_sd,
      "ps_alpha_norm" = ps_alpha_norm,
      "n_year_eagle" = n_year_eagle,
      "eagle_mat" = eagle_mat,
      "n_curr_eagle" = n_curr_eagle,
      "curr_eagle" = curr_eagle,
      "loc_eagle_years" = loc_eagle_years,
      "loc_pf_years_eagle" = loc_pf_years_eagle,
      "loc_pf_years_pss" = loc_pf_years_pss,
      "loc_all_days_my_day" = loc_all_days_my_day,
      "mean_prop_eagle" = mean_prop_eagle,
      "my_day" = my_day,
      # append for pss_prop_es_prop
      "pss_mat_prop_all" = pss_mat_prop_all
    )
  
  # extra stuff for plotting
  dat_out <- append(dat_out, list(
    "cum_pss_mat" = cum_pss_mat,
    "cum_pss_all" = cum_pss_all,
    "cum_eagle" = cum_eagle,
    "cum_curr_pss" = cum_curr_pss,
    "cum_pss" = cum_pss
  ))
  
  if (!is.null(prior_df_log)) {
    dat_out <- append(dat_out, list(
      "ps_m" = ps_m,
      "ps_s" = ps_s,
      "ps_alpha_log" = ps_alpha_log,
      "n_ps_m" = n_ps_m,
      "n_ps_s" = n_ps_s,
      "n_ps_alpha_log" = n_ps_alpha_log
    ))
  }
  
  if (!is.null(pss_sd)) {
    dat_out <- append(dat_out, list(
      "curr_pss_year_sd" = curr_pss_year_sd,
      "pss_year_sd" = pss_year_sd
    ))
  }
  
  if (!is.null(gsi_by_year)) {
    dat_out <- append(dat_out, list(
      "gsi_tbl" = gsi_tbl,
      "mean_gsi_vect" = mean_gsi_vect,
      "sd_gsi_vect" = sd_gsi_vect,
      "gsi_all_tbl" = gsi_all_tbl,
      "mean_gsi_vect_all" = mean_gsi_vect_all,
      "sd_gsi_vect_all" = sd_gsi_vect_all,
      "pss_mat_adj" = pss_mat_adj,
      "cum_pss_adj" = cum_pss_adj,
      "pss_mat_all_adj" = pss_mat_all_adj,
      "adj_curr_pss" = adj_curr_pss
    ))
  }
  
  return(dat_out)

}














