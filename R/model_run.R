
# Yukon River INSEASON FORECAST - Integrated Bayesian Inseason Model

#' Format data for BICA model
#'
#' @param bica_data Input data as a list (from format_bica_data())
#' @param model_version Character string. Model version to run.
#' @param n_chains Number of MCMC chains
#' @param n_iter Number of MCMC iterations
#' @param n_thin Amount of thinning for MCMC chains
#'
#' @return
#' Results and summaries from BICA model run
#' 
#' @export
#'
#' @examples
#' \donotrun{
#' bica_out <- bica::run_bica_model(bica_data, model_version = "pss_prop_es_prop", n_chains = 4, n_iter = 5000, n_thin = 5)
#'
#' bica_out$summary
#' }
#'
run_bica_model <- function(
    bica_data, model_version,
    n_chains, n_iter, n_thin
) {

  if (!model_version %in% c("pss_reg", "pss_normal_es_prop", "pss_prop_es_prop")) {
    stop('Invalid model version. Must be either "pss_reg", "pss_normal_es_prop", or "pss_prop_es_prop".')
  }

  # subset data list ----
  dat_in <-
    with(bica_data, list(
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
      "loc_pf_years_pss" = loc_pf_years_pss
    ))

  if (model_version != "pss_reg") {
    dat_in <-
      with(bica_data, append(dat_in,
                             list(
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
                               "mean_prop_eagle" = mean_prop_eagle,
                               "my_day" = my_day
                             )))
  }

  if (model_version == "pss_normal_es_prop") {
    dat_in <-
      with(bica_data,
           append(dat_in, list("loc_all_days_my_day" = loc_all_days_my_day)))
  } else if (model_version == "pss_prop_es_prop") {
    dat_in <-
      with(bica_data,
           append(dat_in, list("pss_mat_prop_all" = pss_mat_prop_all)))
  }
  
  # Stan Model Call ######################################
  if (model_version == "pss_prop_es_prop") {
    inits <- function(...) {
      list(
        alpha = runif(1, 500, 1500),
        beta = runif(1, 0.3, 0.5),
        sigma = runif(1, 0.1, 0.5),
        # mid = runif(1, 150, 200), # customization seems to be worse off than default random
        # shape = runif(1, 3, 10),
        # sigma_logistic = runif(1, 0, 0.1),
        ln_run_size = runif(1, 5, 15)
      )
    }
  } else if (model_version == "pss_normal_es_prop") {
    inits <- function(...) {
      list(
        ps_alpha_curr = runif(1, 10e4, 11e4),
        ps_mu_curr = runif(1, 174, 176),
        ps_sd_curr = runif(1, 5, 7),
        sigma = runif(1, 2, 3),
        ps_alpha_hist = runif(dat_in$n_year_pss, 10e4, 11e4),
        ps_mu_hist = runif(dat_in$n_year_pss, 174, 176),
        ps_sd_hist = runif(dat_in$n_year_pss, 5, 7),
        sigma_hist = runif(dat_in$n_year_pss, 2, 3),
        alpha = runif(1, 177, 178),
        beta = runif(1, 0, 0.5),
        sigma_reg = runif(1, 2.5, 3),
        ln_run_size = runif(1, 5, 15)
      )
    }
  } else if(model_version == "pss_reg") {
    inits <- function(...) {
      list(
        alpha = runif(1, 150000, 200000),
        beta = runif(1, 0, 0.5),
        sigma = runif(1, 0, 2),
        ln_run_size = runif(1, 5, 15)
      )
    }
  }

  # initial values in stan model as a list
  inits_ll <- lapply(seq.int(n_chains), inits)

  model <- instantiate::stan_package_model(
    name = model_version,
    package = "bica"
  )

  fit <- model$sample(
    data = dat_in,
    init = inits_ll,
    chains = n_chains,
    parallel_chains = n_chains,
    iter_warmup = n_iter / 2,
    iter_sampling = n_iter,
    thin = n_thin,
    max_treedepth = 10,
    adapt_delta = 0.95,
    save_cmdstan_config = TRUE
  )

  # Extract parameter estimates
  
  # RStan fit
  # stanfit <- rstan::read_stan_csv(fit$output_files())
  # pars <- rstan::extract(stanfit)
  # divergent <- rstan::get_divergent_iterations(stanfit)
  # div_trans <- mean(divergent)
  
  # CmdStanR fit
  fit_summary <- fit$summary()
  pars <- fit$draws(format = "df") # trace history
  diagnostics <- fit$diagnostic_summary()

  out <- list(
    # "fit" = fit, # might take forever if included
    "summary" = fit_summary,
    "pars" = pars,
    "diagnostics" = diagnostics,
    "version" = model_version
  )

  return(out)

}
















