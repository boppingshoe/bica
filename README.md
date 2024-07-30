
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BICA model

<!-- badges: start -->
<!-- badges: end -->

The Bayesian Inseason Chinook Abundance (BICA) model was developed by
Aaron Lambert as his Master’s thesis project for the University of
Alaska Fairbanks CFOS. The project was originally called the “Bayesian
Yukon River Canadian-origin Chinook Salmon Inseason Projection Model”
and is designed to predict Chinook abundance for the purpose of inseason
fishery management of the Alaska Department of Fish & Game (ADF&G). We
developed the *bica* package to run the BICA model suite using *CmdStan*
and pre-compiled *Stan* code. The benefits of *bica* package are a
faster model run time and a more streamlined process.

## Installation

To install *bica*:

1)  Install the R package *CmdStanR* using R commend
    `install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))`.
2)  Install R package *instantiate* using R commend
    `install.packages("instantiate")`
3)  Install *bica* from [GitHub](https://github.com/boppingshoe/bica)
    with:

``` r
# install.packages("devtools")
devtools::install_github("boppingshoe/bica")
```

## Workflow

1)  Have the data set up at a designated directory, then load the data
    files in the *R* environment:

``` r
library(bica)

# path for data files
# see Aaron's instructions on types of data needed
dir.data <- file.path(wd, "data")

my_year <- 2024
my_day <- my_day_func(Month = 7, Day = 28)
end_year <- my_year - 1

pf_hist <- readRDS(file.path(dir.data, "Yukon Canadian Chinook PF 29May24.RDS"))
can_hist <- readRDS(file.path(dir.data, "Canadian Chinook Passage RR 2Apr24.RDS"))
pss_hist <-
  readxl::read_xlsx(file.path(dir.data, "ADFG PSS Daily Reports/Yukon Escapement Daily_07292024.xlsx"), skip = 3)
eagle_hist <-
  readxl::read_xlsx(file.path(dir.data, "ADFG Eagle Daily Reports/Yukon Escapement Daily Eagle_07292024.xlsx"), skip = 3)
prior_df_norm <- readRDS(file = file.path(dir.data,"normal curve parameters All Chinook 1995_2023.RDS"))
```

2)  Format the data sets into an object using `format_bica_data()`. The
    object will be used to run BICA model input:

``` r
bica_data <- bica::format_bica_data(
  my_year, my_day, end_year,
  pf_hist, can_hist, pss_hist, eagle_hist,
  prior_df_norm, prior_df_log = NULL,
  gsi_by_year = NULL, pss_sd = NULL,
  start_day_pss = 148, start_year_pss = 1995
)
```

4)  Run BICA model, in this case, model version *pss_prop_es_prop*:

``` r
model_version <- "pss_prop_es_prop"
n_chains <- 3
n_iter <- 30
n_thin <- 1

bica_out <- bica::run_bica_model(
    bica_data, model_version,
    n_chains, n_iter, n_thin
)
#> Init values were only set for a subset of parameters. 
#> Missing init values for the following parameters:
#>  - chain 1: mid, shape, sigma_logistic
#>  - chain 2: mid, shape, sigma_logistic
#>  - chain 3: mid, shape, sigma_logistic
#> 
#> To disable this message use options(cmdstanr_warn_inits = FALSE).
#> Running MCMC with 3 parallel chains...
#> 
#> Chain 1 WARNING: No variance estimation is 
#> Chain 1          performed for num_warmup < 20 
#> Chain 1 Iteration:  1 / 45 [  2%]  (Warmup) 
#> Chain 1 Iteration: 16 / 45 [ 35%]  (Sampling)
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/RtmpW8Jyjb/model-5bb443ca59cb.stan', line 229, column 6 to column 71)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1
#> Chain 2 WARNING: No variance estimation is 
#> Chain 2          performed for num_warmup < 20 
#> Chain 2 Iteration:  1 / 45 [  2%]  (Warmup) 
#> Chain 2 Iteration: 16 / 45 [ 35%]  (Sampling)
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/RtmpW8Jyjb/model-5bb443ca59cb.stan', line 229, column 6 to column 71)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/RtmpW8Jyjb/model-5bb443ca59cb.stan', line 229, column 6 to column 71)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/RtmpW8Jyjb/model-5bb443ca59cb.stan', line 229, column 6 to column 71)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2
#> Chain 3 WARNING: No variance estimation is 
#> Chain 3          performed for num_warmup < 20 
#> Chain 3 Iteration:  1 / 45 [  2%]  (Warmup) 
#> Chain 3 Iteration: 16 / 45 [ 35%]  (Sampling) 
#> Chain 2 Iteration: 45 / 45 [100%]  (Sampling) 
#> Chain 2 finished in 0.2 seconds.
#> Chain 3 Iteration: 45 / 45 [100%]  (Sampling) 
#> Chain 3 finished in 0.1 seconds.
#> Chain 1 Iteration: 45 / 45 [100%]  (Sampling) 
#> Chain 1 finished in 0.4 seconds.
#> 
#> All 3 chains finished successfully.
#> Mean chain execution time: 0.2 seconds.
#> Total execution time: 0.5 seconds.
#> Warning: 1 of 3 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 1 of 3 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.
```

5)  Summarize results:

``` r
print(bica_out$stanfit,
      pars = c("alpha", "beta", "sigma", "ln_run_size", "lp__"),
      probs = c(0.1, 0.5, 0.9))
#> Inference for Stan model: pss_prop_es_prop-202407301548-1-8759bb.
#> 3 chains, each with iter=45; warmup=15; thin=1; 
#> post-warmup draws per chain=30, total post-warmup draws=90.
#> 
#>                mean se_mean      sd     10%     50%     90% n_eff Rhat
#> alpha       4131.72 1192.02 4159.79  634.31 2593.06 8901.38    12 1.20
#> beta           0.46    0.01    0.04    0.41    0.46    0.50    32 1.06
#> sigma          0.42    0.02    0.09    0.33    0.41    0.49    33 1.11
#> ln_run_size   10.03    0.16    0.45    9.81   10.11   10.37     8 1.43
#> lp__        6430.15    4.75   14.32 6421.82 6434.63 6436.81     9 1.33
#> 
#> Samples were drawn using NUTS(diag_e) at Tue Jul 30 3:48:09 PM 2024.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
```
