
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BICA model

<!-- badges: start -->
<!-- badges: end -->

The Bayesian Inseason Chinook Abundance (BICA) model was developed by
Aaron Lambert as his Master’s thesis project for the University of
Alaska Fairbanks CFOS; although he called the project “the Bayesian
Yukon River Canadian-origin Chinook Salmon Inseason Projection Model.”
As the name implied, the BICA model is designed to predict Chinook
abundance for the purpose of inseason fishery management of the Alaska
Department of Fish & Game (ADF&G). We developed the *bica* package to
run the BICA model suite using *CmdStan* and pre-compiled *Stan* code.
The benefits of *bica* package are a faster model run time and a more
streamlined process.

## Installation

You can install the development version of *bica* from
[GitHub](https://github.com/boppingshoe/bica) with:

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

my_year <- 2023
my_day <- my_day_func(Month = 8, Day = 9)
end_year <- my_year - 1

pf_hist <- readRDS(file.path(dir.data,"Yukon Canadian Chinook PF 29May24.RDS"))
can_hist <- readRDS(file.path(dir.data,"Canadian Chinook Passage RR 2Apr24.RDS"))
pss_hist <-
  readxl::read_xlsx(file.path(dir.data,"ADFG PSS Daily Reports/Yukon Escapement Daily 9Aug23.xlsx"), skip = 3)
eagle_hist <-
  readxl::read_xlsx(file.path(dir.data,"ADFG Eagle Daily Reports/Yukon Escapement Daily Eagle 9Aug23.xlsx"), skip = 3)

# gsi_by_year <- readRDS(file = file.path(dir.data,"GSI by year unadj 4Apr24.RDS")) # GSI data optional

pss_sd <- readRDS(file = file.path(dir.data,"PSS SD 1995_2021.RDS"))
prior_df_log <- read.csv(file = file.path(dir.data,"logistic curve parameters All Chinook 1995_2022.csv"))
prior_df_norm <- readRDS(file = file.path(dir.data,"normal curve parameters All Chinook 1995_2023.RDS"))
```

2)  Format the data sets into an object using `format_bica_data()`. The
    object will be used to run BICA model input:

``` r
bica_data <- format_bica_data(
  my_year, my_day, end_year,
  pf_hist, can_hist, pss_hist, eagle_hist,
  gsi_by_year = NULL,
  pss_sd, prior_df_log, prior_df_norm,
  start_day_pss = 148, start_year_pss = 1995
)
```

4)  Run BICA model, in this case, model version *pss_prop_es_prop*:

``` r
model_version <- "pss_prop_es_prop"
n_chains <- 3
n_iter <- 50
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
#> Chain 1 WARNING: There aren't enough warmup iterations to fit the 
#> Chain 1          three stages of adaptation as currently configured. 
#> Chain 1          Reducing each adaptation stage to 15%/75%/10% of 
#> Chain 1          the given number of warmup iterations: 
#> Chain 1            init_buffer = 3 
#> Chain 1            adapt_window = 20 
#> Chain 1            term_buffer = 2 
#> Chain 1 Iteration:  1 / 75 [  1%]  (Warmup) 
#> Chain 1 Iteration: 26 / 75 [ 34%]  (Sampling)
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/RtmpeOHj6x/model-43b0121c1c1.stan', line 229, column 6 to column 71)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/RtmpeOHj6x/model-43b0121c1c1.stan', line 229, column 6 to column 71)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/RtmpeOHj6x/model-43b0121c1c1.stan', line 229, column 6 to column 71)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/RtmpeOHj6x/model-43b0121c1c1.stan', line 229, column 6 to column 71)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1
#> Chain 2 WARNING: There aren't enough warmup iterations to fit the 
#> Chain 2          three stages of adaptation as currently configured. 
#> Chain 2          Reducing each adaptation stage to 15%/75%/10% of 
#> Chain 2          the given number of warmup iterations: 
#> Chain 2            init_buffer = 3 
#> Chain 2            adapt_window = 20 
#> Chain 2            term_buffer = 2 
#> Chain 2 Iteration:  1 / 75 [  1%]  (Warmup) 
#> Chain 2 Iteration: 26 / 75 [ 34%]  (Sampling)
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/RtmpeOHj6x/model-43b0121c1c1.stan', line 229, column 6 to column 71)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/RtmpeOHj6x/model-43b0121c1c1.stan', line 229, column 6 to column 71)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/RtmpeOHj6x/model-43b0121c1c1.stan', line 229, column 6 to column 71)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/RtmpeOHj6x/model-43b0121c1c1.stan', line 229, column 6 to column 71)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2
#> Chain 3 WARNING: There aren't enough warmup iterations to fit the 
#> Chain 3          three stages of adaptation as currently configured. 
#> Chain 3          Reducing each adaptation stage to 15%/75%/10% of 
#> Chain 3          the given number of warmup iterations: 
#> Chain 3            init_buffer = 3 
#> Chain 3            adapt_window = 20 
#> Chain 3            term_buffer = 2 
#> Chain 3 Iteration:  1 / 75 [  1%]  (Warmup) 
#> Chain 3 Iteration: 26 / 75 [ 34%]  (Sampling)
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/RtmpeOHj6x/model-43b0121c1c1.stan', line 229, column 6 to column 71)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/RtmpeOHj6x/model-43b0121c1c1.stan', line 229, column 6 to column 71)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/RtmpeOHj6x/model-43b0121c1c1.stan', line 229, column 6 to column 71)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/RtmpeOHj6x/model-43b0121c1c1.stan', line 229, column 6 to column 71)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3
#> Chain 1 Iteration: 75 / 75 [100%]  (Sampling) 
#> Chain 3 Iteration: 75 / 75 [100%]  (Sampling) 
#> Chain 1 finished in 0.6 seconds.
#> Chain 3 finished in 0.5 seconds.
#> Chain 2 Iteration: 75 / 75 [100%]  (Sampling) 
#> Chain 2 finished in 0.7 seconds.
#> 
#> All 3 chains finished successfully.
#> Mean chain execution time: 0.6 seconds.
#> Total execution time: 0.8 seconds.
#> Warning: 2 of 3 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 2 of 3 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.
```

5)  Summarize results:

``` r
print(bica_out$stanfit,
      pars = c("alpha", "beta", "sigma", "ln_run_size", "lp__"),
      probs = c(0.1, 0.5, 0.9))
#> Inference for Stan model: pss_prop_es_prop-202406200909-1-89453e.
#> 3 chains, each with iter=75; warmup=25; thin=1; 
#> post-warmup draws per chain=50, total post-warmup draws=150.
#> 
#>                mean se_mean      sd     10%     50%      90% n_eff Rhat
#> alpha       5510.60 1647.86 5947.94  695.76 3242.80 13644.71    13 1.11
#> beta           0.45    0.01    0.06    0.37    0.46     0.52    26 1.01
#> sigma          0.41    0.01    0.07    0.33    0.39     0.51    67 1.01
#> ln_run_size   10.06    0.09    0.36    9.87   10.06    10.23    16 1.14
#> lp__        6231.47    2.28   12.50 6230.86 6234.52  6236.06    30 1.07
#> 
#> Samples were drawn using NUTS(diag_e) at Thu Jun 20 9:09:06 AM 2024.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
```
