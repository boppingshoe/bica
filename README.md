
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

## Example

Once you have the data set up at a designated directory, you can load
the data files in the environment:

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

# gsi_by_year <- readRDS(file = file.path(dir.data,"GSI by year unadj 4Apr24.RDS")) # optional

pss_sd <- readRDS(file = file.path(dir.data,"PSS SD 1995_2021.RDS"))
prior_df_log <- read.csv(file = file.path(dir.data,"logistic curve parameters All Chinook 1995_2022.csv"))
prior_df_norm <- readRDS(file = file.path(dir.data,"normal curve parameters All Chinook 1995_2023.RDS"))
```

Compile the input object:

``` r
bica_data <- format_bica_data(
  my_year, my_day, end_year,
  pf_hist, can_hist, pss_hist, eagle_hist,
  gsi_by_year = NULL,
  pss_sd, prior_df_log, prior_df_norm,
  start_day_pss = 148, start_year_pss = 1995
)
```

Run and summarize results for the BICA model, in this case, model
version *PSS prop ES prop*:

``` r
model_version <- "pss_prop_es_prop"
n_chains <- 4
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
#>  - chain 4: mid, shape, sigma_logistic
#> 
#> To disable this message use options(cmdstanr_warn_inits = FALSE).
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 WARNING: There aren't enough warmup iterations to fit the 
#> Chain 1          three stages of adaptation as currently configured. 
#> Chain 1          Reducing each adaptation stage to 15%/75%/10% of 
#> Chain 1          the given number of warmup iterations: 
#> Chain 1            init_buffer = 3 
#> Chain 1            adapt_window = 20 
#> Chain 1            term_buffer = 2 
#> Chain 1 Iteration:  1 / 75 [  1%]  (Warmup)
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/Rtmpe20bYN/model-35a03bd16cc0.stan', line 229, column 6 to column 71)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/Rtmpe20bYN/model-35a03bd16cc0.stan', line 229, column 6 to column 71)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/Rtmpe20bYN/model-35a03bd16cc0.stan', line 229, column 6 to column 71)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/Rtmpe20bYN/model-35a03bd16cc0.stan', line 229, column 6 to column 71)
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
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/Rtmpe20bYN/model-35a03bd16cc0.stan', line 229, column 6 to column 71)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/Rtmpe20bYN/model-35a03bd16cc0.stan', line 229, column 6 to column 71)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/Rtmpe20bYN/model-35a03bd16cc0.stan', line 229, column 6 to column 71)
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
#> Chain 3 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/Rtmpe20bYN/model-35a03bd16cc0.stan', line 229, column 6 to column 71)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/Rtmpe20bYN/model-35a03bd16cc0.stan', line 229, column 6 to column 71)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/Rtmpe20bYN/model-35a03bd16cc0.stan', line 229, column 6 to column 71)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/Rtmpe20bYN/model-35a03bd16cc0.stan', line 229, column 6 to column 71)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3
#> Chain 4 WARNING: There aren't enough warmup iterations to fit the 
#> Chain 4          three stages of adaptation as currently configured. 
#> Chain 4          Reducing each adaptation stage to 15%/75%/10% of 
#> Chain 4          the given number of warmup iterations: 
#> Chain 4            init_buffer = 3 
#> Chain 4            adapt_window = 20 
#> Chain 4            term_buffer = 2 
#> Chain 4 Iteration:  1 / 75 [  1%]  (Warmup)
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/Rtmpe20bYN/model-35a03bd16cc0.stan', line 229, column 6 to column 71)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/Rtmpe20bYN/model-35a03bd16cc0.stan', line 229, column 6 to column 71)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in 'C:/Users/bhsu/AppData/Local/Temp/Rtmpe20bYN/model-35a03bd16cc0.stan', line 229, column 6 to column 71)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4
#> Chain 1 Iteration: 26 / 75 [ 34%]  (Sampling) 
#> Chain 2 Iteration: 26 / 75 [ 34%]  (Sampling) 
#> Chain 4 Iteration: 26 / 75 [ 34%]  (Sampling) 
#> Chain 2 Iteration: 75 / 75 [100%]  (Sampling) 
#> Chain 2 finished in 0.8 seconds.
#> Chain 3 Iteration: 75 / 75 [100%]  (Sampling) 
#> Chain 4 Iteration: 75 / 75 [100%]  (Sampling) 
#> Chain 3 finished in 0.9 seconds.
#> Chain 4 finished in 0.9 seconds.
#> Chain 1 Iteration: 75 / 75 [100%]  (Sampling) 
#> Chain 1 finished in 1.3 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 1.0 seconds.
#> Total execution time: 1.5 seconds.

bica_out$summary
#> # A tibble: 242 × 10
#>    variable          mean  median      sd     mad      q5     q95  rhat ess_bulk
#>    <chr>            <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <dbl>    <dbl>
#>  1 lp__           6.23e+3 6.23e+3 1.91e+0 1.74e+0 6.23e+3 6.24e+3  1.08     46.0
#>  2 alpha          4.98e+3 3.42e+3 4.72e+3 4.26e+3 1.72e+2 1.32e+4  1.25     14.2
#>  3 beta           4.66e-1 4.72e-1 4.21e-2 4.61e-2 3.96e-1 5.24e-1  1.13     28.2
#>  4 sigma          4.05e-1 3.96e-1 7.28e-2 7.74e-2 3.06e-1 5.36e-1  1.04    170. 
#>  5 mid            1.76e+2 1.76e+2 6.68e-2 6.38e-2 1.76e+2 1.76e+2  1.03    206. 
#>  6 shape          5.37e+0 5.37e+0 5.52e-2 5.22e-2 5.28e+0 5.45e+0  1.03    202. 
#>  7 sigma_logistic 6.48e-2 6.47e-2 9.28e-4 9.41e-4 6.33e-2 6.63e-2  1.02    169. 
#>  8 ln_run_size    1.01e+1 1.01e+1 1.42e-1 1.51e-1 9.84e+0 1.03e+1  1.05    164. 
#>  9 pred_pss[1]    1.08e+5 1.08e+5 7.16e+3 8.15e+3 9.75e+4 1.20e+5  1.05    140. 
#> 10 pred_pss[2]    9.81e+4 9.81e+4 6.39e+3 7.28e+3 8.83e+4 1.09e+5  1.04    144. 
#> # ℹ 232 more rows
#> # ℹ 1 more variable: ess_tail <dbl>
```
