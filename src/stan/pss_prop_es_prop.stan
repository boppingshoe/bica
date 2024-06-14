
// Yukon Chinook Inseason Forecast Version 6.0.2

// Notes: 
//  Uses logistic fit to all years to generate the expecte proportion
//  on day D

//Components:
//  1) Pre-season forecast (Prior)
//  2) PSS logistic fit to proporitons  
//  3) Eagle simple proportion estimator


data {
  
  // This years Preseason forecast
  real<lower=0> pf;
  real<lower=0> pf_sigma;
  int <lower=0> n_years_pf;

  // EOS Canadian abundance by year
  int<lower=0> n_total_eos;
  vector<lower=0>[n_total_eos] total_eos;
  
  // PSS days up to my_day
  int n_day_pss;
  vector<lower=0>[n_day_pss] day_pss;
  int<lower=0> n_day_pss_all;
  vector<lower=0>[n_day_pss_all] day_pss_all;
  
  // Historic PSS years 
  int<lower=0> n_year_pss;
  array[n_year_pss] int<lower=0> year_pss;
  
  // Matrix of PSS passage by day and year
  matrix<lower=0> [n_day_pss, n_year_pss] pss_mat;
  
  // Matrix of historic PSS passage for complete historic years
  matrix <lower=0> [n_day_pss_all, n_year_pss] pss_mat_all;
  matrix <lower=0> [n_day_pss_all, n_year_pss] pss_mat_prop_all;
  // Matrix of cumulative historic PSS passage by days and years
  // matrix <lower=0> [n_day_pss_all, n_year_pss] cum_pss_mat_all;
  
  // Current year counts up to my_day
  int<lower=0> n_curr_pss;
  array[n_curr_pss] real<lower=0> curr_pss;
  
  // Day the projection is made on
  real my_day;

  // Matrix of historic Eagel passage by days & years
  int<lower=0> n_year_eagle;
  matrix<lower=0> [n_day_pss, n_year_eagle] eagle_mat;
  
  // Current year Eagle passage up to my_day
  int<lower=0> n_curr_eagle;
  vector<lower=0>[n_curr_eagle] curr_eagle;
  
  // Pointer vectors for correct years
  array[n_years_pf] int<lower=0> loc_pf_years_pss;
  array[n_year_eagle] int<lower=0> loc_eagle_years;
  array[n_years_pf] int<lower=0> loc_pf_years_eagle;
  
  // mean eagle prpoporiton of total run size on my_day
  real mean_prop_eagle;
  
}


transformed data {
  
  // Vector to hold sum of counts for each historic year up to my_day
  vector [n_year_pss] cum_hist_pss;
  
  // Vector for the sum of each historic year to the EOS
  vector [n_year_pss] cum_hist_pss_all;
  
  // Sum of the current year up to my_day
  real cum_current_pss;
  
  // matrix [n_day_pss_all, n_year_pss]pss_mat_prop_logit;
  
  // Loop to get cumulative counts
  for (i in 1:n_year_pss) {
    cum_hist_pss_all[i] = sum(pss_mat_all[,i]);
  }
  
  // Loop to get cumulative historic counts
  for (i in 1:n_year_pss) {
    cum_hist_pss[i] = sum(pss_mat[,i]);
  }
  // Sum up the current year PSS passage to my_day
  cum_current_pss = sum(curr_pss);
  
}
  

parameters {
   
  // PSS regression parameters
  real <lower=0> alpha;
  real <lower=0> beta;
  real <lower=0> sigma;
  
  // logistic curve paramaters
  real  <lower=166, upper=182> mid;
  real  <lower=1,upper=12> shape;
  real  <lower=0> sigma_logistic;
  
  // Log run size
  real ln_run_size;
  
}

transformed parameters{
  
  // Historic PSS prediction
  vector [n_year_pss] pred_pss;
  
  // Current years PSS prediction
  real curr_pred_pss;
  
  // Empirical sd for PSS for updating
  real sigma_pred_pss;
  
  // Expected proportion for my_day (day of projection)
  real expected_prop;
  
  // Historic estimates of total PSS passage from expected proportion
  vector [n_year_pss] est_pred_pss;
  
  // This years (myYear) estimate for total PSS passage from expected proportion
  real curr_est_pred_pss;
  
  // Predicted PSS historically for just years used for updating comparison
  // used to calc empircal sd
  vector [n_year_pss] pred_pss_hist;
  
  // PSS predicted proportions for fitting logistic
  vector [n_day_pss_all] ps_prop_pred;
  
  // Cumulative Eagle counts up to my_day
  vector [n_year_eagle] cum_hist_eagle;
  
  // Predicted Eagle passage
  vector [n_year_eagle] pred_eagle;
  
  // This years Eagle passsage up to my_day
  real cum_current_eagle;
  
  // This years predicted run size from Eagle
  real curr_pred_eagle;
  
  // Empirical sd for updating with Eagle est
  real sigma_pred_eagle;
  
  // Fit logistic to obs PSS proportions
  for(d in 1:n_day_pss_all) {
    ps_prop_pred[d] =  (1/(1 + exp((-(day_pss_all[d] - mid)) / shape)));
  }

  // Regress cumulative total PSS against the EOS Can run size
  for (i in 1:n_year_pss) {
    pred_pss[i] = alpha + beta * cum_hist_pss_all[i];
  }
  
  // Calculate expected proportion for day of projection
  expected_prop = 1 / (1 + exp((-(my_day - mid)) / shape));

  // Estimated historic years by proportions
  for(y in 1:n_year_pss) {
    est_pred_pss[y] = cum_hist_pss[y] /expected_prop;
  }
  
  // Using the historic est total PSS passage to calc empirical variance
  pred_pss_hist = alpha + beta * est_pred_pss;
  
  // Empirical SD for update
  sigma_pred_pss = sd(log(total_eos[loc_pf_years_pss]) - log(pred_pss_hist[loc_pf_years_pss]));
  
  // This years est total PSS passage
  curr_est_pred_pss = cum_current_pss / expected_prop;
 
  // This years PSS est of run size
  curr_pred_pss = alpha + beta * curr_est_pred_pss;
  
  // Loop to get cumulative Eagle counts
  for (i in 1:n_year_eagle) {
    cum_hist_eagle[i] = sum(eagle_mat[,i]);
  }
 
  // Summ current year Eagle passage
  cum_current_eagle = sum(curr_eagle);
  
  // Eagle Component using observed propotions 
  // Only used when fish are observed at eagle in the projection year
  if (mean_prop_eagle > 0 && cum_current_eagle > 0) {
    
  // This years Eagle est of run size
  curr_pred_eagle = (cum_current_eagle+1)/mean_prop_eagle;
  
  // Historic est of run size
  pred_eagle =  (cum_hist_eagle+1)/mean_prop_eagle;
 
  // Empirical SD for Eagle update to run size
  sigma_pred_eagle = sd(log(total_eos[loc_pf_years_pss] ./ pred_eagle[loc_pf_years_eagle]));
  
  } // if
  
}


model {
  
  // Prior on SD for logistic fitting
  sigma_logistic ~ normal(0, 100);
  
  // Regression priors
  alpha ~ normal(0, 1e15);
  beta ~ normal(0, 1e15);
  sigma ~ normal(0, 5);
  
  // Run size drawn form preseason forcast prior dist
  ln_run_size ~ normal(pf, pf_sigma);
  
  // Logistic curve fitting 
  for(d in 1:n_day_pss_all) {
    for(y in 1:n_year_pss) {
      pss_mat_prop_all[d, y] ~ normal(ps_prop_pred[d], sigma_logistic);
    }
  }
    
  // Total PSS vs Run size regression fitting
  for(i in 1:n_year_pss) {
    log(total_eos[i]) ~ normal(log(pred_pss[i]), sigma);
  }

  // Update pf with PSS est
  target += normal_lpdf(ln_run_size | log(curr_pred_pss), sigma_pred_pss);
  
   // Update pf with Eagle est
  if (mean_prop_eagle > 0 && cum_current_eagle > 0) {
    target += normal_lpdf(ln_run_size | log(curr_pred_eagle), sigma_pred_eagle);
  }

}


generated quantities {
  
  // Generated prediction intervals for analysis
  real ln_prior_pf;
  
  real prior_pf;
  
  real ln_post_curr_pred_pss;
  
  real post_curr_pred_pss;
  
  real ln_post_curr_pred_eagle;
  
  real post_curr_pred_eagle;
  
  real run_size;
  
  ln_prior_pf = normal_rng(pf, pf_sigma);
  
  prior_pf = exp(ln_prior_pf);
  
  ln_post_curr_pred_pss = normal_rng(log(curr_pred_pss), sigma_pred_pss);
  
  post_curr_pred_pss = exp(ln_post_curr_pred_pss);
  
  if (mean_prop_eagle > 0 && cum_current_eagle > 0) {
    ln_post_curr_pred_eagle = normal_rng(log(curr_pred_eagle), sigma_pred_eagle);
    post_curr_pred_eagle = exp(ln_post_curr_pred_eagle);
  }
  
  // Bring it back to reality
  run_size = exp(ln_run_size);
}
