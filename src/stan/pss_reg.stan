// Yukon River Canadian Origin Chinook Inseason Forecast
// Version: PSSreg
// Notes:

// Components:
//  1) Pre-season forecast  (prior)
//  2) EOS-Can ~ PSS in normal space

data {

  // This years preseason forecast
  real<lower=0> pf;
  real<lower=0> pf_sigma;
  int<lower=0> n_years_pf;

  // EOS Canadian counts by year up to myyear -1
  int<lower=0> n_total_eos;
  vector<lower=0> [n_total_eos]total_eos;

  // PSS days up to myDay
  int<lower=0> n_day_pss;
  array[n_day_pss] int<lower=0> day_pss;

  // Historic PSS years up to myYear -1
  int<lower=0> n_year_pss;
  array[n_year_pss] int<lower=0> year_pss;

  // Matrix of historic counts by days & years
  // int <lower=0>n_hist_counts;
  matrix<lower=0>[n_day_pss, n_year_pss] pss_mat;

  // Current year counts up to myDay
  int<lower=0> n_curr_pss;
  array[n_curr_pss] real<lower=0> curr_pss;

  // Pointer vector for pf years in PSS
  array[n_years_pf] int<lower=0> loc_pf_years_pss;

}


parameters {

  real<lower=0> alpha;
  real<lower=0> beta;
  real<lower=0> sigma;
  real ln_run_size;

}


transformed parameters {

  //Empty vector for cum sum of counts for each year to myyear -1
  array[n_year_pss] real cum_hist_pss;

  // Empty vector for predicted PSS counts in transformed parameters
  vector[n_year_pss] pred_pss;

  // Variable for cummulative myYear PSS passage
  real cum_current_pss;
  real curr_pred_pss;
  real sigma_pred_pss;

  // Loop to get cumulative counts
  for (i in 1:n_year_pss) {
    cum_hist_pss[i] = sum(pss_mat[,i]);
  }

 //  Calculate pred_pss from aplha, beta and cum_hist_pss for model section
 for (i in 1:n_year_pss) {
   pred_pss[i] = alpha + beta * cum_hist_pss[i];
 }

 // Sigma calculation for update
 // Only years used in pf comparison are used here
 sigma_pred_pss = sd(log(total_eos[loc_pf_years_pss] ./ pred_pss[loc_pf_years_pss]));

 // Sum curr_pss
 cum_current_pss = sum(curr_pss);

 curr_pred_pss = alpha + beta * cum_current_pss;

}


model {

  //prior
  alpha ~ normal(0,1e15);
  beta ~ normal(0,1e15);
  sigma ~ normal(0,10);

  // Preseason Forcast
  ln_run_size ~ normal(pf, pf_sigma);

  //Likelihood Function
  for(i in 1:n_year_pss){
    // log(total_eos[i]) ~ normal(log(pred_pss), sigma);
    // target += normal_lpdf(total_eos|pred_pss, sigma);
    // total_eos[i] ~ normal(pred_pss[i], sigma);
    // Change here
    log(total_eos[i]) ~ normal(log(pred_pss[i]), sigma);
  }

  // Update for the posterior
  target += normal_lpdf(ln_run_size | log(curr_pred_pss), sigma_pred_pss);

}

generated quantities {

  real ln_prior_pf;
  real prior_pf;
  real ln_post_curr_pred_pss;
  real post_curr_pred_pss;

  // Variable for posterior abundance
  real run_size;

  // Predicted forecast
  ln_prior_pf = normal_rng(pf, pf_sigma);

  // Expenential of log predicted preseason forecast
  prior_pf = exp(ln_prior_pf);

  // Predicted PSS passage
  ln_post_curr_pred_pss = normal_rng(log(curr_pred_pss), sigma_pred_pss);

  post_curr_pred_pss = exp(ln_post_curr_pred_pss);

  run_size = exp(ln_run_size);

}
