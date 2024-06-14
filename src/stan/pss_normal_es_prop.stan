// Yukon King Inseason Forecast Version 4.0
// Notes: fitting a normal curve to total PSS passage to predict EOS Can-origin Chinook abundance

// Components:
//  1) Pre-season forecast (Prior)
//  2) PSS Normal Curvve fitting to arival dist
//  3) EOS PSS regression

// Next to address:
//  Sigma pred needs to be reflective of curve fitting, not regression with actual counts

data {

  // This years Preseason forecast
  real<lower=0> pf;
  real<lower=0> pf_sigma;
  int<lower=0> n_years_pf;

  // _eos Canadian counts by year up to myyear -1
  int<lower=0> n_total_eos;
  vector<lower=0>[n_total_eos] total_eos;

  // PSS days up to my_day
  int n_day_pss;
  vector<lower=0>[n_day_pss] day_pss;

  // Historic PSS years up to myYear -1
  int<lower=0> n_year_pss;
  array[n_year_pss] int<lower=0> year_pss;

  // All the days in PSS season
  int<lower=0> n_day_pss_all;
  vector<lower=0>[n_day_pss_all] day_pss_all;

  // Matrix of historic counts by days & years
  // int<lower=0> n_hist_counts;
  matrix<lower=0>[n_day_pss, n_year_pss] pss_mat;

  // Matrix of historic PSS passage for complete historic years
  matrix<lower=0> [n_day_pss_all, n_year_pss] pss_mat_all;

  // Current year counts up to my_day
  int<lower=0> n_curr_pss;
  array[n_curr_pss] real<lower=0> curr_pss;

  // Estimated Mean, sd, and alphas for normal curve
  int<lower=0> n_ps_mu;
  int<lower=0> n_ps_sd;
  int<lower=0> n_ps_alpha_norm;

  array[n_ps_mu] real<lower=0> ps_mu;
  array[n_ps_sd] real<lower=0> ps_sd;
  array[n_ps_alpha_norm] real<lower=0> ps_alpha_norm;

  // Historic PSS years excluding myYear
  int<lower=0> n_year_eagle;

  // Matrix of historic counts by days & years
  matrix<lower=0>[n_day_pss, n_year_eagle] eagle_mat;

  // Current year counts up to my_day
  int<lower=0> n_curr_eagle;
  vector<lower=0>[n_curr_eagle] curr_eagle;

  // Location vector of integers
  array[n_year_eagle] int<lower=0> loc_eagle_years;
  array[n_years_pf] int<lower=0> loc_pf_years_eagle;
  array[n_years_pf] int<lower=0> loc_pf_years_pss;
  int<lower=0> loc_all_days_my_day;

  // Propotion of observed fish at Eagle
  real mean_prop_eagle;

  int<lower=0> my_day;

}


parameters {

  // Normal curve paramaters for current year
  real <lower=10e3,upper=4e5> ps_alpha_curr;
  // real <lower=0> ps_alpha_curr;
  real <lower=165,upper=181> ps_mu_curr;
  // real <lower=0> ps_mu_curr;
  real <lower=1,upper=14> ps_sd_curr;
  // real <lower=0> ps_sd_curr;
  real <lower=0> sigma;

  // Normal curve parameters for histric years
  array[n_year_pss] real<lower=10e3,upper=4e5> ps_alpha_hist;
  // real <lower=0> ps_alpha_hist[n_year_pss];
  array[n_year_pss] real<lower=165,upper=181> ps_mu_hist;
  // real <lower=0> ps_mu_hist[n_year_pss];
  array[n_year_pss] real<lower=1,upper=14> ps_sd_hist;
  // real <lower=0> ps_sd_hist[n_year_pss];
  array[n_year_pss] real<lower=0> sigma_hist;

  // Regression parameters
  real <lower=0> alpha;
  real <lower=0> beta;
  real <lower=0> sigma_reg;

  real ln_run_size;

}

transformed parameters {

  // Vector for holding this years daily predicted passage
  vector [n_day_pss_all] ps_pred_curr;

  // Vector for fitting to all observed days
  vector [n_day_pss] ps_pred_curr_obs;

  // Matrix for fitting curve to complete historic passage/days
  matrix [n_day_pss_all, n_year_pss] ps_pred_hist;

  // Matrix for fitting curve to historic years up to my_day
  matrix [n_day_pss, n_year_pss] ps_pred_hist_obs;

  // Vector to hold cumulative sums for historic passage up to my_day
  // Empty vector for cum sum of counts for each year to myyear -1
  vector [n_year_pss] cum_hist_pss;

  // Historic cumulative curve predictions to add to cum_hist_pss

  vector [n_year_pss] cum_predicted_hist_pss;

  // Variable for cummulative myYear PSS passage
  real cum_current_pss;

  // Empty vector for predicted PSS counts in transformed parameters
  vector [n_year_pss] pred_pss;

  // Vector of cumulative complete PSS passage up to myYear-1
  vector [n_year_pss] cum_hist_pss_all;

  // PSS prediction from current year
  real curr_pred_pss;

  // PSS historic prediction for emiricap sd for update
  vector [n_year_pss] pred_pss_hist;

  // Empirical sd for update
  real sigma_pred_pss;

  real sigma_pred_eagle;

  real cum_current_eagle;

  real curr_pred_eagle;

  //Empty vector for cum sum of counts for each year excluding myYear
  vector [n_year_eagle] cum_hist_eagle;

  // Empty vector for predicted PSS counts in transformed parameters
  vector [n_year_eagle] pred_eagle;

  // Curve fit to current season/year of interest
  ps_pred_curr[1:n_day_pss_all] = ps_alpha_curr * (1/(ps_sd_curr*sqrt(2*pi())))*exp(-0.5*square((day_pss_all - ps_mu_curr)/ps_sd_curr));


  // Curve fit to days up to my_day for the current year of interest
  ps_pred_curr_obs[1:n_day_pss] = ps_alpha_curr * (1/(ps_sd_curr*sqrt(2*pi())))*exp(-0.5*square((day_pss - ps_mu_curr)/ps_sd_curr));

  // Curves fit to complete and partial historic passage by year
  for(y in 1:n_year_pss) {
    for(d in 1:n_day_pss_all) {
      ps_pred_hist[d,y] = ps_alpha_hist[y] * (1/(ps_sd_hist[y]*sqrt(2*pi())))*exp(-0.5*square((day_pss_all[d] - ps_mu_hist[y])/ps_sd_hist[y]));
    }

    for(d in 1:n_day_pss) {
      ps_pred_hist_obs[d,y] = ps_alpha_hist[y] * (1/(ps_sd_hist[y]*sqrt(2*pi())))*exp(-0.5*square((day_pss[d] - ps_mu_hist[y])/ps_sd_hist[y]));
    }
  }

   // Sum up prediction from Normal curve fitting for days my_day +1 to 252
   // Used to calculate empirical sd in update

    for( i in 1:n_year_pss) {
      cum_hist_pss[i] = sum(pss_mat[,i]);
    }

    cum_predicted_hist_pss[1:n_year_pss] = cum_hist_pss + sum(ps_pred_hist[(loc_all_days_my_day + 1):n_day_pss_all,]);


    // Sum the observed and current year estimated passage to end of season
    cum_current_pss = sum(curr_pss)+sum(ps_pred_curr[(loc_all_days_my_day + 1):n_day_pss_all]);

    // Sum up complete PSS histoic days for use in regression
     for( i in 1:n_year_pss) {
      cum_hist_pss_all[i] = sum(pss_mat_all[,i]);
    }

    //  Calculate pred_pss from aplha, beta and cum_hist_pss for model section
    // for (i in 1:n_year_pss){
    //   pred_pss[i] = alpha + beta * cum_hist_pss_all[i];}
    pred_pss = alpha + beta * cum_hist_pss_all;

    // Historic prediction
    pred_pss_hist = alpha + beta * cum_predicted_hist_pss;


    // Empirical SD for update
    // sigma_pred = sd(log(total_eos[loc_pf_years_pss] ./ pred_pss_hist[loc_pf_years_pss]));
    sigma_pred_pss = sd(log(total_eos[loc_pf_years_pss]) - log(pred_pss_hist[loc_pf_years_pss]));

    // Get current years pss prediction for update
    curr_pred_pss = alpha + beta * cum_current_pss;

    // Summ current year Eagle passage
    cum_current_eagle = sum(curr_eagle);

    // Eagle Component using observed propotions
    if(mean_prop_eagle > 0 && cum_current_eagle > 0) {
        // Loop to get cumulative Eagle counts
    for (i in 1:n_year_eagle) {
      cum_hist_eagle[i] = sum(eagle_mat[,i]);
    }

    // Summ current year Eagle passage
    // cum_current_eagle = sum(curr_eagle);

    curr_pred_eagle = (cum_current_eagle + 1e-3) / mean_prop_eagle;

    pred_eagle =  (cum_hist_eagle + 1e-3) / mean_prop_eagle;

  // Empirical SD for update
  sigma_pred_eagle = sd(log(total_eos[loc_pf_years_pss] ./ pred_eagle[loc_pf_years_eagle]));

  } // end if statement

}


model {

  // Arival Dist Priors
  ps_alpha_curr ~ normal(mean(ps_alpha_norm), sd(ps_alpha_norm)) ;
  // ps_alpha_curr ~ uniform(45e3,5e5);
  ps_mu_curr ~ normal(mean(ps_mu), sd(ps_mu));
  // ps_mu_curr ~ uniform(172,181);
  ps_sd_curr ~ normal(mean(ps_sd), sd(ps_sd));
  sigma ~ normal(0, 5);
  // sigma ~ uniform(0,3);

  ps_alpha_hist ~ normal(mean(ps_alpha_norm), sd(ps_alpha_norm)) ;
  // ps_alpha_curr ~ uniform(45e3,5e5);
  ps_mu_hist ~ normal(mean(ps_mu), sd(ps_mu));
  // ps_mu_hist ~ uniform(172,181);
  ps_sd_hist ~ normal(mean(ps_sd), sd(ps_sd));
  sigma_hist ~ normal(0, 5);
  // sigma_hist ~ uniform(0, 3);

  // Regression priors
  alpha ~ normal(0, 1e15);
  beta ~ normal(0, 1e15);
  sigma_reg ~ normal(0, 10);

  // Preseason Forcast likelihood
  ln_run_size ~ normal(pf, pf_sigma);

  // Arrival dist curve fitting likelihood
    for(d in 1:n_day_pss) {
      if(curr_pss[d] > 0) {
        log(curr_pss[d]) ~ normal(log(ps_pred_curr_obs[d]), sigma);
      }

      for(y in 1:n_year_pss) {
        if(pss_mat[d,y] > 0) {
          log(pss_mat[d,y]) ~ normal(log(ps_pred_hist_obs[d,y]), sigma_hist[y]);
        }
      }
    }

  // Regression likelihood
  log(total_eos[1:n_year_pss]) ~ normal(log(pred_pss[1:n_year_pss]), sigma_reg);

  // Update run size projection with PSS projection
  target += normal_lpdf(ln_run_size | log(curr_pred_pss), sigma_pred_pss);

  // Update likelihood for the posterior prediction with the eagle projection
  if(mean_prop_eagle > 0 && cum_current_eagle > 0) {
    target += normal_lpdf(ln_run_size | log(curr_pred_eagle), sigma_pred_eagle);
  }

}


generated quantities {

  real ln_prior_pf;

  real prior_pf;

  real ln_post_curr_pred_pss;

  real post_curr_pred_pss;

  real ln_post_curr_pred_eagle;

  real post_curr_pred_eagle;

  // Projection
  real run_size;

  // Predicted forecast
  ln_prior_pf = normal_rng(pf, pf_sigma);

  // Expenential of log predicted preseason forecast
  prior_pf = exp(ln_prior_pf);

  // Predicted PSS passage
  ln_post_curr_pred_pss = normal_rng(log(curr_pred_pss), sigma_pred_pss);

  post_curr_pred_pss = exp(ln_post_curr_pred_pss);

  if(mean_prop_eagle > 0 && cum_current_eagle >0){
  ln_post_curr_pred_eagle = normal_rng(log(curr_pred_eagle), sigma_pred_eagle);

  post_curr_pred_eagle = exp(ln_post_curr_pred_eagle);}

  // Exponentiate the projection
  run_size = exp(ln_run_size);

}
