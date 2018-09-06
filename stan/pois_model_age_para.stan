data {
  int N_age;
  int N;
  int N_basis;
  matrix[N, N_age] pop_data;
  int vote[N];
  int electorate[N];
  matrix[N_age, 3] age_X;
  

  matrix[N_age, N_basis] age_basis;
}

transformed data {
  matrix[N, N_age] log_pop_data;
  log_pop_data  = log(pop_data);
}

parameters {
  //vector[N_basis - 1] d_age_beta;
  //real<upper=0> age_beta_1;

  

  vector[N] intercept_age_z;
  vector[N] slope_age_z;
  vector[N] slope2_age_z;
    
  real<lower=0> sigma_intercept;
  real mean_intercept;


  real<lower=0> sigma_slope;
  real mean_slope;

  real<lower=0> sigma_slope2;
  real mean_slope2;

  // vector[N] const_effect;
  // real<lower=0> sigma_const;


  // real log_dispersion;
}

transformed parameters {
  

  matrix[N, N_age] log_vote_prop;
  vector[N] lam;
  // real dispersion;

  matrix[N,3] beta_age;

  

  // dispersion = exp(log_dispersion);

  beta_age[1:N,1] = mean_intercept + intercept_age_z * sigma_intercept;
  beta_age[1:N,2] = mean_slope + slope_age_z * sigma_slope;
  beta_age[1:N,3] = mean_slope2 + slope2_age_z * sigma_slope2;
  

  
    
  log_vote_prop = beta_age * age_X';

  for (i in 1:N){  
    //lam[i] = exp(log_vote_prop[i]) * pop_data[i]';
    lam[i] = log_sum_exp(log_vote_prop[i] + log_pop_data[i]);
  }
  
}

model {
  

  //beta_age ~ normal(0, 10);
  //intercept_age ~ normal(mean_intercept, sigma_intercept);
  intercept_age_z ~ normal(0, 1);
  sigma_intercept ~ normal(0, 5);
  mean_intercept ~ normal(0, 5);

  slope_age_z ~ normal(0, 1);
  sigma_slope ~ normal(0, 5);
  mean_slope ~ normal(0, 5);

  slope2_age_z ~ normal(0, 1);
  sigma_slope2 ~ normal(0, 5);
  mean_slope2 ~ normal(0, 5);

  
  vote ~ poisson_log(lam);
  //vote ~ neg_binomial_2_log(lam, dispersion);
}
