data {
  int N_age;
  int N;
  int N_basis;
  matrix[N, N_age] pop_data;
  int vote[N];
  int electorate[N];

  matrix[N_age, N_basis] age_basis;
}

parameters {
  //vector[N_basis - 1] d_age_beta;
  //real<upper=0> age_beta_1;


  vector[N_basis] age_beta;
  real<lower=0> sigma_age_beta;

  // vector[N] const_effect;
  // real<lower=0> sigma_const;


  // real log_dispersion;
}

transformed parameters {
  // real dispersion;
  // vector[N_basis] age_beta;
  vector[N_age] smooth_age;

  matrix[N, N_age] log_vote_x;
  vector[N] lam;
  // vector[N] random_effect;

  // random_effect = sigma_const * const_effect;
  // dispersion = exp(log_dispersion);
  
  

  smooth_age = age_basis * age_beta;



  for (i in 1:N){
    // log_vote_x[i] = random_effect[i] + smooth_age' + log(pop_data[i]);  
    log_vote_x[i] = smooth_age' + log(pop_data[i]);  
    lam[i] = log_sum_exp(log_vote_x[i]);
  }
  
}

model {
  

  age_beta[2:N_basis] ~ normal(age_beta[1:(N_basis - 1)],
                               sigma_age_beta * 0.1);


  sigma_age_beta ~ normal(0, 1);
  // const_effect ~ normal(0, 1);

  //vote ~ neg_binomial_2_log(lam, dispersion);
  vote ~ poisson_log(lam);
}
