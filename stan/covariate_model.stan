data {
  int N;
  int n_covar;
  int vote[N];
  matrix[N, n_covar] XX;
  vector[N] electorate;

}

parameters{
  vector[n_covar] beta_covar;
  real<lower=0> sigma_beta;

  real log_dispersion;

  vector[N] const_effect;
  real const_sigma;
}

transformed parameters {
  vector[N] eta;
  real dispersion;

  dispersion = exp(log_dispersion);

  eta = XX * beta_covar + const_effect * const_sigma;

}

model {

  beta_covar ~ double_exponential(0,sigma_beta);

  const_effect ~ normal(0, 1);
  const_sigma ~ normal(0, 5);

  sigma_beta ~ normal(0, 5);
  vote ~ neg_binomial_2_log(eta + log(electorate) , dispersion);
}
