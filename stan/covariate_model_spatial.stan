// see http://mc-stan.org/users/documentation/case-studies/icar_stan.html
data {
  int N;
  int n_covar;
  int vote[N];
  int n_edges;
  matrix[N, n_covar] XX;
  vector[N] electorate;

  int node_1[n_edges];
  int node_2[n_edges];

}

parameters{
  vector[n_covar] beta_covar;
  real<lower=0> sigma_beta;

  real log_dispersion;

  //vector[N] const_effect;
  // real const_sigma;
  vector[N] phi;
}

transformed parameters {
  vector[N] eta;
  real dispersion;

  dispersion = exp(log_dispersion);

  //eta = XX * beta_covar + const_effect * const_sigma;
  eta = XX * beta_covar + phi;// + const_effect * const_sigma;

}

model {
  
  target += -0.5 * dot_self(phi[node1] - phi[node2]);
  beta_covar ~ double_exponential(0,sigma_beta);

  const_effect ~ normal(0, 1);
  const_sigma ~ normal(0, 5);

  // soft sum-to-zero constraint on phi)
  sum(phi) ~ normal(0, 0.001 * N);  // equivalent to mean(phi) ~ normal(0,0.001)

  sigma_beta ~ normal(0, 5);
  vote ~ neg_binomial_2_log(eta + log(electorate) + phi, dispersion);
}
