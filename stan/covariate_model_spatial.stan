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

transformed data {
  vector[N] log_electorate;

  log_electorate = log(electorate);
}

parameters{
  vector[n_covar-1] beta_covar;

  real intercept;
  real<lower=0> sigma_beta;

  // real log_dispersion;
  //real<lower=0> inv_dispersion;

  vector[N] const_effect;
  real<lower=0> const_sigma;
  vector[N] phi;
  //vector[N-1] phi_raw;
  real<lower=0> sigma_phi;
  //real<lower=0, upper=1> rho; // proportion unstructured vs. spatially structured variance
}

transformed parameters {
  vector[N] eta;
  vector[N] spatial_effect;
  // vector[N] phi;
  // real dispersion;

  // phi[1] = -sum(phi_raw);
  // // phi[1] = 0;
  // phi[2:N] = phi_raw;
 

  //dispersion = exp(log_dispersion);
  // dispersion = 1.0 / (inv_dispersion * 0.01);

  //eta = XX * beta_covar + const_effect * const_sigma;
  //spatial_effect = sqrt(rho) * const_sigma * phi;
  //spatial_effect = const_sigma * phi;
  spatial_effect = sigma_phi * phi;
  eta = (intercept * 0.1 + XX[1:N, 2:n_covar] * beta_covar + 
         spatial_effect + 
         const_effect * const_sigma);

/*  eta = (XX * beta_covar * 0.1 + spatial_effect + 
         sqrt(1 - rho) * const_effect * const_sigma * 0.1);
*/
}

model {
  
  target += -0.5 * dot_self(phi[node_1] - phi[node_2]);
  beta_covar ~ double_exponential(0, sigma_beta);

  const_effect ~ normal(0, 1);
  const_sigma ~ normal(0, 5);
  intercept ~ normal(0,10);
  // inv_dispersion ~ normal(0, 1);

  // soft sum-to-zero constraint on phi)
  sum(phi) ~ normal(0, 0.001 * N);  // equivalent to mean(phi) ~ normal(0,0.001)

  sigma_beta ~ normal(0, 5);
  sigma_phi ~ normal(0, 5);
  //vote ~ neg_binomial_2_log(eta + log_electorate, dispersion);
  vote ~ poisson_log(eta + log(electorate));
}
