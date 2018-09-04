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
  // real log_dispersion;

  real intercept;

  // vector[N] phi;
  vector[N-1] phi_raw;
  
  real<lower=0> sigma_phi;
  //real<lower=0, upper=1> rho; // proportion unstructured vs. spatially structured variance
}

transformed parameters {
  vector[N] eta;
  vector[N] spatial_effect;
  vector[N] phi;
  //vector[N] random_effect;
  // real dispersion;
  

  phi[1] = 0;
  phi[2:N] = phi_raw;
  

  // dispersion = exp(log_dispersion);

  //eta = XX * beta_covar + const_effect * const_sigma;
  //spatial_effect = sqrt(rho) * const_sigma * phi;
  spatial_effect = sigma_phi * phi;
  
  
  // eta = (intercept + spatial_effect + 
  //        sqrt(1 - rho) * const_effect * const_sigma);
  eta = ((intercept * 0.1 + spatial_effect));

}

model {
  
  target += -0.5 * dot_self(phi[node_1] - phi[node_2]);
  
  intercept ~ normal(0, 10);

  
  // soft sum-to-zero constraint on phi)
  // sum(phi) ~ normal(0, 0.001 * N);  // equivalent to mean(phi) ~ normal(0,0.001)

  
  sigma_phi ~ normal(0, 5);
  //vote ~ neg_binomial_2_log(eta + log(electorate), dispersion);
  vote ~ poisson_log(eta + log(electorate));
}
