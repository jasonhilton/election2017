// see http://mc-stan.org/users/documentation/case-studies/icar_stan.html
data {
  int N;
  int n_covar;
  int n_parties;
  int vote[N, n_parties];
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
  matrix[n_covar, n_parties - 1] beta_covar;

  
  real<lower=0> sigma_beta;

  real log_dispersion;
  //real inv_dispersion;

  //vector[N] const_effect;
  vector<lower=0>[n_parties - 1] const_sigma;
  vector[N] phi[n_parties - 1];
  //vector[N-1] phi_raw;
  //real<lower=0> sigma_phi;
  //real<lower=0, upper=1> rho; // proportion unstructured vs. spatially structured variance
}

transformed parameters {
  matrix[N, n_parties - 1] eta;
  matrix[N, n_parties - 1] spatial_effect;
  //vector[N] phi;
  real dispersion;

  // phi[1] = -sum(phi_raw);
  // phi[2:N] = phi_raw;
 

  dispersion = exp(log_dispersion);
  //dispersion = 1.0/inv_dispersion;

  //eta = XX * beta_covar + const_effect * const_sigma;
  //spatial_effect = sqrt(rho) * const_sigma * phi;
  for (p in 1:(n_parties -1)){
    spatial_effect[1:N, p] = const_sigma[p] * phi[p];
  }
  
  eta = (XX * beta_covar + spatial_effect);

/*  eta = (XX * beta_covar * 0.1 + spatial_effect + 
         sqrt(1 - rho) * const_effect * const_sigma * 0.1);
*/
}

model {
  matrix[N, n_parties] true_eta;
  vector[N] norm_eta;
  for (p in 1:(n_parties-1)){
    target += -0.5 * dot_self(phi[p][node_1] - phi[p][node_2]);
    sum(phi[p]) ~ normal(0, 0.001 * N);
  }
  
  to_vector(beta_covar) ~ normal(0, sigma_beta);

  //const_effect ~ normal(0, 1);
  const_sigma ~ normal(0, 5);
  //inv_dispersion ~ normal(0, 1);

  // soft sum-to-zero constraint on phi)
  // equivalent to mean(phi) ~ normal(0,0.001)

  sigma_beta ~ normal(0, 5);
  //sigma_phi ~ normal(0, 5);
  
  true_eta[1:N,n_parties] = rep_vector(0, N);
  true_eta[1:N,1:(n_parties-1)] = eta;
  for (i in 1:N){
    norm_eta[i] = log_sum_exp(true_eta[i]);
  }
  


  for (p in 1:n_parties){
    //vote[1:N,p] ~ neg_binomial_2_log(eta + log_electorate, dispersion);


    // vote[1:N,p] ~ poisson_log(col(true_eta,p)
    //                           - norm_eta
    //                           +  log_electorate);

    for (i in 1:N){
      if (!(vote[i,p]==0)){
        vote[i,p] ~ neg_binomial_2_log(true_eta[i,p]
                                     - norm_eta[i]
                                     +  log_electorate[i],
                                     dispersion);
      }
      
    }

    
  }
  //vote ~ poisson_log(eta + log(electorate));
}
