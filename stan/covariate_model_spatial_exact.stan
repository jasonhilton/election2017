// see http://mc-stan.org/users/documentation/case-studies/icar_stan.html
// http://mc-stan.org/users/documentation/case-studies/mbjoseph-CARStan.html
functions {
  /**
  * Return the log probability of a proper conditional autoregressive (CAR) prior 
  * with a sparse representation for the adjacency matrix
  *
  * @param phi Vector containing the parameters with a CAR prior
  * @param tau Precision parameter for the CAR prior (real)
  * @param alpha Dependence (usually spatial) parameter for the CAR prior (real)
  * @param W_sparse Sparse representation of adjacency matrix (int array)
  * @param n Length of phi (int)
  * @param W_n Number of adjacent pairs (int)
  * @param D_sparse Number of neighbors for each location (vector)
  * @param lambda Eigenvalues of D^{-1/2}*W*D^{-1/2} (vector)
  *
  * @return Log probability density of CAR prior up to additive constant
  */
  real sparse_car_lpdf(vector phi, real tau, real alpha, 
    int[,] W_sparse, vector D_sparse, vector lambda, int n, int W_n) {
      row_vector[n] phit_D; // phi' * D
      row_vector[n] phit_W; // phi' * W
      vector[n] ldet_terms;
    
      phit_D = (phi .* D_sparse)';
      phit_W = rep_row_vector(0, n);
      for (i in 1:W_n) {
        phit_W[W_sparse[i, 1]] = phit_W[W_sparse[i, 1]] + phi[W_sparse[i, 2]];
        phit_W[W_sparse[i, 2]] = phit_W[W_sparse[i, 2]] + phi[W_sparse[i, 1]];
      }
    
      for (i in 1:n) ldet_terms[i] = log1m(alpha * lambda[i]);
      return 0.5 * (n * log(tau)
                    + sum(ldet_terms)
                    - tau * (phit_D * phi - alpha * (phit_W * phi)));
  }
}


data {
  int N;
  int n_covar;
  int vote[N];
  int n_edges;// number of adjacent region pairs
  matrix[N, n_covar] XX;
  vector[N] electorate;
  int node_1[n_edges];
  int node_2[n_edges];

  // just the rbind of node_1, node_2
  int W_sparse[n_edges,2];

  // number of neighbours of each node (rowsums of adjacency mat)
  vector[N] D_sparse;
  vector[N] lambda; // eigenvalues of scaled adj mat.


}


parameters{
  vector[n_covar] beta_covar;
  real<lower=0> sigma_beta;

  //real log_dispersion;

  
  real<lower=0> const_sigma;
  vector[N] phi;

  real<lower=0, upper=1> alpha; // proportion unstructured vs. spatially structured variance
}

transformed parameters {
  vector[N] eta;
  //vector[N] spatial_effect;
  real tau_phi;
  vector[N] log_electorate;

  log_electorate = log(electorate);


  tau_phi = 1 / const_sigma;
  //real dispersion;

  //dispersion = exp(log_dispersion);

  //eta = XX * beta_covar + const_effect * const_sigma;
  
  eta = (XX * beta_covar + phi);

}

model {
  
  //target += -0.5 * dot_self(phi[node_1] - phi[node_2]);
  beta_covar ~ normal(0, sigma_beta);

  //const_effect ~ normal(0, 1);
  //const_sigma ~ normal(0, 5);

  // soft sum-to-zero constraint on phi)
  //sum(phi) ~ normal(0, 0.001 * N);  // equivalent to mean(phi) ~ normal(0,0.001)

  phi ~ sparse_car(tau_phi, alpha, W_sparse, D_sparse, lambda, N, n_edges);

  sigma_beta ~ normal(0, 5);
  const_sigma ~ normal(0, 5);

  //sigma_phi ~ normal(0, 5);
  //vote ~ neg_binomial_2_log(eta + log(electorate), dispersion);
  vote ~ poisson_log(eta + log_electorate);
}
