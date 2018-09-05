data {
  int N_age;
  int N;
  int N_constit;
  int N_basis;
  int n_edges;
  
  matrix[N_constit, N_age] pop_data;
  

  // int results_mat[N_constit,N_outcomes]; // can this be optimised
  int vote[N];
  int electorate[N];

  matrix[N_age, N_basis] age_basis;


  int node_1[n_edges];
  int node_2[n_edges];

}

parameters {
  // vector[N_outcomes - 1] party_beta_raw;
  vector[N_basis - 1] d_age_beta;
  real age_beta_1;

  real<lower=0> sigma_age_beta;

  vector[N] phi;
  //vector[N-1] phi_raw;
  real<lower=0> sigma_phi;

  real log_dispersion;
}

transformed parameters {
  real dispersion;
  vector[N_basis] age_beta;
  vector[N_age] smooth_age;

  vector[N, N_age] log_vote_x;
  vector[N] vote_x;
  vector[N] spatial_effect;

  spatial_effect = sigma_phi * phi;

  dispersion = exp(log_dispersion);
  
  age_beta = age_beta_1 + cumulative_sum(d_age_beta * sigma_age_beta);

  smooth_age = age_basis * age_beta;

  for (i in 1:N){
    log_vote_x[i] = spatial_effect + smooth_age + log(pop_date[i]);  
    lam[i] = log_sum_exp(log_vote_x[i]);
  }
  
}

model {
  
  target += -0.5 * dot_self(phi[node_1] - phi[node_2]);
  sum(phi) ~ normal(0, 0.001 * N); 

  d_age_beta ~ normal(0, 1);
  sigma_age_beta ~ normal(0, 1);

  vote ~ neg_binomial_2_log(lam, dispersion);
}
