data {
  int N_age;
  int N;
  int N_constit;
  int N_basis;
  matrix[N_constit, N_age] pop_data;

  matrix[N_basis, N_basis - 1] inv_constraint;
  matrix[N_basis - 1, N_basis - 1] Tau;
  

  // int results_mat[N_constit,N_outcomes]; // can this be optimised
  int vote[N];
  int electorate[N];

  matrix[N_age, N_basis] age_basis;
}

transformed data {
  vector[N_basis-1] zeros;
  zeros = rep_vector(0,N_basis - 1);
}

parameters {
  // vector[N_outcomes - 1] party_beta_raw;
  vector[N_basis - 1] d_age_beta_raw;
  // real age_beta_1;

  real<lower=0> sigma_age_beta;
  vector[N_constit] const_effect;
  real<lower=0> sigma_const;
  real const_mean;

  real<lower=0> inv_dispersion;
}

transformed parameters {
  vector[N_age] party_age;
  vector[N_age] party_age_const[N_constit];
  matrix[N_constit, N_age] vote_profile;
  vector[N_constit] net_effect;
  vector[N_basis] age_beta;
  real dispersion;

  dispersion = 1 / inv_dispersion;

  //dispersion = exp(log_dispersion);

  
  age_beta = cumulative_sum(inv_constraint * d_age_beta_raw);


  party_age = age_basis * age_beta;

  for (c in 1:N_constit){
    for (i in 1:N_age){
      party_age_const[c][i] = inv_logit(party_age[i] + 
                                        const_mean + const_effect[c] * sigma_const);
    }  
  }
  

  for (i in 1:N){
    vote_profile[i] = pop_data[i] .* party_age_const[i]';
    net_effect[i] = sum(vote_profile[i]);
  }
}

model {
  vector[N] lam;

  //party_beta ~ normal(0, 10);
  // age_beta_1 ~ normal(0, 1);
  d_age_beta_raw ~ multi_normal(zeros, sigma_age_beta * Tau);
  sigma_age_beta ~ normal(0, 1);
  const_effect ~ normal(0, 1);
  const_mean ~ normal(0, 1);
  sigma_const ~ normal(0, 5);
  
  inv_dispersion ~ normal(0,1);

  for (i in 1:N){

    lam[i] = ((electorate[i] /
               sum(pop_data[i])) *
              net_effect[i]);
              
  }
  

  //vote ~ poisson(lam);
  vote ~ neg_binomial_2(lam, dispersion);
}
