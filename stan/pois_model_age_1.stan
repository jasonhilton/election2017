data {
  int N_age;
  int N;
  int N_constit;
  int N_basis;
  matrix[N_constit, N_age] pop_data;
  

  // int results_mat[N_constit,N_outcomes]; // can this be optimised
  int vote[N];
  int electorate[N];

  matrix[N_age, N_basis] age_basis;
}

parameters {
  // vector[N_outcomes - 1] party_beta_raw;
  vector[N_basis - 1] d_age_beta;
  real age_beta_1;

  real<lower=0> sigma_age_beta;

}

transformed parameters {
  vector[N_age] party_age;
  matrix[N_constit, N_age] vote_profile;
  vector[N_constit] net_effect;
  vector[N_basis] age_beta;

  age_beta[1] = age_beta_1;

  age_beta[2:N_basis] = age_beta_1 + cumulative_sum(d_age_beta * sigma_age_beta);

  party_age = age_basis * age_beta;


  for (i in 1:N){
    vote_profile[i] = pop_data[i] .* exp(party_age)';
    net_effect[i] = sum(vote_profile[i]);
  }
}

model {
  vector[N] lam;

  //party_beta ~ normal(0, 10);

  d_age_beta ~ normal(0, 1);
  sigma_age_beta ~ normal(0, 1);

  

  for (i in 1:N){

    lam[i] = ((electorate[i] /
               sum(pop_data[i])) *
              net_effect[i]);
              
  }
  

  vote ~ poisson(lam);
}
