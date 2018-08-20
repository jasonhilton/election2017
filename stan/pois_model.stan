data {
  int N_constit;
  int N_age;
  int N_outcomes;
  int N;
  matrix[N_constit,N_age] pop_data;
  

  // int results_mat[N_constit,N_outcomes]; // can this be optimised
  int vote[N];
  int constit_ind[N];
  int party_ind[N];
  int electorate[N];
}

parameters {
  vector[N_outcomes - 1] party_beta_raw;


}

transformed parameters {
  vector[N_outcomes] party_beta;
  party_beta[4] = 0; // No vote as ref category (CHECK WITH CHANGES)
  party_beta[1:3] = party_beta_raw[1:3];
  party_beta[5:N_outcomes] = party_beta_raw[4:(N_outcomes-1)];
}


model {
  vector[N] lam;

  //party_beta ~ normal(0, 10);

  for (i in 1:N){

    lam[i] = (log(electorate[i]) + party_beta[party_ind[i]] - 
              log_sum_exp(party_beta));
  }
  

  vote ~ poisson_log(lam);
}
