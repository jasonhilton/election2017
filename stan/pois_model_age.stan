data {
  int N_constit;
  int N_age;
  int N_outcomes;
  int N;
  int N_basis;
  matrix[N_constit, N_age] pop_data;
  

  // int results_mat[N_constit,N_outcomes]; // can this be optimised
  int vote[N];
  int constit_ind[N];
  int party_ind[N];
  int electorate[N];

  matrix[N_age, N_basis] age_basis;
}

parameters {
  // vector[N_outcomes - 1] party_beta_raw;
  matrix[N_outcomes - 1, N_basis] age_beta_raw;

}

transformed parameters {
  matrix[N_outcomes, N_basis] age_beta;
  vector[N_age] party_age[N_outcomes];
  matrix[N_constit, N_age] vote_profile[N_outcomes];
  vector[N_constit] norm_const;


  age_beta[4] = rep_row_vector(0, N_basis);
  age_beta[1:3] = age_beta_raw[1:3];
  age_beta[5:N_outcomes] = age_beta_raw[4:(N_outcomes - 1)];
  

  for (p in 1:N_outcomes){
    party_age[p] = age_basis * age_beta[p]';  
  }

  for(c in 1:N_constit){
    norm_const[c] = 0;
    for(p in 1:N_outcomes){
      vote_profile[p][c] = pop_data[c] .* party_age[p]';
      norm_const[c] += exp(sum(vote_profile[p][c]));
    }
  }
  


}

model {
  vector[N] lam;

  //party_beta ~ normal(0, 10);

  for (p in 1:(N_outcomes - 1)){
    age_beta_raw[p] ~ normal(0, 5);
  }
  

  for (i in 1:N){

    lam[i] = (log(electorate[i]) + 
              sum(vote_profile[party_ind[i]][constit_ind[i]]) - 
              log(norm_const[constit_ind[i]]));
  }
  

  vote ~ poisson_log(lam);
}
