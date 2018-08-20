data {
  int N_constit;
  int N_age;
  int N_outcomes;
  matrix[N_constit,N_age] pop_data;
  int results_mat[N_constit,N_outcomes]; // can this be optimised

}

parameters {
  simplex[N_outcomes] theta;

}

transformed parameters {


}


model {
  vector[N_outcomes] alpha;

  alpha = rep_vector(1, N_outcomes);

  theta ~ dirichlet(alpha);

  for (i in 1:N_constit){
    results_mat[i] ~ multinomial(theta);
  }
  
}
