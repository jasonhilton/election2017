transparent_theme <- theme(
    panel.background = element_rect(fill = "transparent",
                                    colour="transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent",
                                     colour="transparent") # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent",
                                       colour="transparent",
                                       linetype = 0) # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent", 
                                           colour="transparent") # get rid of legend panel bg
  )



commonwealth_countries <- c(
"Antigua and Barbuda",
"Australia",
"Bangladesh",
"Barbados",
"Belize",
"Botswana",
"Brunei",
"Cameroon",
"Canada",
"Dominica",
"Fiji",
"The Gambia",
"Ghana",
"Grenada",
"Grenadines",
"Guyana",
"India",
"Jamaica",
"Kenya",
"Kiribati",
"Lesotho",
"Malawi",
"Malaysia",
"Malta",
"Mauritius",
"Mozambique",
"Namibia",
"Nauru",
"New Zealand",
"Nigeria",
"Pakistan",
"Papua New Guinea",
"Republic of Cyprus*",
"Rwanda",
"Samoa",
"Seychelles",
"Sierra Leone",
"Singapore",
"Solomon Islands",
"South Africa",
"Sri Lanka",
"St Christopher and Nevis",
"St Lucia",
"St Vincent and the",
"Swaziland",
"The Bahamas",
"Tonga",
"Trinidad and Tobago",
"Tuvalu",
"Uganda",
"United Kingdom",
"United Republic of Tanzania",
"Vanuatu",
"Zambia",
"Zimbabwe"
) 
# Source:
# https://www.tendringdc.gov.uk/council/elections-voting/list-commonwealth-countries-voting-rights


plot_matrix <- function(sample_mat, plot_dim =c("columns","rows"), add=F, ...){
  plot_dim <- match.arg(plot_dim)
  if (plot_dim == "rows"){
    sample_mat <- t(sample_mat)
  }
  
  iters <- 1:ncol(sample_mat)
  if (!add){
    plot(sample_mat[,1], ylim=range(sample_mat), type="l", ...)  
  }
  rubbish<-lapply(iters, function(j) points(sample_mat[,j], type="l", ...))
}


get_constraint <- function(basis_matrix){
  n_elems <- dim(basis_matrix)[1]
  n_basis <- dim(basis_matrix)[2]
  
  #cons <- c(1,rep(0, n_elems-1)) %*% basis_matrix
  cons <- rep(1, n_elems) %*% basis_matrix
  
  return(cons)
}

#' Construct the first-difference matrix operator
#' 
#' Returns square matrix $D$ with rows and columns $n$ that computes the first
#' difference of anything it is post-multiplied by.
#' 
#' @param n The number of rows in the difference matrix.
#' 
#' @return An $n$ x $n$ matrix with 1 on the diagonal and -1 on the first 
#' lower off-diagnoal
get_difference_matrix <- function(n){
  D <- matrix(0, n, n)
  D[2:n, 1:(n - 1)] <- - diag(n - 1)
  D <- D + diag(n)
  return(D)
}


get_age_conditional_cov_matrix <- function(cohort_basis){
  n_cohort_basis <- min(dim(cohort_basis))
  constraint <- get_constraint(cohort_basis)  
  # get cumulative sum matrix
  S <- solve(get_difference_matrix(n_cohort_basis))
  CS <- constraint %*% S
  result <- get_conditional_cov_matrix(CS, 1)
  return(result)
}




get_conditional_cov_matrix <- function(CS, con_ind){
  ZZ <- get_constraint_transformation_matrix(CS, con_ind)
  Sigma <- ZZ %*% t(ZZ)
  Tau <- (Sigma[-con_ind,-con_ind] - Sigma[-con_ind,con_ind] %*%
            solve(Sigma[con_ind,con_ind]) %*%
            Sigma[con_ind,-con_ind])
  inv_constraint <- solve(ZZ)[,-con_ind]
  return(list(Tau=Tau, inv_constraint=inv_constraint))
}

get_constraint_transformation_matrix <- function(constraint, con_ind){
  n_basis <- dim(constraint)[2]
  ZZ <- diag(n_basis)
  ZZ[con_ind,] <- constraint
  return(ZZ)
}












