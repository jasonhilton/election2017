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


get_mass_matrix <- function(fert_fit){
  split_adapt_strings <- stringr::str_split(rstan::get_adaptation_info(fert_fit), "#")
  mass_matrices <- map(split_adapt_strings, 
                       function(x) stringr::str_split(x[5], ", ") %>% 
                         unlist() %>% (function(y) gsub("\n", "",y)) %>% 
                         as.numeric())
  mass_mat <- do.call(cbind, mass_matrices) %>% as_tibble()
  par_names <- names(fert_fit@sim$samples[[1]])[1:dim(mass_mat)[1]]
  mass_mat <- cbind(tibble(parameter=par_names), mass_mat)
  mass_mat %<>% gather(Chain, Inv_mass, -parameter) 
  mass_mat %<>% mutate(Chain=as.numeric(gsub(pattern="V", "",Chain)))
  
  return(mass_mat)
}

get_num_parameters <- function(fert_fit){
  split_adapt_strings <- stringr::str_split(rstan::get_adaptation_info(fert_fit), "#")
  n_pars <- stringr::str_split(split_adapt_strings[[1]][5], ", ") %>% 
    unlist() %>%
    length()
  return(n_pars)
}


get_stepsizes <- function(fert_fit){
  split_adapt_strings <- stringr::str_split(rstan::get_adaptation_info(fert_fit), "#")
  stepsize <- map_dbl(split_adapt_strings, 
                      function(x) as.numeric(stringr::str_extract(x[3],"[0-9]+\\.[0-9]+")))
  return(stepsize)
}


read_diag <- function(i, file_name, diag_dir="results"){
  diag_df <- read.csv(file.path(diag_dir,
                                paste0(file_name, "_", i, ".csv")),
                      comment.char="#") %>% as_tibble() %>%
    mutate(iter=1:n(), Chain=i)
  return(diag_df)
}

get_2d_stan_df  <- function(model_fit, param_name,
                            index_1_name= "Index_1", 
                            index_2_name= "Index_2",
                            conversion_1_func=identity,
                            conversion_2_func=identity,
                            value_name ="Value"){
  if(class(model_fit)!="stanfit"){
    stop("model_fit must be of class `stanfit`")
  }
  par_mat <- as.matrix(model_fit, param_name)
  
  par_names <- colnames(par_mat)
  
  if(max(stringr::str_count(par_names,"\\,")) > 1){
    stop("Parameter has more than 2 dimensions")
  }
  if(min(stringr::str_count(par_names, "\\,")) <1 ){
    stop("Parameter has fewer than 2 dimensions")
  }
  ind_1 <- stringr::str_extract_all(par_names,
                                    # match all numbers followed by a comma
                                    "[0-9]+(?=\\,)",
                                    simplify = T) %>% 
    as.numeric()
  ind_2 <- stringr::str_extract_all(par_names,
                                    # match all numbers preceded by a comma
                                    "(?<=\\,)[0-9]+",
                                    simplify = T) %>% 
    as.numeric()
  
  par_df <- par_mat %>% t() %>% tibble::as_tibble() %>%
    mutate(!!index_1_name:=conversion_1_func(ind_1),
           !!index_2_name:=conversion_2_func(ind_2)) %>%
    gather(Sim, !!value_name, -!!index_1_name, -!!index_2_name)
  return(par_df)
}









