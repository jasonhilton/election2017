# starting with nonspatial model. 

library(tidyr)
library(dplyr)
library(magrittr)
library(purrr)
library(ggplot2)
library(splines)
library(rstan)
library(ggfan)
source("R/utility.R")

data_path <- file.path("data/GE2017_results.dta")
results_df <- haven::read_dta(file = data_path)

results_df %<>% filter(ConstituencyName!="Buckingham")
results_df %<>% arrange(ONSConstID) %>% 
  mutate(map_id = 1:631)


results_e_df <- results_df %>% select(ONSConstID, map_id, ConstituencyName,
                       Country, Region, ConstituencyType,
                       ConVote17, LabVote17, LDVote17, SNPVote17, 
                       PCVote17, UKIPVote17, GreenVote17, 
                       TotalVote17, Electorate17) %>%
  filter(grepl("^E", ONSConstID)) ## England only for now

#results_e_df %<>% select(-SNP17, -PC18)
results_e_df %<>%
  mutate(NoVote17 = Electorate17 - TotalVote17,
         OtherVote17 = TotalVote17 - LabVote17 - LDVote17 - ConVote17 - 
           UKIPVote17) %>%
  select(ONSConstID,ConVote17,LabVote17, LDVote17, UKIPVote17, OtherVote17, 
         NoVote17) 

pop_data <- read.csv(file.path("data","const_pop.csv")) %>% as_tibble() %>%
  filter(!(constituency_name == "Buckingham")) %>%
  filter(grepl("^E", ons_id)) %>%
  arrange(ons_id) 

stopifnot(all(pop_data$ons_id==results_e_df$ONSConstID))

pop_data %<>% 
  select(-X, -ons_id, -constituency_name, 
                     -All.Ages) %>% as.matrix()
# exclude age<17
pop_data <- pop_data[,18:dim(pop_data)[2]]

results_mat <- results_e_df %>% select(-ONSConstID) %>% as.matrix()

results_mat[is.na(results_mat)] <- 0
  


N_constit <- dim(pop_data)[1]
N_age <- dim(pop_data)[2]
#n_cats <- 

N_outcomes <- dim(results_mat)[2] 

stan_data <- list(N_constit=N_constit,
                  N_age=N_age,
                  N_outcomes= N_outcomes,
                  pop_data = pop_data,
                  results_mat = results_mat)


base_mod <- stan_model("stan/base_model.stan")

elec_fit <- sampling(base_mod, iter=1000, data=stan_data,
                     cores=4, chains=3)

plot(elec_fit)

stan_diag(elec_fit)
stan_diag(elec_fit,"stepsize")
stan_trace(elec_fit, "theta")

## Test poisson regression version. 

results_e_df <- results_e_df %>%
  mutate_all(function(x) ifelse(is.na(x), 0, x)) %>%
  mutate(N=ConVote17 + LabVote17 + LDVote17 + OtherVote17 +
           NoVote17) 

results_e_df %<>% gather(Party, Vote, -ONSConstID, -N)

results_e_df %<>% mutate(Const_ind = as.factor(ONSConstID), 
                        Const_ind=as.numeric(Const_ind)) %>% 
  mutate(Party_ind = as.numeric(as.factor(Party)))


stan_data <- list(N=dim(results_e_df)[1], 
                  vote = results_e_df$Vote,
                  party_ind = results_e_df$Party_ind,
                  constit_ind = results_e_df$Const_ind,
                  electorate = results_e_df$N,
                  N_constit=N_constit,
                  N_age=N_age,
                  N_outcomes=N_outcomes,
                  pop_data=pop_data)

pois_mod <- stan_model("stan/pois_model.stan")



elec_fit <- sampling(pois_mod, iter=1000, data=stan_data,
                     cores=3, chains=3)

plot(elec_fit)

stan_diag(elec_fit)
stan_diag(elec_fit,"stepsize")
stan_trace(elec_fit, "party_beta")
pairs(elec_fit, pars=c("party_beta"))

party_beta <- as.matrix(elec_fit, "party_beta")
exp(party_beta) /log(rowSums(exp(party_beta)))

probs <- exp(party_beta)/rowSums(exp(party_beta))
plot_matrix(probs, "rows")

## build age splines ------------------------------------
n_basis <- 10
xx <- 1:stan_data$N_age
knots <- seq(1, stan_data$N_age, length.out=n_basis-2)
knots <- c(1 - 3:1*diff(knots)[1], knots, max(knots) + 1:3*diff(knots)[1])

plot_matrix(splineDesign(knots, xx))

age_basis <- splineDesign(knots, xx)

n_basis <- dim(age_basis)[2]

stan_data$age_basis <- age_basis
stan_data$N_basis <- n_basis

pois_age_mod <- stan_model("stan/pois_model_age.stan")

elec_fit <- sampling(pois_age_mod, iter=1000, cores=3, chains=3,
                     data=stan_data)

## test with one party -------------------------------------

results_lab <- results_e_df %>% filter(Party=="LabVote17")

stan_data <- list(electorate=results_lab$N,
                  vote=results_lab$Vote,
                  N=N_constit,
                  N_constit=N_constit,
                  N_age=N_age,
                  pop_data=pop_data,
                  N_basis=n_basis,
                  age_basis=age_basis)


ident <- diag(N_age)
ident[1,] <- rep(1, N_age)


pois_age_mod <- stan_model("stan/pois_model_age_1.stan")

elec_fit <- sampling(pois_age_mod, iter=1000, cores=3, chains=3,
                     data=stan_data)

stan_trace(elec_fit, "age_beta")

party_age <- as.matrix(elec_fit, "party_age")

