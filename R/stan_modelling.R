# starting with nonspatial model. 

library(tidyr)

library(magrittr)
library(purrr)
library(ggplot2)
library(splines)
library(rstan)
library(ggfan)
library(bayesplot)
library(raster)

library(maptools)

library(spdep)
#install.packages("INLA", repos=c(getOption("repos"), 
#                                 INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)

library(tibble)
library(haven)
library(rgdal) 
library(spdep)
source("R/utility.R")
library(dplyr)

# raster library (used by one of the geo libs) masks the dplyr select.
select <- dplyr::select


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
                     cores=3, chains=3)

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
stan_trace(elec_fit, "lp__")
stan_trace(elec_fit, "log_dispersion")
stan_trace(elec_fit, "inv_dispersion")
stan_diag(elec_fit)
stan_diag(elec_fit, "stepsize")
stan_diag(elec_fit, "divergence")
stan_rhat(elec_fit)
party_age <- as.matrix(elec_fit, "party_age")
plot_matrix(party_age, "rows")
age_beta <- as.matrix(elec_fit, "age_beta")
plot_matrix(age_beta, "rows")

vote_profile <- as.matrix(elec_fit, "vote_profile")
vote_profile %<>% t() %>% as_tibble() %>%
  mutate(Constit = rep(results_lab$Const_ind, stan_data$N_age),
         Age= rep(17 + 1:N_age, rep(stan_data$N_constit, stan_data$N_age)))

vote_profile %<>% gather(Sim, Profile, -Age, -Constit)

ggplot(vote_profile %>% filter(Constit==1), 
       aes(x=Age,y=Profile)) + geom_interval()

ggplot(vote_profile %>% filter(Constit==2), 
       aes(x=Age,y=Profile)) + geom_interval()

net_effect <- as.matrix(elec_fit, "net_effect")

net_effect %<>% t() %>% as_tibble() %>% 
  mutate(Constituency=results_lab$Const_ind) %>%
  gather(Sim, Effect, -Constituency)

net_effect %<>% group_by(Constituency) %>% 
  summarise(Effect=mean(Effect)) %>%
  rename(Const_ind=Constituency) %>%
  left_join(results_lab)

ggplot(net_effect,aes(x=Effect,y=Vote)) + geom_point() +
  geom_abline()


## Try with constraint of first element = 0

cons <- get_age_conditional_cov_matrix(stan_data$age_basis)

stan_data$Tau <- cons$Tau
stan_data$inv_constraint <- cons$inv_constraint

# normalise pop_data
#stan_data$pop_data <- stan_data$pop_data/rowSums(stan_data$pop_data)

elec_model <- stan_model("stan/pois_model_age_2.stan")

elec_fit <- sampling(elec_model, iter=1000, cores=3, chains=3,
                     data=stan_data)

const_mean <- as.matrix(elec_fit, "const_mean")
plot_matrix(apply(party_age, 2, function(X) plogis(X + const_mean)),"rows")

net_effect %<>% mutate(Pop=rowSums(pop_data), lam=N/Pop*Effect)

ggplot(net_effect, aes(x=lam, y=Vote)) + geom_point()

stan_dens(elec_fit, "const_mean")


## covariates -----------------------

results_e_df <- results_df %>%
  filter(grepl("^E", ONSConstID)) %>% ## England only for now
  select(ONSConstID,
         ConstituencyName,
         LabVote17,
         ConVote17, 
         Electorate17,
         c11HouseOwned,
         c11EthnicityAsian,
         c11EthnicityBlack,
         c11Employed,
         c11Degree,
         c11DeprivedNone,
         c11PassportEU,
         c11QualNone) %>%
  filter(!grepl("Wight", ConstituencyName)) 
# The isle of wight is disconnected


XX <- results_e_df %>% select(-ONSConstID,
                              -ConstituencyName,
                              -LabVote17,
                              -ConVote17, 
                              -Electorate17) %>%
  mutate_all(scale) %>% as.matrix()

XX <- cbind(rep(1,dim(XX)[1]), XX)

stan_data <- list(XX=XX, 
                  vote=results_e_df$LabVote17, 
                  electorate=results_e_df$Electorate17,
                  N=dim(XX)[1], n_covar=dim(XX)[2])

elec_model <- stan_model("stan/covariate_model.stan")

elec_fit <- sampling(elec_model, iter=1000, cores=3, chains=3,
                     data=stan_data)

plot(elec_fit)

stan_diag(elec_fit)
stan_diag(elec_fit, "stepsize")
stan_diag(elec_fit, "treedepth")
stan_diag(elec_fit, "divergence")
stan_rhat(elec_fit)
stan_trace(elec_fit, "lp__")
stan_trace(elec_fit, "beta_covar")
stan_trace(elec_fit, "intercept")
stan_trace(elec_fit, "sigma_phi")
#pairs(elec_fit, pars=c("const_sigma","sigma_phi","lp__"))
pairs(elec_fit, pars=c("const_sigma","rho","lp__"))
pairs(elec_fit, pars=c("phi[1]", "const_effect[1]","phi[2]", "const_effect[2]",
                       "phi[3]", "const_effect[3]"))

pairs(elec_fit, pars=c("phi[1]","phi[2]", "phi[3]", "const_sigma"))
stan_trace(elec_fit, pars=c("phi[1]","phi[2]", "phi[3]", "const_sigma"))

beta_covar <- as.matrix(elec_fit, "beta_covar")
eta <- as.matrix(elec_fit, "eta")

plot(stan_data$vote, colMeans(exp(eta + log(stan_data$electorate))))
abline(0,1)

## spatial ---------------------------------------------------------------------
shape_file_name <- list.files(file.path("data", "mapping"), 
                              pattern="*.shp")
const <- readOGR(file.path( "data", "mapping",shape_file_name),
                 stringsAsFactors = F)

const <- const[grepl("^E",const$pcon17cd),]
const <- const[!grepl("Buckingham",const$pcon17nm),]
const <- const[!grepl("Wight",const$pcon17nm),]
const <- const[order(const$pcon17cd),]
const$map_id <- 1:dim(const)[1]

const_nb <- poly2nb(const,snap=1e-03, row.names = const$map_id)
plot(const)
const_coords <- coordinates(const)

const_nb <- poly2nb(const,snap=1e-03, row.names = const$map_id)
plot(const_nb, coords=const_coords)
nb_B <- nb2listw(const_nb, style="B", zero.policy = T)
mat<-as(nb_B, "symmetricMatrix")

ii <- mat@i
jj <- mat@j


stan_data$node_1 <- ii + 1
stan_data$node_2 <- jj + 1 
stan_data$n_edges <- length(ii)


elec_model <- stan_model("stan/covariate_model_spatial.stan")
elec_model <- stan_model("stan/model_spatial.stan")
dir.create("results")
elec_fit <- sampling(elec_model, iter=1000, cores=3, chains=3,
                     data=stan_data)
elec_fit <- sampling(elec_model, iter=1000, cores=3, chains=3,
                     data=stan_data,
                     diagnostic_file="results/spatial_diag")

mcmc_nuts_energy(nuts_params(elec_fit))

spatial_effect <- as.matrix(elec_fit, "spatial_effect")
map_df <- fortify(const, region="map_id")

results_df <- tibble(map_id=1:531,
                     `Spatial residual` = colMeans(exp(spatial_effect)))


ggplot(results_df) + 
  geom_map(map=map_df,
           aes(map_id=map_id,fill=`Spatial residual`),
           colour="grey",size=0.1) + 
  expand_limits(x=map_df$long,y=map_df$lat) +
  coord_fixed() +
  scale_fill_gradient2(low="black",mid="white", high="red",midpoint =1) +
  transparent_theme +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )

pairs(elec_fit, pars=c("intercept","beta_covar[1]","beta_covar[6]","beta_covar[8]"))
pairs(elec_fit, pars=c("log_dispersion", "const_sigma", "lp__"))

read_diag <- function(i, diag_dir="results"){
  diag_df <- read.csv(file.path(diag_dir,
                                paste0("spatial_diag_",i,".csv")),
                      comment.char="#") %>% as_tibble() %>%
    mutate(iter=1:n(), Chain=i)
  return(diag_df)
}

diag_df <- map_dfr(1:length(elec_fit@stan_args), read_diag)

upar <- diag_df %>% filter(iter>500) %>% select(beta_covar.1:rho)
upar <- diag_df %>% filter(iter>500) %>% select(beta_covar.1:phi.531)
upar <- diag_df %>% filter(iter>500) %>% select(intercept:phi_raw.530)

upar %>% summarise_all(sd) %>% gather()

masses <- get_mass_matrix(elec_fit)
arrange(masses, Inv_mass) %>% head(20)

all_cors <- cor(diag_df %>% filter(iter>500) %>% select(beta_covar.1:rho))

ggplot(diag_df %>% filter(iter>500),
       aes(x=const_sigma,y=sigma_phi,
                 colour=lp__)) + geom_point()

ggplot(diag_df %>% filter(iter>500),
       aes(x=const_sigma,y=inv_dispersion,
           colour=lp__)) + geom_point()


library(corrplot)
i1<-1
i2<-100
corrplot(all_cors[i1:i2,i1:i2], method="color",tl.pos='n')
i1<-550
i2<-600
corrplot(all_cors[i1:i2,i1:i2], method="color")

## exact car -----------------------------------------------------------------
# exact car needs no intercept
stan_data$XX <- stan_data$XX[,2:stan_data$n_covar]
stan_data$n_covar <- stan_data$n_covar - 1
D_sparse <- rowSums(as.matrix(mat))
invsqrt_D <- diag(1/sqrt(D_sparse))
lambda <- eigen(invsqrt_D %*% mat %*% invsqrt_D)$values
W_sparse = cbind(stan_data$node_1, stan_data$node_2)
stan_data$W_sparse <- W_sparse
stan_data$D_sparse <- D_sparse
stan_data$lambda <- lambda

elec_model <- stan_model("stan/covariate_model_spatial_exact.stan")


elec_fit <- sampling(elec_model, iter=1000, cores=3, chains=3,
                     data=stan_data, 
                     diagnostic_file="results/spatial_exact")
# alpha is virtually 1
stan_dens(elec_fit, "alpha")

### -- multivariate-------------------------------------------------------------

results_df <- haven::read_dta(file = data_path)

results_df %<>% filter(ConstituencyName!="Buckingham")
results_df %<>% arrange(ONSConstID) %>% 
  mutate(map_id = 1:631)


results_e_df <- results_df %>%
  filter(grepl("^E", ONSConstID)) %>% ## England only for now
  select(ONSConstID,
         ConstituencyName,
         LabVote17,
         ConVote17, 
         LDVote17,
         UKIPVote17,
         GreenVote17,
         TotalVote17,
         Electorate17,
         c11HouseOwned,
         c11EthnicityAsian,
         c11EthnicityBlack,
         c11Employed,
         c11Degree,
         c11DeprivedNone,
         c11PassportEU,
         c11QualNone) %>%
  filter(!grepl("Wight", ConstituencyName)) 
# The isle of wight is disconnected


XX <- results_e_df %>% select(starts_with("c11")) %>%
  mutate_all(scale) %>% as.matrix()

XX <- cbind(rep(1,dim(XX)[1]), XX)

results_e_df %<>%
  mutate_at(vars(matches("Vote17$")), function(x) ifelse(is.na(x), 0, x)) %>%
  mutate(OtherVote17 = TotalVote17 - LabVote17 - ConVote17 - LDVote17,
         NoVote17 = Electorate17 - TotalVote17)

vote <- results_e_df %>% select(LabVote17, ConVote17, 
                              LDVote17, OtherVote17, NoVote17) %>%
  as.matrix()

stan_data <- list(XX=XX, 
                  vote=vote,
                  n_parties=dim(vote)[2],
                  electorate=results_e_df$Electorate17,
                  N=dim(XX)[1], 
                  n_covar=dim(XX)[2])

stan_data$node_1 <- ii + 1
stan_data$node_2 <- jj + 1 
stan_data$n_edges <- length(ii)

elec_model <- stan_model("stan/covariate_model_spatial_multi.stan")


elec_fit <- sampling(elec_model, iter=1000, cores=3, chains=3,
                     data=stan_data)


spat <- spatial_effect %>% t() %>% as_tibble() %>% 
  mutate(const_id =rep(1:stan_data$N,stan_data$n_parties-1), 
         party=rep(c("Lab", "Con", "LD", "Other"), rep(stan_data$N, 4)))

spat %<>% gather(Sim, Spatial_effect, -party, -const_id) %>% 
  group_by(const_id, party) %>% 
  summarise(Spatial_effect=mean(Spatial_effect))

spat_lab <- spat %>% filter(party=="Lab")




ggplot(spat_lab) + 
  geom_map(map=map_df,
           aes(map_id=const_id,fill=Spatial_effect),
           colour="grey",size=0.1) + 
  expand_limits(x=map_df$long,y=map_df$lat) +
  coord_fixed() +
  scale_fill_gradient2(low="black",mid="white", high="red") +
  transparent_theme +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )


spat_con <- spat %>% filter(party=="Con")
ggplot(spat_con) + 
  geom_map(map=map_df,
           aes(map_id=const_id,fill=Spatial_effect),
           colour="grey",size=0.1) + 
  expand_limits(x=map_df$long,y=map_df$lat) +
  coord_fixed() +
  scale_fill_gradient2(low="black",mid="white", high="blue") +
  transparent_theme +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )

spat_lib <- spat %>% filter(party=="LD")
ggplot(spat_lib) + 
  geom_map(map=map_df,
           aes(map_id=const_id,fill=Spatial_effect),
           colour="grey",size=0.1) + 
  expand_limits(x=map_df$long,y=map_df$lat) +
  coord_fixed() +
  scale_fill_gradient2(low="black",mid="white", high="orange") +
  transparent_theme +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )

spat_other <- spat %>% filter(party=="Other")
ggplot(spat_other) + 
  geom_map(map=map_df,
           aes(map_id=const_id,fill=Spatial_effect),
           colour="grey",size=0.1) + 
  expand_limits(x=map_df$long,y=map_df$lat) +
  coord_fixed() +
  scale_fill_gradient2(low="black",mid="white", high="purple") +
  transparent_theme +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )
