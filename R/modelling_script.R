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
library(corrplot)
library(tibble)
library(haven)
library(rgdal) 
library(spdep)
library(dplyr)

source("R/utility.R")

select <- dplyr::select


## load in and process data

# Electoral data --------------------------------------------------------------
data_path <- file.path("data/GE2017_results.dta")
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

# Geographic data ---------------------------------------------------------------
## read in map data

shape_file_name <- list.files(file.path("data", "mapping"), 
                              pattern="*.shp")
const <- readOGR(file.path( "data", "mapping",shape_file_name),
                 stringsAsFactors = F)

## filter to correct bits 
const <- const[grepl("^E",const$pcon17cd),]
const <- const[!grepl("Buckingham",const$pcon17nm),]
const <- const[!grepl("Wight",const$pcon17nm),]
const <- const[order(const$pcon17cd),]
const$map_id <- 1:dim(const)[1]


plot(const)


## create adjacne
const_nb <- poly2nb(const, snap=1e-03, row.names = const$map_id)
const_coords <- coordinates(const)
plot(const_nb, coords=const_coords)
nb_B <- nb2listw(const_nb, style="B", zero.policy = T)
mat<-as(nb_B, "symmetricMatrix")

# undirected edges as pairs of nodes ii <-> jj
ii <- mat@i
jj <- mat@j

## create stan data -----------------------------------------------------


stan_data <- list(XX=XX,
                  vote=results_e_df$LabVote17,
#                  vote=results_e_df$ConVote17, 
                  electorate=results_e_df$Electorate17,
                  N=dim(XX)[1], 
                  n_covar=dim(XX)[2],
                  node_1 = ii + 1,
                  node_2 = jj + 1,
                  n_edges = length(ii))

# run model ------------------------------------------------------------

elec_model <- stan_model("stan/covariate_model_spatial.stan")

elec_fit <- sampling(elec_model, 
                     data=stan_data,
                     iter=1000, 
                     cores=3,
                     chains=3,
                     diagnostic_file="results/pois")


# General diagnostics --------------------------------------------------
stan_diag(elec_fit)
stan_diag(elec_fit, "stepsize")
stan_diag(elec_fit, "treedepth")
stan_diag(elec_fit, "divergence")
stan_rhat(elec_fit)
stan_trace(elec_fit, "lp__")

diag_df <- map_dfr(1:length(elec_fit@stan_args), read_diag, "pois")

upar <- diag_df %>% filter(iter>500) %>% select(beta_covar.1:sigma_phi)
upar %>% summarise_all(sd) %>% gather() %>% arrange(value)
masses <- get_mass_matrix(elec_fit)
arrange(masses, Inv_mass) %>% head(20)
all_cors <- cor(upar)

i1<-900
i2<-1074
corrplot(all_cors[i1:i2,i1:i2], method="color",tl.pos='n')
# corrs with sig phi
ind <-1074
which(all_cors[ind,]==min(all_cors[ind,-ind]))
prob_vars <- as.matrix(elec_fit, pars=c("sigma_phi", "const_sigma", "lp__")) %>% as_tibble()
ggplot(prob_vars, aes(x=log(sigma_phi), y=log(const_sigma), colour=lp__)) + 
  geom_point() + scale_colour_viridis_c()

# plot parameters ------------------------------------------------------
plot(elec_fit, pars=c("beta_covar"))
stan_trace(elec_fit, pars=c("beta_covar", "sigma_beta"))
stan_trace(elec_fit, pars=c("log_dispersion", "const_sigma","intercept"))
stan_trace(elec_fit, pars=c("inv_dispersion", "const_sigma","intercept"))
#pairs(elec_fit, pars=c("log_dispersion", "const_sigma","intercept"))
pairs(elec_fit, pars=c("log_dispersion", "const_sigma","intercept"))
pairs(elec_fit, pars=c("sigma_phi", "const_sigma","intercept"))
stan_trace(elec_fit, pars=c("sigma_phi", "const_sigma","intercept"))

stan_trace(elec_fit, pars=c("phi[1]", "phi[2]", "phi[3]"))
stan_trace(elec_fit, pars=c("phi[324]", "phi[241]", "phi[235]"))
pairs(elec_fit, pars=c("phi[324]", "phi[241]", "phi[235]"))
mcmc_nuts_energy(nuts_params(elec_fit))

# plot spatial bit ---------------------------------------------------

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

# plot non-spatial resid

eta <- as.matrix(elec_fit, "eta")
pred_vote <- apply(eta, 1, function(x) (x + log(stan_data$electorate)))

pred_vote_df <- pred_vote %>% as_tibble() %>% 
  mutate(Const_id = 1:stan_data$N) %>% 
  gather(Sim, Vote_pred, -Const_id)

pred_mean <- pred_vote_df %>% group_by(Const_id) %>% 
  summarise(Vote_estimate=mean(exp(Vote_pred))) %>%
  mutate(Vote=stan_data$vote,
         Residual=Vote_estimate - Vote)

ggplot(pred_mean, aes(x=Vote, y=Vote_estimate)) + geom_point() + 
  geom_abline(slope=1, intercept=0)


ggplot(pred_mean) + 
  geom_map(map=map_df,
           aes(map_id=Const_id,fill=Residual),
           colour="grey",size=0.1) + 
  expand_limits(x=map_df$long,y=map_df$lat) +
  coord_fixed() +
  scale_fill_viridis_c() + 
  transparent_theme +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )


## Age gam ---------------------------------------------------------------------

# read in age data 
pop_data <- read.csv(file.path("data","const_pop.csv")) %>% as_tibble() %>%
  filter(!(constituency_name == "Buckingham")) %>%
  filter(!grepl("Wight", constituency_name)) %>%
  filter(grepl("^E", ons_id)) %>%
  arrange(ons_id) 

stopifnot(all(pop_data$ons_id==results_e_df$ONSConstID))

pop_data %<>%
  select(-X, -ons_id, -constituency_name, 
         -All.Ages) %>% as.matrix()
# exclude age<17
pop_data <- pop_data[,18:dim(pop_data)[2]]

n_basis <- 10
N_age <- dim(pop_data)[2]
xx <- 1:N_age
knots <- seq(1, N_age, length.out=n_basis-2)
knots <- c(1 - 3:1*diff(knots)[1], knots, max(knots) + 1:3*diff(knots)[1])

plot_matrix(splineDesign(knots, xx))

age_basis <- splineDesign(knots, xx)

n_basis <- dim(age_basis)[2]

stan_data$age_basis <- age_basis
stan_data$N_basis <- n_basis
stan_data$pop_data <- pop_data
stan_data$N_age <- N_age

elec_model <- stan_model("stan/pois_model_age_1.stan")

elec_fit <- sampling(elec_model,
                     data=stan_data,
                     iter=1000, 
                     cores=3,
                     chains=3)

stan_trace(elec_fit, "age_beta")

smooth_age <- as.matrix(elec_fit, "smooth_age")
log_vote_x <- as.matrix(elec_fit, "log_vote_x")

