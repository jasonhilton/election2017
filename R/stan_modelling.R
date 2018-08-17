# starting with nonspatial model. 

library(tidyr)
library(dplyr)
library(magrittr)
library(purrr)
library(ggplot2)

library(rstan)
library(ggfan)

data_path <- file.path("data/GE2017_results.dta")
results_df <- haven::read_dta(file = data_path)

results_df %<>% filter(ConstituencyName!="Buckingham")
results_df %<>% arrange(ONSConstID) %>% 
  mutate(map_id = 1:631)


results_df %<>% select(ONSConstID, map_id,ConstituencyName,
                       Country, Region, ConstituencyType,
                       Con17, Lab17, LD17, SNP17, PC17, 
                       UKIP17, Green17, Other17, Electorate17) %>%
  filter(grepl("E", ONSConstID)) ## England only for now

results_df %<>% select(-SNP17, PC18)
results_df %>% select(Con17,Lab17, LD17, UKIP17)

pop_data <- read.csv(file.path("data","const_pop.csv")) %>% as_tibble() %>%
  filter(!(constituency_name == "Buckingham"))

pop_data %<>% select(-X, -ons_id, -constituency_name, 
                     -All.Ages) %>% as.matrix()
# exclude age<17
pop_data <- pop_data[,18:dim(pop_data)[2]]


N_constit <- dim(pop_data)[1]
N_age <- dim(pop_data)[2]
n_cats <- 

stan_data()





