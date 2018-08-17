library(maptools)

library(spdep)
#install.packages("INLA", repos=c(getOption("repos"), 
#                                 INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)

library(tidyr)
library(dplyr)
library(magrittr)
library(purrr)
library(ggplot2)

library(tibble)
library(haven)
library(rgdal) 
library(spdep)

source("R/utility.R")

shape_file_name <- list.files(file.path("data", "mapping"), 
                              pattern="*.shp")
const <- readOGR(file.path( "data", "mapping",shape_file_name),
                 stringsAsFactors = F)

# exclude NI and arrange alphabetical
const <- const[!grepl("^N",const$pcon17cd),]
const <- const[!grepl("Buckingham",const$pcon17nm),]
const <- const[order(const$pcon17cd),]
const$map_id <- 1:631
data_path <- file.path("data/GE2017_results.dta")
results_df <- haven::read_dta(file = data_path)
results_df %<>% mutate(Winner17=as_factor(Winner17)) 
results_df %<>% filter(ConstituencyName!="Buckingham")
results_df %<>% arrange(ONSConstID) %>% 
  mutate(map_id = 1:631)

plot(const)
const_coords <- coordinates(const)

const_nb <- poly2nb(const,snap=1e-03, row.names = const$map_id)
plot(const_nb, coords=const_coords)
graph_file <- "data/const_adj.graph"
nb2INLA(graph_file, const_nb)


H <- inla.read.graph(filename=graph_file)
plot(H)

mod_tes <- inla(LabVote17 ~ 1  +  f(map_id,
                                    model="bym",
                                    graph=graph_file,
                                    scale.model = T),
                data=results_df,
                family="poisson",
                verbose=F, 
                E=Electorate17,
                control.predictor = list(compute=T))
summary(mod_tes)

get_prop_spat_var <- function(mod){
  marg_hyper <- inla.hyperpar.sample(100000, mod)
  prop_spat_var <- marg_hyper[,1] / (marg_hyper[,1] + marg_hyper[,2])
  return(prop_spat_var)
}
get_prop_spat_var(mod_tes) %>% mean()

results_df %<>% mutate(Lab_electorate_share_17 = LabVote17 / Electorate17)
plot(mod_tes$summary.fitted.values$mean, results_df$Lab_electorate_share_17)

mod_chg <- inla(LabVote17 ~ 1  + LabVote15
                            + f(map_id,model="bym", graph=graph_file),
                data=results_df,
                family="poisson", 
                E=Electorate17)
summary(mod_chg)
get_prop_spat_var(mod_chg) %>% mean()

mod_age <- inla(LabVote17 ~ 1  + c11Age18to19 + c11Age20to24 + c11Age25to29 +
                                 c11Age65to74 + c11Age30to44
                + f(map_id,model="bym", graph=graph_file,
                    scale.model = T),
                data=results_df,
                family="poisson", 
                E=Electorate17,
                control.compute = list(dic=T))
summary(mod_age)
n_areas <- length(const)
get_prop_spat_var(mod_age) %>% mean()
csi <- mod_age$marginals.random$map_id[1:n_areas]
zeta <- lapply(csi, function(x) inla.emarginal(exp,x))
csi <- mod_age$marginals.random$map_id[(n_areas+1):(n_areas*2)]
spat_1 <- map_dbl(csi, function(x) inla.emarginal(exp,x))

map_df <- fortify(const, region="map_id")
  
results_df %<>% mutate(`Spatial residual`=spat_1)



ggplot(results_df) + 
  geom_map(map=map_df,
           aes(map_id=map_id,fill=LabVote17)) + 
  expand_limits(x=map_df$long,y=map_df$lat) +
  coord_fixed() +
  scale_fill_gradient(low="black", high="red") + 
  transparent_theme +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  ) + 
  labs(fill="Labour Vote")


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


mod_all <- inla(LabVote17 ~ 1  + c11Age18to19 + c11Age20to24 + c11Age25to29 +
                  c11Age65to74 + c11Age30to44 + c11HouseOwned + c11HouseSocial + 
                  c11Deprived4 + c11IndustryMining + c11EthnicityBlack +
                  c11Degree + c11NSSECRoutine + c11QualNone + 
                  c11NSSECHigherProfessional + 
                  c11EthnicityAsian
                + f(map_id,model="bym", graph=graph_file,
                    scale.model = T),
                data=results_df,
                family="poisson", 
                E=Electorate17,
                control.compute = list(dic=T))

get_prop_spat_var(mod_all) %>% mean()
csi <- mod_all$marginals.random$map_id[(n_areas+1):(n_areas*2)]
spat_2 <- map_dbl(csi, function(x) inla.emarginal(exp,x))

results_df %<>% mutate(`Spatial residual \n Model 2`=spat_2)

ggplot(results_df) + 
  geom_map(map=map_df,
           aes(map_id=map_id,fill=`Spatial residual \n Model 2`),
           colour="grey",size=0.1) + 
  expand_limits(x=map_df$long,y=map_df$lat) +
  coord_fixed() +
  scale_fill_gradient2(low="black",mid="white", high="red",midpoint =1) +
  transparent_theme +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )


#---
  
centr <- gCentroid(map_df, byid = TRUE)

# create SpatialPointsDataFrame to export via writeOGR
# positive side effect: All data from landuse@data joined to centr@data
centr <- SpatialPointsDataFrame(centr, data= landuse@data) 





