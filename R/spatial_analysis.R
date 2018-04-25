library(maptools)

library(spdep)
#install.packages("INLA", repos=c(getOption("repos"), 
#                                 INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)


shape_file_name <- list.files(file.path("data", "mapping"), 
                              pattern="*.shp")
const <- readOGR(file.path( "data", "mapping",shape_file_name),
                 stringsAsFactors = F)

# exclude NI and arrange alphabetical
const <- const[!grepl("^N",const$pcon17cd),]
const <- const[order(const$pcon17nm),]
const$map_id <- 1:632
results_df %<>% arrange(ConstituencyName) %>% 
  mutate(map_id = 1:632)

plot(const)
const_coords <- coordinates(const)

const_nb <- poly2nb(const,snap=1e-03)
plot(const_nb, coords=const_coords)
graph_file <- "data/const_adj.graph"
nb2INLA(graph_file, const_nb)


H <- inla.read.graph(filename=graph_file)
plot(H)

mod_tes <- inla(LabVote17 ~ 1  +  f(map_id,model="bym", graph=graph_file),
                data=results_df,
                family="poisson",
                verbose=F, 
                E=Electorate17,
                control.predictor = list(compute=T))
summary(mod_tes)

this <-lapply(mod_tes$marginals.random$map_id[1:632], 
              function(x) inla.emarginal(exp,x))
plot(results_df$LabVote17, unlist(this))

results_df %<>% mutate(Lab_electorate_share_17 = Lab17 / Electorate17)


mod_chg <- inla(LabVote17 ~ 1  + LabVote15
                            + f(map_id,model="bym", graph=graph_file),
                data=results_df,
                family="poisson", 
                E=Electorate17)
summary(mod_chg)


mod_age <- inla(LabVote17 ~ 1  + c11Age20to24 + c11Age65to74
                + f(map_id,model="bym", graph=graph_file),
                data=results_df,
                family="poisson", 
                E=Electorate17)
summary(mod_age)

