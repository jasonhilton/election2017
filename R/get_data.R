library(curl)
library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(xlsx)
library(tibble)
library(haven)

# can be tricky to install. requires sudo apt-get install libgdal1-dev libproj-dev
library(rgdal) 

# get election results data from HOC library website
url <- paste0("http://www.britishelectionstudy.com/wp-content/uploads/2017/",
              "07/BES-2017-General-Election-results-file-v1.0.dta")

dest_path <- file.path("data/GE2017_results.dta")

curl_download(url=url, destfile = dest_path)

results_df <- haven::read_dta(file = dest_path)

results_df <- read.csv(dest_path,stringsAsFactors = F)


# results
results_df %>% group_by(Winner17) %>% summarise(Seats=n()) %>% arrange(-Seats)

results_df %<>% mutate(Winner17=as_factor(Winner17))
# set traditional party colours

parties <- results_df %>%  select(Winner17) %>% unique() 

party_colours <- c("red", "blue", "yellow", "darkgreen","orange",
                   "green","black")
party_colours <- setNames(party_colours, parties$Winner17)

# share vs change
ggplot(results_df, aes(x=share,y=change, colour=party_name)) + geom_point() + 
  scale_colour_manual(values=party_colours)

# incumbent
ggplot(results_df %>% filter(party_name %in% c("Labour","Conservative")), 
       aes(x=share,y=change, colour=sitting_mp)) + geom_point() 
  
## get ons midyear pop estimates, estimated by constituency for single year of age.

url <- paste0("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
              "populationandmigration/populationestimates/datasets/",
              "parliamentaryconstituencymidyearpopulationestimates/",
              "mid2015sape18dt7/sape18dt7mid2015parliconsyoaestimates.zip")

dest_file <- "data/constituency_pop.zip"
curl_download(url, dest_file)
unzip(dest_file, exdir="data")

pop_constituency <- read.xlsx(file.path("data",
                              "SAPE18DT7-mid-2015-parlicon-syoa-estimates.xls"),
                              sheetIndex = 2,startRow = 4)

# tidy data format
pop_constituency_df <- pop_constituency %>% as_tibble() %>% 
  rename(ons_id=PCON11CD,constituency_name=PCON11NM) %>%
  gather(key=Age,value=Population, -ons_id,-constituency_name, -All.Ages) %>%
  mutate(Age = as.numeric(gsub("X","", Age)))
    
pop_constituency_df 

write.csv(pop_constituency_df,file=file.path("data","const_pop.csv"))


## other data... 
# see  https://www.ons.gov.uk/methodology/methodologicalpublications/generalmethodology/ukparliamentaryconstituencies/datacatalogueforparliamentaryconstituencies


# constituency maps
dir.create(file.path("data","mapping"))
url <- paste0("http://geoportal.statistics.gov.uk/datasets/",
              "b0f309e493cf4b9ba0d343eebb97b5ee_3.geojson")
dest_file <- file.path("data", "mapping", "constituency.geojson")
curl_download(url, destfile = dest_file )
const <-readOGR(dest_file , "OGRGeoJSON")
