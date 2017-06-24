library(curl)
library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(xlsx)
library(tibble)

# can be tricky to install. requires sudo apt-get install libgdal1-dev libproj-dev
library(rgdal) 

# get election results data from HOC library website
url <- paste0("http://researchbriefings.files.parliament.uk/",
              "documents/CBP-7979/hocl-ge2017-results-full.csv")

dest_path <- "data/GE2017_results.csv"

curl_download(url=url,destfile = dest_path)

results_df <- read.csv(dest_path,stringsAsFactors = F)

#  find the winner in each seat
winners_df <- results_df %>% group_by(ons_id) %>% filter(share==max(share))

# party name splits out lab/co-operative and lab seperately, although in 
# practice they are one party

winners_df <- winners_df %>% 
  mutate(party_name = ifelse(party_name == "Labour and Co-operative", 
                             "Labour", party_name))
# results
winners_df %>% group_by(party_name) %>% summarise(Seats=n()) %>% arrange(-Seats)

# apply this recode to the full df also
results_df <- results_df %>% 
  mutate(party_name = ifelse(party_name == "Labour and Co-operative", 
                             "Labour", party_name))
# set traditional party colours

parties <- winners_df %>% ungroup() %>%  select(party_name) %>% unique()
parties
party_colours <- c("red", "blue", "yellow", "darkgreen","orange",
             "purple", "lightgreen", "green","black","grey")
party_colours <- setNames(party_colours, parties$party_name)

# share vs change
ggplot(winners_df, aes(x=share,y=change, colour=party_name)) + geom_point() + 
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
