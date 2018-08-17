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

# get election results data from BES website
url <- paste0("http://www.britishelectionstudy.com/wp-content/uploads/2017/",
              "07/BES-2017-General-Election-results-file-v1.0.dta")

dest_path <- file.path("data/GE2017_results.dta")

curl_download(url=url, destfile = dest_path)

results_df <- haven::read_dta(file = dest_path)



# results


results_df %<>% mutate(Winner17=as_factor(Winner17))
results_df %>% group_by(Winner17) %>% summarise(Seats=n()) %>% arrange(-Seats)

# set traditional party colours
parties <- results_df %>%  select(Winner17) %>% unique() 

party_colours <- c( "blue", "red", "yellow", "orange","darkgreen",
                   "green","black")
party_colours <- setNames(party_colours, parties$Winner17)

# share vs change
ggplot(results_df , aes(x=Lab17,y=Lab1517)) + geom_point(colour="red") +
  theme_minimal()
ggplot(results_df , aes(x=Con17,y=Con17 - Con15),colour="blue") + geom_point()

party_prefixs <- c("Con","Lab", "SNP", "LD", "PC", "Green", "UKIP", "Other")
nice_variables <-   c("ONSConstID", "ConstituencyName",
                      "Country",  "Region", "ConstituencyType",
                      "Winner17") 
share_df <- results_df %>% select(paste0(party_prefixs, 17), nice_variables)

# incumbent
ggplot(results_df %>% filter(party_name %in% c("Labour","Conservative")), 
       aes(x=share,y=change, colour=sitting_mp)) + geom_point() 
  
## get ons midyear pop estimates, estimated by constituency for single year of age.

url <- paste0("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
              "populationandmigration/populationestimates/datasets/",
              "parliamentaryconstituencymidyearpopulationestimates/",
              "mid2016sape19dt7/",
              "sape19dt7mid2016parliconsyoaestimatesunformatted.zip")

dir.create("data")
dest_file <- "data/constituency_pop.zip"
curl_download(url, dest_file)
unzip(dest_file, exdir="data")

# sheets
# 2 both sexes
# 3 males
# 4 females
pop_constituency <- read.xlsx(file.path("data",
                              "SAPE19DT7-mid-2016-parlicon-syoa-estimates-unformatted.xls"),
                              sheetIndex = 2,startRow = 5)

pop_constituency_m <- read.xlsx(file.path("data",
                                        "SAPE19DT7-mid-2016-parlicon-syoa-estimates-unformatted.xls"),
                              sheetIndex = 3,startRow = 5)

pop_constituency_f <- read.xlsx(file.path("data",
                                          "SAPE19DT7-mid-2016-parlicon-syoa-estimates-unformatted.xls"),
                                sheetIndex = 4,startRow = 5)


pop_constituency %<>% rbind(pop_constituency %>% mutate(Sex="Total"),
                           pop_constituency_m %>% mutate(Sex="Male"),
                           pop_constituency_f %>% mutate(Sex="Female"))


# tidy data format
pop_constituency_df <- pop_constituency %>% as_tibble() %>% 
  rename(ons_id=PCON11CD,constituency_name=PCON11NM) %>% 
  arrange(ons_id)

#  gather(key=Age,value=Population, -ons_id,-constituency_name, -All.Ages,-Sex) %>%
#  mutate(Age = as.numeric(gsub("X","", Age)))
    
pop_constituency_df 

write.csv(pop_constituency_df,file=file.path("data","const_pop.csv"))


## other data... 
# see  https://www.ons.gov.uk/methodology/methodologicalpublications/generalmethodology/ukparliamentaryconstituencies/datacatalogueforparliamentaryconstituencies


# constituency maps
dir.create(file.path("data","mapping"))

url <- paste0("https://opendata.arcgis.com/datasets/",
              "5c582cef61d04618928639dd17e4f896_4.zip?",
              "outSR=%7B%22wkid%22%3A4326%2C%22latestWkid%22%3A4326%7D")
dest_file <- file.path("data", "mapping", "constituency.zip")
curl_download(url, destfile = dest_file )
unzip(dest_file, exdir = "data/mapping")
shape_file_name <- list.files(file.path("data", "mapping"), pattern="*.shp")
const <-readOGR(file.path("data", "mapping",shape_file_name),stringsAsFactors = F)


bbc_EU_ward_results_url <- paste0("https://3859gp38qzh51h504x6gvv0o-wpengine.netdna-ssl.com/",
                                  "files/2017/02/ward-results.xlsx")

eu_df <- read.xlsx("data/eu_ward_level.xlsx",sheetIndex = 1)

# deaths




