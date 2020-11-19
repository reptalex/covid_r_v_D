library(tidyverse)
library(COVID19)
library(padr)
library(lubridate)
library(progress)
library(RcppRoll)
library(parallel)
library(data.table)
source("scripts/utils.R")

set.seed(195893)

compute_dispersions=TRUE



# load data ---------------------------------------------------------------

# Get all countries
world <- covid19()

# Get state level for specific countries
ssc <- c("South Africa", "India", "Sweden", "United States", "Canada", 
         "Switzerland", "Australia", "Italy", "United Kingdom", "Brazil")
regions_to_exclude <- c("Montserrat", "Turks and Caicos Islands", 
                        "Falkland Islands (Malvinas)", "British Virgin Islands", 
                        "Appenzell Innerrhoden", "Yukon", "Prince Edward Island", 
                        "Mizoram", "Benin", 
                        "Brunei", "Comoros", "Dominica", "Eritrea", 
                        "Fiji", "Grenada", "Holy See", "Northern Mariana Islands", 
                        "Saint Lucia", "Seychelles", "Western Sahara", "Virgin Islands") 
states <- covid19(country=ssc, level=2) %>% 
  filter(!(administrative_area_level_1 %in% regions_to_exclude)) %>% 
  filter(!(administrative_area_level_2 %in% regions_to_exclude))

# Counties within the US
counties <- covid19(country="United States", level=3)

# bind levels together and world level data together to process together 
world <- bind_rows(world, states, counties)

# Process data
world <- as.data.table(world)
setkey(world,id,date)
world[,new_confirmed:=(confirmed - lag(confirmed)),by=id]
world[,new_deaths:=(deaths - lag(deaths)),by=id]
world <- world [!is.na(new_deaths) & !is.na(new_confirmed)]
world$new_confirmed <- ifelse(world$new_confirmed < 0, NA, world$new_confirmed)
world$new_deaths <- ifelse(world$new_deaths < 0, NA, world$new_deaths)
world <- world[!is.na(date)][date< Sys.Date()-days(1)]
world <- as.data.frame(world)

world <- dplyr::select(world,date, tests, id, hosp, deaths, new_deaths, confirmed, new_confirmed, population, contains("administrative"))
world <- as.data.frame(world)


# NBSS growth rate estimation -----------------------------------------------------------

# debugging
# world <- world %>% 
#   filter(administrative_area_level_1=="United States", administrative_area_level==2)
####

## IF DISPERSIONS NEED TO BE CALCULATED
if (compute_dispersions){
  fits <- covid19_nbss(world, series="new_confirmed", mc.cores=1)
  fits <- covid19_nbss(fits, series="new_deaths", mc.cores=1)
  
  # Consolidate error codes
  fits <- fits %>% 
    mutate(error = paste(error, `error...30`, `error...31`, sep=" / ")) %>% 
    select(-`error...30`, -`error...31`) %>% 
    mutate(error = ifelse(error == "NA / NA / NA", NA, error))

  save(fits, file="fits.RData")
  fits %>%
    group_by(id) %>%
    summarise(dispersion=unique(dispersion)) %>%
    write_csv("data/precomputed_dispersion.csv")
  fits %>%
    group_by(id) %>%
    summarise(dispersion=unique(dispersion_deaths)) %>%
    write_csv("data/precomputed_dispersion_deaths.csv")
} else {
## IF PRECOMPUTED DISPERSIONS ARE PRESENT 
# Load pre-computed nb dispersion parameters
  dispersions <- read.csv("data/precomputed_dispersion.csv")
  dispersions_deaths <- read.csv("data/precomputed_dispersion_deaths.csv")
  
  # run in parallel with pre-computed dispersions
  fits <- covid19_nbss(world, mc.cores=1, precomputed_dispersions = dispersions)
  fits <- covid19_nbss(fits,series='new_deaths', mc.cores=1, precomputed_dispersions = dispersions_deaths)
  
}
save(fits, file="data/fits.RData")
# load("data/fits.RData")


# Write important chunks into csv ---------------------------------------------

fits <- as.data.table(fits)
fits[,country:=administrative_area_level_1]
fits[,state:=administrative_area_level_2]
fits[,county:=administrative_area_level_3]
fits[,deaths_pc:=deaths/population]


fits[administrative_area_level==1] %>%
  write.csv('data/nbss_countries.csv')

fits[country=='United States' & administrative_area_level==2] %>%
  write.csv('data/nbss_us_states.csv')

fits[country=='Sweden' & administrative_area_level==2] %>%
  write.csv('data/nbss_sweden_states.csv')

fits[country=='United States' & administrative_area_level==3] %>%
  write.csv('data/nbss_us_counties.csv')
