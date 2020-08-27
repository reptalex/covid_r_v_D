library(tidyverse)
library(COVID19)
library(padr)
library(lubridate)
library(progress)
library(RcppRoll)
library(parallel)


set.seed(195893)


source("scripts/utils.R")


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
world <- world %>% 
  group_by(id) %>% 
  pad() %>% 
  mutate(new_confirmed = confirmed - lag(confirmed), 
         new_deaths = deaths - lag(deaths)) %>% 
  ungroup() %>% 
  filter(new_confirmed >= 0) %>% 
  filter(date < Sys.Date()-days(1)) %>% # exclude last day as seems to have  reporting lag 
  dplyr::select(date, id, deaths, new_deaths, confirmed, new_confirmed, population, contains("administrative")) %>% 
  filter(!is.na(date))



# NBSS growth rate estimation -----------------------------------------------------------

## IF DISPERSIONS NEED TO BE CALCULATED
if (compute_dispersions){
  fits <- nbss(world, series="new_confirmed", mc.cores=7)
  fits <- nbss(fits, series="new_deaths", mc.cores=7)
  save(fits, file="fits.RData")
  fits %>%
    group_by(id) %>%
    summarise(dispersion=unique(dispersion)) %>%
    write_csv("data/precomputed_dispersion.csv")
  fits %>%
    group_by(id) %>%
    summarise(dispersion=unique(dispersion_deaths)) %>%
    write_csv("data/precomputed_dispersion_deaths.csv")
}
## IF PRECOMPUTED DISPERSIONS ARE PRESENT 
# Load pre-computed nb dispersion parameters
dispersions <- read.csv("data/precomputed_dispersion.csv")
dispersions_deaths <- read.csv("data/precomputed_dispersion_deaths.csv")

# run in parallel with pre-computed dispersions
fits <- nbss(world, mc.cores=7, precomputed_dispersions = dispersions)
fits <- nbss(fits, mc.cores=7, precomputed_dispersions = dispersions_deaths)


save(fits, file="data/fits.RData")
# load("fits.RData")



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
  write.csv('data/nbss_sweeden_states.csv')

fits[country=='United States' & administrative_area_level==3] %>%
  write.csV('data/nbss_us_counties.csv')