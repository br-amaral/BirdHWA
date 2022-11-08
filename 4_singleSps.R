# 4_singleSps ---------------------------------------------------------------------------
# get data for only the species that I'm interested in analyzing, and add no detections (zeros) for years
#   where the species was not found but was previously found, and create species specific tibbles
#
# Input: 
#   data/src/sps_list.csv: list of hemlock associates and control species used in the analysis
#   data/BirdHWA_2.rds: bird dataset with info from BBS and HWA created by 3_combineData       
#
# Output: 
#   data/species/{species}.rds: BirdHWA for each species with zeros
#

library(purrr)
library(tidyverse)

# Create individual species data.frames to run the models
SPECIES_DATA_PATH <- "data/src/sps_list.csv"
BIRD_FILE_PATH <- "data/BirdHWA_2.rds"

sps_list <- read_csv(SPECIES_DATA_PATH)
BIRD <- readRDS(BIRD_FILE_PATH)

single_sps <- function(species){
  ## select a species and keep undetected records --------------------
  BIRD2a <- BIRD %>% 
    filter(SpeciesCode == species)
  
  route_range_year <- BIRD2a %>% 
    select(RouteId, Year) %>% 
    mutate(RouteIdYr = paste0(RouteId, as.character(Year))) %>% 
    unique() %>% 
    arrange()
   
  BIRD2b <- BIRD %>% 
    filter(RouteId %in% as_vector(route_range_year$RouteId)) %>% 
    mutate(RouteIdYr = paste0(RouteId, as.character(Year))) %>% 
    filter(!(RouteIdYr %in% route_range_year$RouteIdYr)) %>% 
    filter(SpeciesCode != species) %>% 
    mutate(SpeciesId = NA,  
           SpeciesCode = NA,
           SpeciesName = NA,
           SpeciesSciName = NA,
           SpeciesTotal = 0) %>% 
    distinct(RouteId, StateNum, Route, Year, 
             SpeciesId, SpeciesCode, SpeciesName, SpeciesSciName, SpeciesTotal,
             Infested, YearInfested, yrhwa,
             Latitude, Longitude, 
             #ObserverId, ObserverType, ObserverRoute, NewObserver,
             minTemp, meanTemp, hexID, sd_tempMi, sd_tempMe,
             .keep_all = T) %>% 
    select(-RouteIdYr)

  add_nodetcs <- left_join(route_range_year, BIRD2b, by= c("RouteId", "Year")) %>% 
    relocate(colnames(BIRD2a))
  
  if(nrow(route_range_year) != nrow(add_nodetcs)) {
    stop("error in line 55!")
  }
  
  BIRD2 <- rbind(BIRD2a, BIRD2b) %>% 
    arrange(RouteId, Year)
  
  BIRD2uni <- BIRD2 %>% 
    select(RouteId, minTemp, meanTemp) %>% 
    distinct()
  
  ## standardize temperature  -----------------
  # mean by species, sd for all data (calculated in combineData.R)
  BIRD3 <- BIRD2 %>% 
    mutate(min_tempMe = mean(BIRD2uni$minTemp),
           mean_tempMe = mean(BIRD2uni$meanTemp),
           temp_min_scale = (minTemp - min_tempMe)/sd_tempMi,
           temp_mean_scale = (meanTemp - mean_tempMe)/sd_tempMe)
 
  filename <- paste("data/species/", species, ".rds",sep= "")
  par(mfrow=c(1,2))
  hist(BIRD3$temp_min_scale)
  plot(BIRD3$temp_min_scale, BIRD3$minTemp)
    
  write_rds(BIRD3, file = filename)
  
}

invisible(lapply(sps_list$SpeciesCode, single_sps))

