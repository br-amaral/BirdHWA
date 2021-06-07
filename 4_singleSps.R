# 4_singleSps

# Input: 
#        
# Output: 
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
    stop("error in line 51!")
  }
  
  BIRD2 <- rbind(BIRD2a, BIRD2b) %>% 
    arrange(RouteId, Year)
  
  ## standardize temperature  -----------------
  # mean by species, sd for all data (calculated in combineData.R)
  BIRD3 <- BIRD2 %>% 
    mutate(min_tempMe = mean(BIRD2$minTemp),
           mean_tempMe = mean(BIRD2$meanTemp),
           temp_min_scale = (minTemp - min_tempMe)/sd_tempMi,
           temp_mean_scale = (meanTemp - mean_tempMe)/sd_tempMe)
 
  filename <- paste("data/species/", species, ".rds",sep= "")
  
  hist(BIRD3$temp_min_scale)
  plot(BIRD3$temp_min_scale, BIRD3$minTemp)
    
  write_rds(BIRD3, file = filename)
  
}

invisible(lapply(sps_list$SpeciesCode, single_sps))

