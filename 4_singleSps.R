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
  route_range <- BIRD %>% 
    filter(SpeciesCode == species) %>% 
    select(RouteId) %>% 
    unique() %>% 
    arrange()
  
  BIRD2a <- BIRD %>% 
    filter(RouteId %in% as_vector(route_range)) %>% 
    filter(SpeciesCode == species)
  
  BIRD2b <- BIRD %>% 
    filter(RouteId %in% as_vector(route_range)) %>% 
    filter(SpeciesCode != species) %>% 
    mutate(SpeciesId = NA,  
           SpeciesCode = NA,
           SpeciesName = NA,
           SpeciesSciName = NA,
           SpeciesTotal = 0) %>% 
    distinct(RouteId, StateNum, Route, Year, 
             SpeciesId,SpeciesCode, SpeciesName, SpeciesSciName, SpeciesTotal,
             Infested, YearInfested, yrhwa,
             Latitude, Longitude, 
             #ObserverId, ObserverType, ObserverRoute, NewObserver,
             minTemp, meanTemp, hexID, sd_tempMi, sd_tempMe,
             .keep_all = T)

  year_route_samp <- BIRD2a %>% 
    select(RouteId,Year) %>% 
    distinct()

  add_nodetcs <- left_join(year_route_samp, BIRD2b, by= c("RouteId", "Year")) %>% 
    relocate(colnames(BIRD2a))
  
  if(nrow(year_route_samp) != nrow(add_nodetcs)) {
    stop("error in line 44!")
  }
  
  BIRD2 <- rbind(BIRD2a, BIRD2b) %>% 
    arrange(RouteId, Year)
  
  ## standardize temperature  -----------------
  # mean by species, sd for all data (calculated in combineData.R)
  BIRD3 <- BIRD2 %>% 
    mutate(min_tempMe = mean(minTemp),
           mean_tempMe = mean(meanTemp),
           temp_min_scale = (minTemp - min_tempMe)/sd_tempMi,
           temp_mean_scale = (meanTemp - mean_tempMe)/sd_tempMe)
 
  filename <- paste("data/species/", species, ".rds",sep= "")
    
  write_rds(BIRD3, file = filename)
  
}

invisible(lapply(sps_list$SpeciesCode, single_sps))

