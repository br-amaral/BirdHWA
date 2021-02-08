library(purrr)
library(dplyr)

# Create individual species data.frames to run the models
SPECIES_DATA_PATH <- "data/src/sps_list.csv"
BIRD_FILE_PATH <- "data/BirdHWA_2.rds"

sps_list <- read_csv(SPECIES_DATA_PATH)
BIRD <- readRDS(BIRD_FILE_PATH)

single_sps <- function(species){
  ## select a species and keep undetected records --------------------
  route_range <- BIRD %>% 
    filter(SpeciesCode == species) %>% 
    dplyr::select(RouteId) %>% 
    unique() %>% 
    arrange()
  
  BIRD2 <- BIRD %>% 
    filter(RouteId %in% as_vector(route_range)) %>% 
    filter(SpeciesCode == species | is.na(SpeciesCode))
  
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

