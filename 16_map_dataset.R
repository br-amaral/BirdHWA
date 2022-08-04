# xx_ get final dataset

# Input: 
#        /data/hexmap.graph
#        data/src/sps_list.csv  
#        data/species/{species}.rds
#        5_formulasModels.R (sourcing)       
# Output: 
#        data/models_res/{species} (folder)
#        data/models_res/{species}/{name}.rds (files)

library(INLA)
library(tidyverse)
library(glue)

set.seed(10)

SPECIES_DATA_PATH <- "data/src/sps_list.csv"
sps_list <- read_csv(SPECIES_DATA_PATH)

finaldat <- function(species){
  SPECIES_MOD_DAT <- glue("data/species/{species}.rds")
  BIRDx <- readRDS(SPECIES_MOD_DAT)
  ## Create an year offset for that species ------------------  
  BIRDx <- BIRDx %>%  
    # remove 20 ears before and after infestation
    mutate(year_offset = ifelse(YearInfested != 0, Year - YearInfested, 0)) %>% 
    filter(year_offset > -20 & year_offset < 20) %>% 
    # Only routes infested for at least 10 years
    group_by(RouteId) %>% 
    mutate(max = max(year_offset)) %>%  
    filter(max > 9) %>% 
    ungroup() 
  
  return(BIRDx)
}

BIRD <- as.data.frame(matrix(NA, ncol = 29, nrow = 0)) %>% 
  as_tibble()

for(i in 1:length(sps_list$SpeciesCode)) {
 
  BIRD <- rbind(BIRD,finaldat(sps_list$SpeciesCode[i]))
  
}

BIRD2 <- BIRD %>% 
  distinct() %>% 
  dplyr::select(RouteId, YearInfested) %>% 
  distinct()



