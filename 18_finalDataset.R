# 18_finalDataset
# get final dataset numbers

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
library(fs)

set.seed(10)
HEXAGON_PATH <- path("data/route_hex.rds")
SPECIES_DATA_PATH <- path("data/src/sps_list.csv")
FIPSROU_DATA_PATH <- path("data/RouteFips.csv")

sps_list <- read_csv(SPECIES_DATA_PATH)
route_hex <- read_rds(HEXAGON_PATH)

roufip <- read_csv(FIPSROU_DATA_PATH, col_types = list(col_double(), col_character()))

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

BIRD %>% 
  group_by(SpeciesCode) %>% 
  summarise(Total = sum(SpeciesTotal))
BIRD %>% 
  summarise(Total = sum(SpeciesTotal)) 

table(BIRD$SpeciesCode)
sum(table(BIRD$SpeciesCode))

BIRD %>% 
  dplyr::select(SpeciesCode, RouteId) %>% 
  distinct() %>% 
  group_by(SpeciesCode) %>% 
  summarise(routes = n())

length(unique(BIRD$RouteId))

length(unique(BIRD$Year))

BIRD2 <- BIRD %>% 
  distinct() %>% 
  dplyr::select(RouteId, YearInfested) %>% 
  distinct()

fipinf <- left_join(BIRD2, route_hex, by = "RouteId") %>% 
  dplyr::select(RouteId, YearInfested)

fipinf <- left_join(fipinf, roufip, by = "RouteId")
