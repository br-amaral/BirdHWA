library(tidyverse)
library(hablar)

# files I need: BirdHWA, infestations
BirdHWA <- readRDS('data/BirdHWA.rds') 
infestations <- readRDS('data/infestations.rds')

## Filter: locations  --------------------
# routes only in counties with hemlock trees
BirdHWA_f <- BirdHWA %>% 
  filter(RouteId %in% infestations$RouteId)

## Filter: only one type of observation --------------------
# keep only one row for each routeXday
# RPID order of keepers: 101 102 103 203

BirdHWA_f2 <- BirdHWA_f %>% 
  group_by(RouteId, Year, SpeciesId) %>%
  filter(ObsType == min(ObsType)) %>% 
  slice(1) %>%   # takes the first occurrence if there is a tie
  ungroup()

## Create new columns  --------------------
# yrhwa and first time observer
BirdHWA_n3 <- BirdHWA_f2 %>% 
  mutate(yrhwa = Year - YearInfested,
         ObserverRoute = paste(RouteId,ObserverId, sep="_")) %>% 
  rename(ObserverType = ObsType) %>% 
  arrange(RouteId,Year)

first_obs <- BirdHWA_n3 %>% 
  select(RouteId,Year,ObserverId) %>%
  group_by(ObserverId) %>% 
  filter(Year == min(Year)) %>% 
  slice(1) %>%   # takes the first occurrence if there is a tie
  ungroup() %>%
    mutate(NewObserver = T)
   
BirdHWA_n4 <- left_join(BirdHWA_n3,first_obs,
                        by= c('RouteId','Year','ObserverId')) %>% 
  mutate(NewObserver = replace_na(NewObserver, F)) %>% 
  relocate(RouteId, StateNum, Route, Year,
           SpeciesId, SpeciesCode, SpeciesName, SpeciesSciName,SpeciesTotal,
           Infested, YearInfested, yrhwa,
           Latitude, Longitude,
           ObserverId, ObserverType, ObserverRoute, NewObserver)

for(i in 1:nrow(BirdHWA_n4)){
  if(!is.finite(BirdHWA_n4$YearInfested[i])) {BirdHWA_n4$YearInfested[i] <- 0}
  if(!is.finite(BirdHWA_n4$Infested[i])) {BirdHWA_n4$Infested[i] <- 0}
  if(!is.finite(BirdHWA_n4$yrhwa[i])) {BirdHWA_n4$yrhwa[i] <- 0}
}

BirdHWA_2 <- BirdHWA_n4
  
write_rds(BirdHWA_2, file = "data/BirdHWA_2.rds") 
