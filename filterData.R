library(tidyverse)
library(hablar)

# files I need: BirdHWA, infestations

## Filter: locations  --------------------
# routes only in counties with hemlock trees
BirdHWA_f <- BirdHWA %>% 
  filter(RouteId %in% infestations$RouteId)

## Filter: only selected species --------------------





## Filter: only one type of observation --------------------
# keep only one row for each routeXday
# RPID order of keeper: 101 102 103 203

BirdHWA_f2dup <- BirdHWA_f %>%
  find_duplicates(RouteId, Year, SpeciesId)

BirdHWA_f2unique <- setdiff(BirdHWA_f, BirdHWA_f2dup)

if(nrow(BirdHWA_f) != sum(nrow(BirdHWA_f2unique), nrow(BirdHWA_f2dup))){stop("problem in line 26")}

BirdHWA_f2dup <- BirdHWA_f2dup %>% 
  arrange(SpeciesCode,Year,RouteId,ObsType) 
BirdHWA_f2dup_2 <- BirdHWA_f2dup[!duplicated(BirdHWA_f2dup[,c(1,5,6)]),]

BirdHWA_f2 <- rbind(BirdHWA_f2unique,BirdHWA_f2dup_2)

## Create new columns  --------------------
# yrhwa and first time observer
BirdHWA_n3 <- BirdHWA_f2 %>% 
  mutate(yrhwa = Year - YearInfested,
         Observer_route = paste(RouteId,ObserverId, sep="_")) %>% 
  rename(ObserverType = ObsType) %>% 
  arrange(RouteId,Year)

first_obs <- BirdHWA_n3 %>% 
  select(RouteId,Year,ObserverId) %>%
  group_by(ObserverId) %>% 
  filter(Year == min(Year)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>%
    mutate(NewObserver = T)
   
BirdHWA_n4 <- left_join(BirdHWA_n3,first_obs,
                        by= c('RouteId','Year','ObserverId')) %>% 
  mutate(NewObserver = replace_na(NewObserver, F)) %>% 
  relocate(RouteId, StateNum, Route, Year,
           SpeciesId, SpeciesCode, SpeciesName, SpeciesSciName,SpeciesTotal,
           Infested, YearInfested, yrhwa,
           Latitude, Longitude,
           ObserverId, ObserverType, Observer_route, NewObserver)

