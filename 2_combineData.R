# 2_combineData ----------------------------------      
# R code to combine the BBS database with the HWA database. BBS data from all 22 states with the Eastern hemlock trees were used (Alabama,
#  Connecticut, Delaware, Georgia, Kentucky, Maine, Maryland, Massachusetts, Michigan, North Carolina, New Hampshire, New Jersey,
#  New York, Ohio, Rhode Island, South Carolina, Tennessee, Vermont, Virginia, West Virginia, Washington, Wisconsin). Hemlock wooly adelgid 
#  infestation data were extracted from a shape file with county level and year information about adelgid status.
#
# Input: 
#   StateData: folder with .cvs matrices from BBS with bird data for individual states from BBS
#   infestations.rds: routes with information of when they were infested
#   weather.csv: matrix with information regarding the observer
#   route_coor.csv: matrix with xy coordinates of the routes
#   route_hex.rds: output of 1_createSpace. matrix with routes and hexagon number
#   https://(...)speclist.cfm: website with species names and codes
#        
# Output: 
#   BirdHWA.rds: information about species couns in a route in an year
#   infestations_2.rds: information about which routes were infested at each year

# Load packages ---------------------
library(tidyverse)
library(fs)
library(rvest)
library(sp)
# MUST have raster library installed

# files to be sourced paths ---------------------
STATE_DATA_PATH <- "data/src/StateData"
INFESTATIONS_PATH <- "data/src/infestations.rds"
INFESTATIONS_PATH2 <- "data/fips_infes2.csv"
WEATHER_PATH <- "data/src/weather.csv"
LATLONG_PATH <- "data/src/route_coor.csv"
HEXAGON_PATH <- "data/route_hex.rds"
SPECIES_TABLE_URL <- "https://www.pwrc.usgs.gov/BBl/manual/speclist.cfm"

# Import: Species codes and names --------------------
speciesList <- read_html(SPECIES_TABLE_URL) %>%
  html_nodes("table") %>%
  html_table(fill = T) %>%
  flatten_df() %>%
  select(SpeciesId = `Species Number`,
         SpeciesCode = `Alpha Code`,
         SpeciesName = `Common Name`,
         SpeciesSciName = `Scientific Name`)
  
# Import: Observer info  --------------------
weather <- read_csv(WEATHER_PATH, col_types = cols_only(
  StateNum = col_number(),
  Route = col_number(),
  Year = col_number(),
  ObsN = col_number(),
  RPID = col_number()
)) %>%
  rename(ObserverId = ObsN,
         ObsType = RPID)

# Import: Infestation info  --------------------
# table with years when adelgid arrived in different counties

addYearInfested <- . %>%
  group_by(StateNum, Route) %>%
  arrange(Year, .by_group=T) %>%
  mutate(YearInfested = min(if_else(Infested, Year, Inf))) %>%
  ungroup()

infestations <- read_rds(INFESTATIONS_PATH) %>%
  extract(RouteId, c("StateNum", "Route"), "(..)(...)", convert=T, remove= FALSE) %>%
  select(-Infested) %>%
  pivot_longer(`2018`:`1951`, names_to = "Year", values_to = "Infested",
               names_transform = list(Year = as.numeric),
               values_ptypes = list(Infested = logical())) %>%
  addYearInfested() %>%
  select(RouteId,StateNum, Route, Year, Infested, YearInfested)

years_with_data <- sort(unique(infestations$Year))

# Import: bird data for states --------------------
stateData <- STATE_DATA_PATH %>%
  dir_ls() %>%
  map(read_csv, col_types = cols_only(
    Year = col_number(),
    StateNum = col_number(),
    Route = col_number(),
    RPID = col_number(),
    AOU = col_number(),
    SpeciesTotal = col_number()
  )) %>%
  bind_rows() %>%
  rename(ObsType = RPID, SpeciesId = AOU) %>% 
  mutate(RouteId = paste(sprintf("%02d",StateNum),sprintf("%03d",Route), sep=""))%>%
  relocate(RouteId) %>% 
  filter(Year %in% years_with_data)

# Import: lat long coordinates for routes --------------------
latlong <- read_csv(LATLONG_PATH, col_types = cols_only(
  StateNum = col_number(),
  Route = col_number(),
  Latitude = col_number(),
  Longitude = col_number())) %>%
  mutate(RouteId = paste(sprintf("%02d",StateNum),sprintf("%03d",Route), sep=""))%>%
  relocate(RouteId)

# Select only routes in hemlock range
stateData <- stateData %>% 
  filter(RouteId %in% infestations$RouteId)

# Combine data sets: infestation, species, observer and lat long  --------------------
# single tibble with all the information
BirdHWA <- stateData %>%
  left_join(infestations, by = c("StateNum", "Route", "Year", "RouteId")) %>%
  left_join(speciesList, by = c("SpeciesId")) %>%
  left_join(weather, by = c("StateNum", "Route", "Year","ObsType")) %>% 
  left_join(latlong, by = c("StateNum", "Route", "RouteId"))

if(nrow(BirdHWA) != nrow(stateData)){stop("Something wrong with the joins!")}

# Make no infestation have YearInfested equals to zero
BirdHWA <- BirdHWA %>% 
  mutate(YearInfested = ifelse(YearInfested == Inf, 0, YearInfested))

# Combine data sets: add temperature data -------------------
# temperature data is in °C * 10
climate <- raster::getData('worldclim', var = 'bio', res = 2.5)
# saveRDS(climate, file = "data/climate.rds")
clim <- climate[[c(6,11)]]
names(clim) <- c("minTemp","meanTemp")   ## Minimum and Mean Temperature of Coldest Quarter

xy <- BirdHWA %>% 
  select(`RouteId`,
         `Longitude`,
         `Latitude`) %>% 
  distinct() %>% 
  as.data.frame()

tempDF <- cbind(raster::extract(clim, SpatialPoints(xy[,2:3]), df = T), xy)

tempDF <- tempDF %>% 
  select(`RouteId`,
         `minTemp`,
         `meanTemp`) %>% 
  distinct() %>% 
  mutate(RouteId = ifelse(nchar(tempDF$RouteId) != 5,
                          str_c(rep(0,(5-nchar(tempDF$RouteId))), tempDF$RouteId, collapse= ""),
                          RouteId))

# Combine data sets: add hexagon number ---------------------
route_hex <- read_rds(HEXAGON_PATH)

BirdHWA <- BirdHWA %>%
  left_join(tempDF, by = c("RouteId")) %>% 
  left_join(route_hex, by= "RouteId") 

temps_rou <- BirdHWA %>% 
  select(RouteId, minTemp, meanTemp) %>% 
  distinct()

BirdHWA <- BirdHWA %>% 
  mutate(sd_tempMi = sd(temps_rou$minTemp),
         sd_tempMe = sd(temps_rou$meanTemp))
#BirdHWA <- BirdHWA %>% 
#  mutate(sd_tempMi = sapply(unique(BirdHWA[which(BirdHWA$Infested == T), 16]), sd),
#         sd_tempMe = sapply(unique(BirdHWA[which(BirdHWA$Infested == T), 17]), sd))

# Export tibbles  -------------------
write_rds(BirdHWA, file = 'data/BirdHWA.rds') 
write_rds(infestations, file = 'data/infestations_2.rds') 

