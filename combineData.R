library(tidyverse)
library(fs)
library(rvest)

STATE_DATA_PATH <- "data/src/StateData"
INFESTATIONS_PATH <- "data/src/infestations.rds"
WEATHER_PATH <- "data/src/weather.csv"
SPECIES_TABLE_URL <- "https://www.pwrc.usgs.gov/BBl/manual/speclist.cfm"

speciesList <- read_html(SPECIES_TABLE_URL) %>%
  html_nodes("table") %>%
  html_table(fill = T) %>%
  flatten_df() %>%
  select(SpeciesId = `Species Number`,
         SpeciesCode = `Alpha Code`,
         SpeciesName = `Common Name`,
         SpeciesSciName = `Scientific Name`)
  

weather <- read_csv(WEATHER_PATH, col_types = cols_only(
  StateNum = col_integer(),
  Route = col_integer(),
  Year = col_integer(),
  ObsN = col_integer()
)) %>%
  rename(ObserverId = ObsN)

infestations <- read_rds(INFESTATIONS_PATH) %>%
  extract(RouteId, c("StateNum", "Route"), "(..)(...)", convert=T) %>%
  select(-Infested) %>%
  pivot_longer(`2018`:`1951`, names_to = "Year", values_to = "Infested",
               names_transform = list(Year = as.integer),
               values_ptypes = list(Infested = logical())) %>%
  select(StateNum, Route, Year, Infested)

stateData <- STATE_DATA_PATH %>%
  dir_ls() %>%
  map(read_csv, col_types = cols_only(
    Year = col_integer(),
    StateNum = col_integer(),
    Route = col_integer(),
    RPID = col_integer(),
    AOU = col_integer(),
    SpeciesTotal = col_integer()
  )) %>%
  bind_rows() %>%
  rename(ObsType = RPID, SpeciesId = AOU)

BirdHWA <- stateData %>%
  left_join(infestations, by = c("StateNum", "Route", "Year")) %>%
  left_join(speciesList, by = c("SpeciesId")) %>%
  left_join(weather, by = c("StateNum", "Route", "Year"))

save(BirdHWA, file = 'data/BirdHWA.rda') 