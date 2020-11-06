library(tidyverse)
library(fs)

STATE_DATA_PATH <- "data/src/StateData"
INFESTATIONS_PATH <- "data/src/infestations.rds"
SPECIES_LIST_PATH <- "data/src/SspList.csv"
WEATHER_PATH <- "data/src/weather.csv"

infestations <- read_rds(INFESTATIONS_PATH) %>%
  extract(RouteId, c("StateNum", "Route"), "(..)(...)") %>%
  select(-Infested) %>%
  pivot_longer(`2018`:`1951`, names_to = "Year", values_to = "Infested") %>%
  select(StateNum, Route, Year, Infested)

stateData <- STATE_DATA_PATH %>%
  dir_ls() %>%
  map(read_csv, col_types = "ccccccciiiiiii") %>%
  bind_rows() %>%
  left_join(infestations, by = c("StateNum", "Route", "Year"))


speciesList <- read_csv(SPECIES_LIST_PATH)

weather <- read_csv(WEATHER_PATH)

  
