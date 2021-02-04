library(tidyverse)
library(fs)
library(rvest)

STATE_DATA_PATH <- "data/src/StateData"
INFESTATIONS_PATH <- "data/src/infestations.rds"
WEATHER_PATH <- "data/src/weather.csv"
SPECIES_TABLE_URL <- "https://www.pwrc.usgs.gov/BBl/manual/speclist.cfm"

## Import: Species codes and names --------------------
speciesList <- read_html(SPECIES_TABLE_URL) %>%
  html_nodes("table") %>%
  html_table(fill = T) %>%
  flatten_df() %>%
  select(SpeciesId = `Species Number`,
         SpeciesCode = `Alpha Code`,
         SpeciesName = `Common Name`,
         SpeciesSciName = `Scientific Name`)
  
## Import: Observer info  --------------------
weather <- read_csv(WEATHER_PATH, col_types = cols_only(
  StateNum = col_number(),
  Route = col_number(),
  Year = col_number(),
  ObsN = col_number()
)) %>%
  rename(ObserverId = ObsN)

## Import: Infestation info  --------------------
# table with years when adelgid arrived in different counties

addYearInfested <- . %>%
  group_by(StateNum, Route) %>%
  arrange(Year, .by_group=T) %>%
  mutate(YearInfested = min(if_else(Infested, Year, Inf))) %>%
  ungroup()

infestations <- read_rds(INFESTATIONS_PATH) %>%
  extract(RouteId, c("StateNum", "Route"), "(..)(...)", convert=T) %>%
  select(-Infested) %>%
  pivot_longer(`2018`:`1951`, names_to = "Year", values_to = "Infested",
               names_transform = list(Year = as.numeric),
               values_ptypes = list(Infested = logical())) %>%
  addYearInfested() %>%
  select(RouteId,StateNum, Route, Year, Infested, YearInfested)

## Import: bird data for states --------------------
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
  mutate(RouteID = paste(sprintf("%02d",StateNum),sprintf("%03d",Route), sep=""))%>%
  relocate(RouteID)

## Combine data sets  --------------------
# single tibble with all the information
BirdHWA <- stateData %>%
  left_join(infestations, by = c("StateNum", "Route", "Year")) %>%
  left_join(speciesList, by = c("SpeciesId")) %>%
  left_join(weather, by = c("StateNum", "Route", "Year")) 

save(BirdHWA, file = 'data/BirdHWA.rda') 