library(tidyverse)
library(rvest)
library(glue)

# load results: for a species, for a model, for an offset
SPECIES_DATA_PATH <- "data/src/sps_list.csv"
SPECIES_TABLE_URL <- "https://www.pwrc.usgs.gov/BBl/manual/speclist.cfm"

sps_mod <- read_csv(SPECIES_DATA_PATH)
speciesList <- read_html(SPECIES_TABLE_URL) %>%
  html_nodes("table") %>%
  html_table(fill = T) %>%
  flatten_df() %>%
  select(SpeciesId = `Species Number`,
         SpeciesCode = `Alpha Code`,
         SpeciesName = `Common Name`,
         SpeciesSciName = `Scientific Name`)

file_path <- "~/Box/BirdsHemlocks/BirdHWA_GitHub_labPc/data (1)/models_res/"
species <- as.character(sps_mod[2,])

# function to load all model and offset combinations for a species
load_res <- function(species){
  tb <- tibble::tibble(path = glue(file_path, species,"/", species,"_model")) %>%
    slice(rep(1:n(), each = 150)) %>% 
    mutate(model = sort(rep(seq(1,10,1),15)),
           years = rep(seq(2,16,1),10),
           end = "yrs.RDS") %>% 
    transmute(name_file = paste0(path, model,"_", years, end)) %>% 
    pull()  # transform tibble to vector
    map(tb, readRDS)
}

BHVI_model <- load_res("BHVI")

summary(BHVI_model[[2]]$model)



