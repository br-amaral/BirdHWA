library(tidyverse)
library(rvest)
library(glue)

# setwd("~/Box/BirdsHemlocks/BirdHWA_GitHub_labPc")
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

file_path <- "~/Box/BirdsHemlocks/BirdHWA_GitHub_labPc/data/models_res/"

# tibble to store waic values
WAIC <- matrix(NA,10,15)
colnames(WAIC) <- c("yr2", "yr3", "yr4", "yr5", "yr6", 
                    "yr7", "yr8", "yr9", "yr10", "yr11", 
                    "yr12", "yr13", "yr14", "yr15", "yr16")
rownames(WAIC) <- c("mod1", "mod2", "mod3", "mod4", "mod5", 
                    "mod6", "mod7", "mod8", "mod9", "mod10")

# function to load all model and offset combinations for a species
load_res <- function(species){
  tb <- tibble::tibble(path = glue(file_path, species,"/", species,"_model")) %>%
    slice(rep(1:n(), each = 150)) %>% 
    mutate(model = sort(rep(seq(1,10,1),15)),
           years = rep(seq(2,16,1),10),
           end = "yrs.rds",
           #end = "yrs.RData",
           name_file = paste0(path, model,"_", years, end))
  
  for(i in 1:nrow(tb)){
    if(str_count(tb$name_file[i]) == 82) {compri <- c(63,82)}
    if(str_count(tb$name_file[i]) == 83) {compri <- c(63,83)}
    if(str_count(tb$name_file[i]) == 84) {compri <- c(63,84)}
    modname <- str_sub(tb$name_file[i], start = compri[1], end = compri[2])
    #restemp <- readRDS(tb$name_file[i])
    modname2 <- as.character(glue("{file_path}{species}/{modname}"))
    load(modname2, envir=globalenv())
    #assign(modname, restemp)
    row <- tb$model[i]
    col <- tb$years[i] - 1
    modname3 <- substr(modname, 1, nchar(modname) - 6)
    WAIC[row,col] <- as.numeric(summary(get(modname3))$waic[1])
    print(i)
  }
}

load_res("BHVI")

mods1 <- lapply(tb[1:2], readRDS)
mods1Waic <- mods1[1]

species <- as.character(sps_mod[1,])

 