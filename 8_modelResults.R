library(tidyverse)
library(glue)
library(fs)
library(progress)
library(INLA)
library(tidyselect)

source("5_formulasModels.R")

DATA_PATH <- path("data")
RESULT_PATH <- path(DATA_PATH, "models_resnew")
RESULT_GLUE <- "{species}/{species}_model{model}_{year}yrs.rds"
RESULT_GLUE2 <- "/{species}/{species}_model{model}_{year}yrs.rds"
SUMMARY_RESULT_PATH <- path(RESULT_PATH, "/{sps}/summary_results.rds")
SPECIES_DATA_PATH <- "data/src/sps_list.csv"
EACH_SPS_DATA_PATH <- path(DATA_PATH,"species/{species}.rds")

sps_list <- read_csv(SPECIES_DATA_PATH)
offsets <- seq(2,16,1)

all_combinations <- tibble(
  model = seq_along(formulas),
  formula = formulas,
  year = list(offsets),
  species = list(sps_list$SpeciesCode)  # change species here
) %>%
  unnest(year) %>%
  unnest(species) %>%
  mutate(
    result_path = path(RESULT_PATH, glue_data(., RESULT_GLUE))
  )

up_res_model <- function(model, formula, year, species, result_path) {
  if (exists("pb")) {
    pb$tick()
  }
  result <- read_rds(result_path)
  summary(result)
}

pb <- progress_bar$new(total = nrow(all_combinations))

summary_results <- all_combinations %>%
  mutate(result = pmap(., up_res_model))

for(i in 1:nrow(sps_list)){
  sps <- pull(sps_list[i,])
  summary_results2 <- summary_results %>% filter(species == sps)
  write_rds(summary_results2, glue(SUMMARY_RESULT_PATH))
  rm(summary_results2)
}

#write_rds(summary_results, "summary_results.rds")
