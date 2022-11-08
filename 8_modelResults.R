# 8_modelResults ---------------------------------------------------------------------------
# code for extracting model results from all model outputs that were saved in individual files
#    and put them in the same tibble, to compare WAIC and select the best model and offset.
# INPUT
#   data/src/sps_list.csv: list of all species analysed
#   data/models_resnew/{species}/{species}_model{model}_{year}yrs.rds: model output for each species, model and offset
#
# OUTPUT:
#   data/models_resnew/{sps}/summary_results.rds: coefficient estimates and WAIC for all models and offsets
#   data/models_resnew/summary_results.rds: summary for all species together
#

# load packages ----------------------------
library(tidyverse)
library(glue)
library(fs)
library(progress)
library(INLA)
library(tidyselect)

# source the formulas of each model ---------------------------
source("5_formulasModels.R")

# objects used to run code -----------------------------
DATA_PATH <- path("data")
RESULT_PATH <- path(DATA_PATH, "models_resnew")
RESULT_GLUE <- "{species}/{species}_model{model}_{year}yrs.rds"
RESULT_SUMMARY_PATH <- path(RESULT_PATH, "/{sps}/summary_results.rds")
SPECIES_DATA_PATH <- "data/src/sps_list.csv"

sps_list <- read_csv(SPECIES_DATA_PATH)
offsets <- seq(2,16,1)

# get all combinations of species, models and offsets in a tibble
all_combinations <- tibble(
  model = seq_along(formulas),
  formula = formulas,
  year = list(offsets),
  species = list(sps_list$SpeciesCode)  # change species here for not all
) %>%
  unnest(year) %>%
  unnest(species) %>%
  mutate(
    result_path = path(RESULT_PATH, glue_data(., RESULT_GLUE))
  )

# load all combinat6ions from the previous tibble
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

# save individual results for each species in different files
for(i in 1:nrow(sps_list)){
  sps <- pull(sps_list[i,])
  summary_results2 <- summary_results %>% filter(species == sps)
  write_rds(summary_results2, glue(RESULT_SUMMARY_PATH))
  rm(summary_results2)
}

write_rds(summary_results, glue(RESULT_PATH, "/summary_results.rds"))
