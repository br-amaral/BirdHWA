# 9_ExtractPars ---------------------------------------------------------------------------
# select the best model and offsets for each species according to WAIC and extract coefficient estimates.
# 
# Input:  data/src/sps_list.csv: list of species analysed
#         data/models_resnew/{spsr}/summary_results.rds: results of all models and offsets for a species
#
# Output: data/models_resnew/{species}/summary_results2.rds: coefficient of all models and offsets for a species
#         data/waicbest.rds: table with the best model number and offset year for each species
#
# Source: 7_extract_fixed_pars.R"
#

# Load packages ------------------------------
library(tidyverse)
library(glue)
library(fs)
library(INLA)
library(tidyselect)
library(progress)

# Load functions that extract parameters from list on a tibble column
source("~/Documents/GitHub/BirdHWA/7_extract_fixed_pars.R")

# Load species list 
SPECIES_DATA_PATH <- "data/src/sps_list.csv"
sps_list <- read_csv(SPECIES_DATA_PATH)
pb <- progress_bar$new(total = nrow(sps_list))

pars_models_FUNC <- function(i) {
  summary_results2 %>%
    rowwise() %>%
    mutate(intercept = as.numeric(intercept[[i]]),
           year_offset = year_offset[[i]],
           infoff = infoff[[i]],
           NewObserver = NewObserver[[i]],
           temp_min_scale = temp_min_scale[[i]],
           year_offset_infoff = year_offset_infoff[[i]],
           year_offset_temp_min_scale = year_offset_temp_min_scale[[i]],
           infoff_temp_min_scale = infoff_temp_min_scale[[i]],
           year_offset_infoff_temp_min_scale = year_offset_infoff_temp_min_scale[[i]],
           esti_type = ifelse(i == 1, "mean", ifelse(i == 2, "low", "up"))) %>% 
    select(-c(result_path, result, waic_list, waicNull, fixed))
}

for(k in 1:nrow(sps_list)){
  if (exists("pb")) {
    pb$tick()
  }
  spsr <- species <- pull(sps_list[k,])
  summary_results <- readRDS(glue('data/models_resnew/{spsr}/summary_results.rds'))
  
  summary_results2 <- summary_results %>%
    mutate(waic_list = map(result, "waic")) %>%
    #filter(!map_lgl(waic_list, is.null)) %>%
    mutate(waicNull = !map_lgl(waic_list, is.null),
           waic = NA)
  
  for(i in 1:nrow(summary_results2)){
    if(as.logical(summary_results2$waicNull[i]) == TRUE) {
      summary_results2$waic[i] <- unlist(pluck(summary_results2$waic_list[i], 1))} else {
        summary_results2$waic[i] <- NA
      }
  }
  
  summary_results2 <- summary_results2 %>%
    mutate(fixed = map(result, "fixed"),
           intercept = map(fixed, f_intercept),
           year_offset = map(fixed, f_year_offset),
           infoff = map(fixed, f_infoff),
           NewObserver = map(fixed, f_NewObserver),
           temp_min_scale = map(fixed, f_temp_min_scale),
           year_offset_infoff = map(fixed, f_year_offset_infoff),
           year_offset_temp_min_scale = map(fixed, f_year_offset_temp_min_scale),
           infoff_temp_min_scale = map(fixed, f_infoff_temp_min_scale),
           year_offset_infoff_temp_min_scale = map(fixed, f_year_offset_infoff_temp_min_scale)) 
  
  pars_models <- as_tibble(rbind(pars_models_FUNC(1),
                                 pars_models_FUNC(2),
                                 pars_models_FUNC(3)))
  
  waic_best <- summary_results2 %>% 
    arrange(waic) %>% 
    select(model, formula, year, species, waic)
  waic_best2 <- summary_results2[which(summary_results2$waic == min(summary_results2$waic)),c(1:4,9)]
  for(i in 2:nrow(waic_best)) {
    if((waic_best$waic[i] - waic_best2$waic[1]) < 2) {
      waic_best2 <- rbind(waic_best2, waic_best[i,])
    } else { break }
      }  
  
  if(nrow(waic_best2) > 1) {
    waic_best2 <- waic_best2 %>% 
      arrange(-model)
    waic_best2 <- waic_best2[1:2,]
    if(waic_best2$model[1] == 3 & waic_best2$model[2] == 2 | waic_best2$model[1] == 2 & waic_best2$model[2] == 3) {
      waic_best2 <- waic_best2 %>% filter(model == 2) } else {print("yep")}
    
    if(waic_best2$model[1] == 5 & waic_best2$model[2] == 6 | waic_best2$model[1] == 6 & waic_best2$model[2] == 5) {
      waic_best2 <- waic_best2 %>% filter(model == 5)} else {print("yep")}
    
    if(waic_best2$model[1] == 8 & waic_best2$model[2] == 9 | waic_best2$model[1] == 9 & waic_best2$model[2] == 8) {
      waic_best2 <- waic_best2 %>% filter(model == 8) } else {print("yep")}
  
    if(waic_best2$model[1] == 3 & waic_best2$model[2] == 2 | waic_best2$model[1] == 2 & waic_best2$model[2] == 3 | 
       waic_best2$model[1] == 5 & waic_best2$model[2] == 6 | waic_best2$model[1] == 6 & waic_best2$model[2] == 5 | 
       waic_best2$model[1] == 8 & waic_best2$model[2] == 9 | waic_best2$model[1] == 9 & waic_best2$model[2] == 8) {
      print("yep")
    } else {waic_best2 <- waic_best2[1,]}
    
  } else {print("yep")}
  
  if(k == 1) {waic_best3 <- waic_best2} else {waic_best3 <- rbind(waic_best3, waic_best2)}
  
  write_rds(summary_results2, file = glue("data/models_resnew/{species}/summary_results2.rds"))
  write_rds(waic_best3, file = "data/waicbest.rds")
  rm(summary_results2)
  rm(summary_results)
  rm(species)
  rm(spsr)
}
