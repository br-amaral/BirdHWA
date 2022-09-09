#
# script to build table 4 - coefficient estimates for the model model and offset

library(tidyverse)
library(fs)


SPECIES_DATA_PATH <- path("data/src/sps_list.csv")
WAIC_BEST_PATH <- path("data/waicbest.rds")

sps_list <- read_csv(SPECIES_DATA_PATH)
waic_best <- read_rds(WAIC_BEST_PATH)

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

for(j in 1:nrow(sps_list)){
  species1 <- sps_list[j,]
  waic_sps <- waic_best %>% 
    filter(species == as.character(species1)) %>% 
    dplyr::select(model, year) %>% 
    filter(row_number() == n())
  
  summary_results2 <- read_rds(glue("data/models_resnew/{species1}/summary_results2.rds")) %>% 
    filter(model == waic_sps$model,
           year == waic_sps$year)
  pars_models <- as_tibble(rbind(pars_models_FUNC(1),
                                 pars_models_FUNC(2),
                                 pars_models_FUNC(3)))
  
  if(j == 1) {summaryALL <- pars_models} else {summaryALL <- rbind(summaryALL, pars_models)}
  print(sps_list[j,])
}

meanALL <- summaryALL %>% 
  filter(esti_type = "mean")

upALL <- summaryALL %>% 
  filter(esti_type = "up")

lowALL <- summaryALL %>% 
  filter(esti_type = "low")









