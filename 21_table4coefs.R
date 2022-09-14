#
# script to build table 4 - coefficient estimates for the model model and offset

library(tidyverse)
library(fs)

colnmaes <- colnames

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
  filter(esti_type == "mean")

upALL <- summaryALL %>% 
  filter(esti_type == "up")
colnames(upALL)[6:15] <- glue("up_{colnames(upALL)[6:15]}")

lowALL <- summaryALL %>% 
  filter(esti_type == "low")
colnames(lowALL)[6:15] <- glue("low_{colnames(lowALL)[6:15]}")

order <- c("ACFL", "BHVI", "BLBW", "BTNW", "HETH", "MAWA", "RBNU",
               "BLJA", "CERW", "EAPH", "REVI", "SCTA", "WBNU", "WOTH")

meanALLpap <- left_join(meanALL, upALL, by = c("model","formula","year","species","waic")) %>% 
  left_join(., lowALL, by = c("model","formula","year","species","waic")) %>%
  mutate(over_intercept = data.table::between(0,low_intercept, up_intercept),
         over_year_offset = data.table::between(0,low_year_offset, up_year_offset),
         over_infoff = data.table::between(0,low_infoff, up_infoff),
         over_year_offset_infoff = data.table::between(0,low_year_offset_infoff, up_year_offset_infoff),
         over_temp_min_scale = data.table::between(0,low_temp_min_scale, up_temp_min_scale),
         over_year_offset_temp_min_scale = data.table::between(0,low_year_offset_temp_min_scale, up_year_offset_temp_min_scale),
         over_infoff_temp_min_scale = data.table::between(0,low_infoff_temp_min_scale, up_infoff_temp_min_scale),
         over_year_offset_infoff_temp_min_scale = data.table::between(0,low_year_offset_infoff_temp_min_scale, up_year_offset_infoff_temp_min_scale),
         over_NewObserver = data.table::between(0,low_NewObserver, up_NewObserver)
           ) %>% 
  relocate(species, model, year, 
           intercept, low_intercept, up_intercept, over_intercept, # b0
           year_offset, low_year_offset, up_year_offset, over_year_offset,  # b1
           infoff, low_infoff, up_infoff, over_infoff,  # b2
           year_offset_infoff, low_year_offset_infoff, up_year_offset_infoff, over_year_offset_infoff,  # b3
           temp_min_scale, low_temp_min_scale, up_temp_min_scale, over_temp_min_scale, # b4
           year_offset_temp_min_scale, low_year_offset_temp_min_scale, up_year_offset_temp_min_scale, over_year_offset_temp_min_scale, # b5
           infoff_temp_min_scale, low_infoff_temp_min_scale, up_infoff_temp_min_scale, over_infoff_temp_min_scale, # b6
           year_offset_infoff_temp_min_scale, low_year_offset_infoff_temp_min_scale, up_year_offset_infoff_temp_min_scale, over_year_offset_infoff_temp_min_scale, # b7
           NewObserver, low_NewObserver, up_NewObserver, over_NewObserver # a
           ) %>%
  arrange(factor(species, levels = order)) %>% 
  dplyr::select(-c(formula,waic,esti_type,up_esti_type,low_esti_type))

write_csv(meanALLpap, file = "Figures/Tables/Table4/table4.csv")







