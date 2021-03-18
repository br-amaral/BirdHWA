library(tidyverse)
library(glue)
library(fs)
library(progress)
library(INLA)
library(tidyselect)

#teste teste testet

source("5_formulasModels.R")
source("extract_fixed_pars.R")

DATA_PATH <- path("data")
RESULT_PATH <- path(DATA_PATH, "models_res")
RESULT_GLUE <- "{species}/{species}_model{model}_{year}yrs.rds"
SUMMARY_RESULT_PATH <- path(RESULT_PATH, "summary_results.rds")
SPECIES_DATA_PATH <- "data/src/sps_list.csv"

sps_list <- read_csv(SPECIES_DATA_PATH)
offsets <- seq(2,16,1)

all_combinations <- tibble(
  model = seq_along(formulas),
  formula = formulas,
  year = list(offsets),
  species = list(sps_list$SpeciesCode[2])  # change species here
) %>%
  unnest(year) %>%
  unnest(species) %>%
  mutate(
    result_path = path(RESULT_PATH, glue_data(., RESULT_GLUE))
  )

run_save_model <- function(model, formula, year, species, result_path) {
  if (exists("pb")) {
    pb$tick()
  }
  result <- read_rds(result_path)
  summary(result)
}

pb <- progress_bar$new(total = nrow(all_combinations))

summary_results <- all_combinations %>%
  mutate(result = pmap(., run_save_model))

write_rds(summary_results, SUMMARY_RESULT_PATH)

summary_results2 <- summary_results %>%
  mutate(waic_list = map(result, "waic")) %>%
  #filter(!map_lgl(waic_list, is.null)) %>%
  mutate(waicNull = !map_lgl(waic_list, is.null),
         waic = NA)
  #mutate(waic_list = na_if(waic_list, NULL))
  #mutate(waic_list= ifelse(is.null(waic_list), NA, waic_list)) %>%
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

# waic plot
ggplot(aes(year, jitter(waic, amount = 0.5), group=model, color=factor(model)), data = summary_results2) +
  geom_line(alpha=0.5) +
  geom_point(alpha=0.5)

model_names <- paste(rep("Model",10), seq(1,10), sep= " ")

plot_var <- function(my_tibble, variable1) {
  my_tibble %>% 
  select(all_of(variable1)) %>% 
  unlist() %>% 
  matrix(., ncol = 3, byrow = T) %>% 
  as_tibble() %>% 
  rename(mean = V1, low = V2, up = V3) %>% 
  bind_cols(summary_results2[,c(4,1,3)], .) %>%
  mutate(model_name = as.character(glue("Model {model}"))) %>% 
  group_by(model) %>% 
  transform(model_name = factor(model_name, levels= model_names)) %>% 
  ggplot(aes(year, mean)) +
    geom_hline(aes(yintercept = 0, colour="red"),
               show.legend = FALSE) +
    geom_errorbar(aes(ymin= low, ymax= up),
                  colour = "grey65") +
    geom_point() +
    facet_wrap(~ model_name, ncol = 5) + 
    theme_bw() +
    labs(title = variable1) +
    theme(plot.title = element_text(hjust = 0.5))
}

colnames(summary_results2[11:19])

"intercept_change"                 
"year_offset"                      
"infoff"                           
"NewObserver"                      
"temp_min_scale"                   
"year_offset_infoff"               
"year_offset_temp_min_scale"       
"infoff_temp_min_scale"            
"year_offset_infoff_temp_min_scale"

plot_var(summary_results2, "infoff_temp_min_scale")















summary_results %>%
  mutate(waic_list = map(result, "waic")) %>%
  filter(!map_lgl(waic_list, is.null)) %>%
  mutate(waic = map_dbl(waic_list, pluck, 1)) %>%
  ggplot(aes(year, waic, group=model, color=factor(model))) +
  geom_line()



