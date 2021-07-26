library(tidyverse)
library(glue)
library(fs)
library(INLA)
library(tidyselect)

### choose species here! species list is: 
###  BHVI BLBW BTNW HETH MAWA RBNU ACFL  
spsr <- "RBNU"

source("7_extract_fixed_pars.R")

summary_results <- readRDS("data/models_res/summary_results.rds") %>% 
  filter(species == spsr)
# summary_results <- readRDS(glue('{spsr}_summares_2.RDS'))
# summary_results <- summary_results[1:15,]

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

pars_models <- as_tibble(rbind(pars_models_FUNC(1),
                               pars_models_FUNC(2),
                               pars_models_FUNC(3)))

(waic_best <- summary_results2[which(summary_results2$waic == min(summary_results2$waic)),1:4])

if(nrow(waic_best) != 1) {stop("\n\n\n\n two models tied as the best according to WAIC - pick one \n\n\n\n")}

(year_ <- waic_best$year)
(mod_ <- waic_best$model)
nacol <- c("mean", "low", "up","par")
par_tib <- pars_models %>% 
  filter(model == mod_,
         year == year_,
         species == spsr) %>% 
  mutate(WAIC = waic) %>% 
  select(-c(model, formula, year, species, waic)) %>% 
  relocate(esti_type) %>% 
  t() %>% 
  cbind(rownames(.)) %>% 
  as_tibble() %>% 
  rename_with(~nacol) %>% 
  mutate(mean = as.double(mean),
         low = as.double(low),
         up = as.double(up)) %>% 
  relocate(par)
(par_tib <- par_tib[-1,])

# waic plot   ---------------------
ggplot(aes(year, 
           jitter(waic, amount = 0.5),
           waic,
           group=model, color=factor(model)), data = summary_results2) +
  geom_line(alpha=0.5) +
  geom_point(alpha=0.5)

# ploting parameters  -----------------
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

plot_var(summary_results2, "infoff")
plot_var(summary_results2, "year_offset_infoff")
plot_var(summary_results2, "year_offset_infoff_temp_min_scale")

SUM_NAME <- glue("data/models_res/{spsr}/{spsr}_bestmodres.rds")
BEST_MOD <- glue("data/models_res/{spsr}/{spsr}_bestmod.rds")
BEST_OFF <- glue("data/models_res/{spsr}/{spsr}_bestoff.rds")
PARS_RES <- glue("data/models_res/{spsr}/{spsr}_partib.rds")

dir.create(glue("data/models_res/{spsr}"))

write_rds(summary_results2, file = SUM_NAME)
write_rds(mod_, file = BEST_MOD)
write_rds(year_, file = BEST_OFF)
write_rds(par_tib, file = PARS_RES)

