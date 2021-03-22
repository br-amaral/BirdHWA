library(tidyverse)
library(glue)
library(fs)
library(progress)
library(INLA)
library(tidyselect)


source("5_formulasModels.R")
source("extract_fixed_pars.R")

DATA_PATH <- path("data")
RESULT_PATH <- path(DATA_PATH, "models_res")
RESULT_GLUE <- "{species}/{species}_model{model}_{year}yrs.rds"
RESULT_GLUE2 <- "/{species}/{species}_model{model}_{year}yrs.rds"
SUMMARY_RESULT_PATH <- path(RESULT_PATH, "summary_results.rds")
SPECIES_DATA_PATH <- "data/src/sps_list.csv"
EACH_SPS_DATA_PATH <- path(DATA_PATH,"species/{species}.rds")

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

#write_rds(summary_results, SUMMARY_RESULT_PATH)

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

# waic plot   ---------------------
ggplot(aes(year, jitter(waic, amount = 0.5), group=model, color=factor(model)), data = summary_results2) +
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

plot_var(summary_results2, "infoff_temp_min_scale")

# Make predictions and plotting ---------
# summary_results <- summary_results_MAWA2
# change all MAWA2 for data object path, and summary_results_MAWA2 for summary_results$results

plot_pred <- function (yr_type, i, bird2) {
  if(i > 15) {stop("we do not have that offset!")}
  if(yr_type == "yrhwa"){
    predmean <- aggregate(bird2[[i]]$`pred.0.5quant`, list(bird2[[i]]$year_offset), FUN=mean)  ## aggregate medians by year (mean of medians)
    predUCL<-aggregate(bird2[[i]]$`pred.0.975quant`, list(bird2[[i]]$year_offset), FUN=mean)
    predLCL<-aggregate(bird2[[i]]$`pred.0.025quant`, list(bird2[[i]]$year_offset), FUN=mean)
  }
  
  if(yr_type == "year"){
    predmean <- aggregate(bird2[[i]]$`pred.0.5quant`, list(bird2[[i]]$Year), FUN=mean)  
    predUCL<-aggregate(bird2[[i]]$`pred.0.975quant`, list(bird2[[i]]$Year), FUN=mean)
    predLCL<-aggregate(bird2[[i]]$`pred.0.025quant`, list(bird2[[i]]$Year), FUN=mean)
  }
  
  names(predmean)[names(predmean)=="x"]<-"mean"
  names(predmean)[names(predmean)=="Group.1"]<-"yrnb"
  names(predUCL)[names(predUCL)=="x"]<-"UCL"
  names(predLCL)[names(predLCL)=="x"]<-"LCL"
  birdpred <- cbind(predmean, predLCL$LCL, predUCL$UCL)
  colnames(birdpred) <- c("yrnb", "mean", "LCL", "UCL")
  #freq_key <- as.data.frame(cbind(sort(unique(bird2[[i]]$YearInfested)),table(bird2[[i]]$YearInfested)))
  #colnames(freq_key) <- c("YearInfested", "Freq_yrinf")
  #rownames(freq_key) <- NULL
  #birdpred <- left_join(birdpred, freq_key, by = "YearInfested")
  
  LCLmax <- ceiling(max(birdpred$UCL))
  plot(birdpred$yrnb,birdpred$mean, pch=16, ylim=c(0,LCLmax),
       xlim = c(1988, 2018),
       xlab="years since first HWA detection", ylab="birds per route", #main="Segmented Model",
       cex.lab=1.1, cex.axis=1.1, main = glue("{i+1} year offset")) 
  lines(x = birdpred$yrnb, y = birdpred$LCL, col="darkgrey", lwd=2)
  lines(x = birdpred$yrnb, y = birdpred$UCL, col="darkgrey", lwd=2)
  abline(v = mean(bird2[[i]]$YearInfested[bird2[[i]]$YearInfested != 0 ],
                  na.rm = TRUE) + i + 1, col = "blue")
  abline(v = min(bird2[[i]]$YearInfested[bird2[[i]]$YearInfested != 0 ],
                 na.rm = TRUE)  + i + 1, col = "red")
}



species <- "MAWA"
model <- 3
yr_type <- "year"   ## year or yrhwa 

inla_pred <- function (species, model, yr_type){
  summary_results2 <- summary_results %>% 
    filter(species == species,
           model == model) %>% 
    mutate(pred_path = glue(RESULT_PATH, RESULT_GLUE2),
           pred_obj = NA,
           sps_tib = glue(EACH_SPS_DATA_PATH))
  
  preds <- list()
  for(i in 1:nrow(summary_results2)){
    a <- read_rds(summary_results2$pred_path[i])
    b <- as_tibble(as.data.frame(a$summary.fitted.values))
    preds[[i]] <- b
    rm(a)
  }
  
  ind_sps <- list()
  for(i in 1:nrow(summary_results2)){
    c <- read_rds(summary_results2$sps_tib[i]) %>% 
      # year_offset is standardizing yrhwa to the offset (years after infestation to the impact)
      mutate(year_offset = ifelse(YearInfested != 0, Year - YearInfested - summary_results2$year[i], 0),
             # infoff: 'infested' route according to the delay in the effect (offset)
             infoff = ifelse(year_offset <= 0, 0, ifelse(year_offset > 0, 1, NA)))
    ind_sps[[i]] <- c
  }
  
  bird1 <- Map(cbind, ind_sps, pred = preds)
  bird2 <- Map(na.omit, bird1)
  
  par(mfrow = c(3,5))
  plot_pred("year", 1, bird2)
  plot_pred("year", 2, bird2)
  plot_pred("year", 3, bird2)
  plot_pred("year", 4, bird2)
  plot_pred("year", 5, bird2)
  plot_pred("year", 6, bird2)
  plot_pred("year", 7, bird2)
  plot_pred("year", 8, bird2)
  plot_pred("year", 9, bird2)
  plot_pred("year", 10, bird2)
  plot_pred("year", 11, bird2)
  plot_pred("year", 12, bird2)
  plot_pred("year", 13, bird2)
  plot_pred("year", 14, bird2)
  plot_pred("year", 15, bird2)
  #print(glue("model {model} predictions for {species}"))
}








summary_results %>%
  mutate(waic_list = map(result, "waic")) %>%
  filter(!map_lgl(waic_list, is.null)) %>%
  mutate(waic = map_dbl(waic_list, pluck, 1)) %>%
  ggplot(aes(year, waic, group=model, color=factor(model))) +
  geom_line()



