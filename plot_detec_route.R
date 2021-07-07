# plot number of detections per route

library(tidyverse)
library(glue)
library(ggplot2)
library(hrbrthemes)

# single species data frames in data/species folder
SPECIES_DATA_PATH <- "data/src/sps_list.csv"
SINGLE_SPS <- "data/species"
BIRD_FILE_PATH <- "data/BirdHWA_2.rds"

BIRDallsps <- readRDS(BIRD_FILE_PATH) %>% 
  select(Longitude, Latitude) %>% 
  unique()
sps_list <- read_csv(SPECIES_DATA_PATH)

plot_heat <- function(BIRDdat, mindec, maxlim){
  ggplot(data = BIRDdat, aes(RouteId, Year, fill= SpeciesTotal)) + 
    geom_tile() +
    scale_fill_distiller(palette = "Spectral",
                         limits = c(mindec, maxlim)) +
    theme_ipsum() +
    ggtitle(BIRDdat$SpeciesCode[1]) +
    theme(axis.text.x = element_text(angle = 90, 
                                     vjust = 0.5,
                                     hjust=1))
}

species <- "ACFL"
mindec <- 0
maxdec <- 27

#plot_data_dist <- function(species, mindec, maxdec) {
  LOAD_SINGLE_SPS <- glue("{SINGLE_SPS}/{species}.rds")
  SBIRD <- read_rds(LOAD_SINGLE_SPS)
  SBIRD2 <- SBIRD %>% 
    select(SpeciesCode, SpeciesTotal, RouteId, Year, Infested) %>% 
    filter(!is.na(SpeciesCode)) %>% 
    filter(SpeciesTotal > mindec) %>% 
    arrange(RouteId, Year)
  
  uni_rou <- unique(SBIRD2$RouteId)
  len_uni_rou <- length(uni_rou)
  n_split <- floor(len_uni_rou/50)
  length(uni_rou) <- prod(dim(matrix(uni_rou, ncol = n_split)))  ## add NA to end of df
  rout_mat <- matrix(data = uni_rou, nrow = n_split, byrow = T)

  n_splits <- nrow(rout_mat)
  maxlim <- max(SBIRD2$SpeciesTotal)
  BIRDs <- vector(mode = "list", length = n_splits)
  for(i in 1:n_splits){
    BIRDs[[i]] <- SBIRD2 %>% filter(RouteId %in% rout_mat[i,])
  }
  #print(map(.x = BIRDs,mindec = mindec, maxlim = maxlim,.f = plot_heat))
    for(i in 1:length(BIRDs)){
      print(plot_heat(BIRDdat = BIRDs[[i]], 
                      mindec = mindec, maxlim = maxlim))
    }
    
  over_max <- SBIRD %>% 
    select(SpeciesCode, SpeciesTotal, RouteId, Year, Infested,
           Latitude, Longitude) %>% 
    filter(!is.na(SpeciesCode)) %>% 
    filter(SpeciesTotal > maxdec) %>% 
    arrange(RouteId, Year)
  
  View(over_max)
  
  print(
    ggplot(data = over_max, aes(RouteId, Year, fill= SpeciesTotal)) + 
      geom_tile() +
      scale_fill_distiller(palette = "RdPu") +
      theme_ipsum() +
      ggtitle(glue(
        "{over_max$SpeciesCode[1]} - {nrow(over_max)}/{nrow(SBIRD2)} occasions with over {maxdec} detecs")) +
      theme(axis.text.x = element_text(angle = 90, 
                                       vjust = 0.5,
                                       hjust=1))
  )
  
  par(mfrow=c(1,2))
  hist(SBIRD$SpeciesTotal)
  hist(log(SBIRD$SpeciesTotal))
  
  par(mfrow=c(1,1))
  plot(BIRDallsps, col="lightgray")
  points(over_max$Longitude, over_max$Latitude)
  
#}



#plot_data_dist("ACFL", 0, 30)
#plot_data_dist("BLBW", 0, 30)
#plot_data_dist("BTWN", 0, 30)
#plot_data_dist("HETH", 0, 30)
#plot_data_dist("MAWA", 0, 30)
#plot_data_dist("OVEN", 0, 30)
#plot_data_dist("RBNU", 0, 30)
#plot_data_dist("BHVI", 0, 30)

