# 7.5_Sensitivity and permutation test of the best model
# sensitivity is to remove each route at a time and re-fit the model
# permutation is to randomize the infestation year x times and refit the model

# Input: same as the 6_model:
#        /data/hexmap.graph
#        data/src/sps_list.csv  
#        data/species/{species}.rds
#        5_formulasModels.R (sourcing)
# Output: 
#        data/models_res/{species} (folder)
#        data/models_res/{species}/{name}.rds (files)

library(INLA)
library(tidyverse)
library(glue)

species <- "BHVI"
offsets <- 2
mod <- 1

set.seed(10)
SPECIES_DATA_PATH <- "data/src/sps_list.csv"
source("5_formulasModels.R")
sps_list <- read_csv(SPECIES_DATA_PATH)
hex.adj <- paste0(getwd(),"/data/hexmap.graph")
formula <- get(glue("formula{mod}"))

create_data_sensi <- function(offset2, BIRDx) {
  ## Create an year offset for that species ------------------  
  BIRDx2 <- BIRDx %>%  
    # remove 20 ears before and after infestation
    mutate(year_offset = ifelse(YearInfested != 0, Year - YearInfested, 0)) %>% 
    filter(year_offset > -20 & year_offset < 20) %>% 
    # Only routes infested for at least 10 years
    group_by(RouteId) %>% 
    mutate(max = max(year_offset)) %>%  
    filter(max > 9) %>% 
    ungroup() %>% 
    # year_offset is standardizing yrhwa to the offset (years after infestation to the impact) ADDING THE LAG
    mutate(year_offset = ifelse(YearInfested != 0, Year - YearInfested + offset2, 0),
           # infoff: 'infested' route according to the delay in the effect (offset)
           infoff = ifelse(year_offset <= 0, 0, ifelse(year_offset > 0, 1, NA)))
  
  return(BIRDx2)
}

create_data_perm <- function(offset2, BIRDin, perms) {
  ## Create an year offset for that species ------------------  
  BIRDx2 <- BIRDx %>%  
    # remove 20 ears before and after infestation
    mutate(year_offset = ifelse(YearInfested != 0, Year - YearInfested, 0)) %>% 
    filter(year_offset > -20 & year_offset < 20) %>% 
    # Only routes infested for at least 10 years
    group_by(RouteId) %>% 
    mutate(max = max(year_offset)) %>%  
    filter(max > 9) %>% 
    ungroup() %>% 
    # year_offset is standardizing yrhwa to the offset (years after infestation to the impact) ADDING THE LAG
    mutate(year_offset = ifelse(YearInfested != 0, Year - YearInfested + offset2, 0),
           # infoff: 'infested' route according to the delay in the effect (offset)
           infoff = ifelse(year_offset <= 0, 0, ifelse(year_offset > 0, 1, NA)))
  
  return(BIRDx2)
  
  }


run_model <- function(BIRDx_sub, formula) {
  model <- inla(formula, family="poisson", data=BIRDx_sub, 
                control.predictor=list(compute=TRUE), 
                control.compute=list(waic=TRUE, dic=TRUE, cpo=TRUE))
  return(model)
}


run_sensi <- function(species, offsets) {
  SPECIES_MOD_DAT <- glue("data/species/{species}.rds")
  BIRDtab <- readRDS(SPECIES_MOD_DAT)
  BIRDtab2 <- create_data_sensi(offsets, BIRDtab)
  off <- offsets
  
  routes <- BIRDtab2 %>% select(RouteId) %>% distinct() %>% arrange()
  
  dir.create(glue("data/models_res/{species}/sensi"))
  intercept <- matrix(NA, nrow = nrow(routes), ncol = 4) %>%
    as_tibble()
  
  colnames(intercept) <- c("route", "mean", "low", "up")
  intercept$route <- routes
  
  intercept <- intercept %>% 
    mutate(mean = as.numeric(mean),
           low = as.numeric(low),
           up = as.numeric(up))
  
  year_offset <- infoff <- NewObserver <- temp_min_scale <- year_offset.infoff <-
    year_offset.temp_min_scale <- infoff.temp_min_scale <-  year_offset.infoff.temp_min_scale <- intercept
  
  for(i in 1:nrow(routes)){
    
    BIRDtab3 <- BIRDtab2[which(BIRDtab2$RouteId != as.character(routes[i,1])),]
    
    resu <- run_model(BIRDtab3, formula)
    name <- glue("{species}_model_{off}yrs_{routes[i,1]}")
    assign(name, resu)
    print(name)
    name2 <- glue("data/models_res/{species}/sensi/{name}.rds", sep= "")
   
    coefs <- resu$summary.fixed[,c(1,3,5)]
    
    intercept[i,2:4] <- coefs["(Intercept)",]
    year_offset[i,2:4] <- coefs["year_offset",]
    infoff[i,2:4] <- coefs["infoff",]
    NewObserver[i,2:4] <- coefs["NewObserverTRUE",]
    temp_min_scale[i,2:4] <- coefs["temp_min_scale",]
    year_offset.infoff[i,2:4] <- coefs["year_offset:infoff",]
    year_offset.temp_min_scale[i,2:4] <- coefs["year_offset:temp_min_scale",]
    infoff.temp_min_scale[i,2:4] <- coefs["infoff:temp_min_scale",]
    year_offset.infoff.temp_min_scale[i,2:4] <- coefs["year_offset:infoff:temp_min_scale",]
    
    saveRDS(object = get(name), file = name2)
    rm(resu)
    rm(BIRDtab3)
    rm(coefs)
  }
  
  saveRDS(intercept, file = glue("data/models_res/{species}/sensi/intercept_{species}.rds", sep= ""))
  saveRDS(year_offset, file = glue("data/models_res/{species}/sensi/year_offset_{species}.rds", sep= ""))
  saveRDS(infoff, file = glue("data/models_res/{species}/sensi/infoff_{species}.rds", sep= ""))
  saveRDS(NewObserver, file = glue("data/models_res/{species}/sensi/NewObserver_{species}.rds", sep= ""))
  saveRDS(temp_min_scale, file = glue("data/models_res/{species}/sensi/temp_min_scale_{species}.rds", sep= ""))
  saveRDS(year_offset.infoff, file = glue("data/models_res/{species}/sensi/year_offset.infoff_{species}.rds", sep= ""))
  saveRDS(year_offset.temp_min_scale, file = glue("data/models_res/{species}/sensi/year_offset.temp_min_scale_{species}.rds", sep= ""))
  saveRDS(infoff.temp_min_scale, file = glue("data/models_res/{species}/sensi/infoff.temp_min_scale_{species}.rds", sep= ""))
  saveRDS(year_offset.infoff.temp_min_scale, file = glue("data/models_res/{species}/sensi/year_offset.infoff.temp_min_scale_{species}.rds", sep= ""))
  
}

run_perm <- function(species, perm, offsets) {
  off <- offsets
  perms <- perm
  SPECIES_MOD_DAT <- glue("data/species/{species}.rds")
  BIRDtab <- readRDS(SPECIES_MOD_DAT)
  
  for(i in 1:perms){
    
    BIRDtab2 <- create_data_perm(off, BIRDtab, perms[i])
    
    resu <- run_model(BIRDtab2, formula)
    name <- glue("{species}_model_{off}yrs_perm{i}")
    assign(name, resu)
    print(name)
    name2 <- glue("data/models_res/{species}/perm/{name}.rds", sep= "")
    #dir.create(glue("data/models_res/{species}"))
    if (i == 1) {dir.create(glue("data/models_res/{species}/perm"))}
    saveRDS(object = get(name), file = name2)
    rm(resu)
    rm(BIRDtab2)
    rm(name)
  }
}


run_sensi(species = species, offsets = 1)
run_perm(species = species, perm = 10, offsets = 1)
