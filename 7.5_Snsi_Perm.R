# 7.5_Sensitivity and permutation test of the best model

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

species <- "ACFL"
offsets <- 2
mod <- 1

set.seed(10)
SPECIES_DATA_PATH <- "data/src/sps_list.csv"
source("5_formulasModels.R")
sps_list <- read_csv(SPECIES_DATA_PATH)
hex.adj <- paste0(getwd(),"/data/hexmap.graph")
formula <- get(glue("formula{mod}"))

create_data <- function(offset, BIRDx){
  off <- offset
  ## Create an year offset for that species ------------------  
  BIRDx <- BIRDx %>% 
    # year_offset is standardizing yrhwa to the offset (years after infestation to the impact)
    mutate(year_offset = ifelse(YearInfested != 0, Year - YearInfested - offset, 0),
           # infoff: 'infested' route according to the delay in the effect (offset)
           infoff = ifelse(year_offset <= 0, 0, ifelse(year_offset > 0, 1, NA)))
  
  rout_notinf <- BIRDx %>% 
    select(RouteId, Year, YearInfested, Infested) %>% 
    filter(YearInfested == 0) %>% 
    distinct() %>% 
    group_by(RouteId) %>% 
    mutate(maxYear = max(Year)) %>% 
    select(RouteId, maxYear) %>% 
    distinct()
  
  for(i in nrow(BIRDx)){
    if(BIRDx$YearInfested[i] == 0){
      off_noin <- rout_notinf[which(rout_notinf$RouteId == BIRDx$RouteId[i]), 2]
      BIRDx$year_offset[i] <- off_noin
    }
  }
  return(BIRDx)
}

run_model <- function(offset, BIRDx_sub, formula){
  model <- inla(formula, family="poisson", data= BIRDx_sub, 
                control.predictor=list(compute=TRUE), 
                control.compute=list(waic=TRUE, dic=TRUE, cpo=TRUE))
  return(model)
}

run_combinations <- function(species){
  SPECIES_MOD_DAT <- glue("data/species/{species}.rds")
  BIRDtab <- readRDS(SPECIES_MOD_DAT)
  BIRDtab2 <- create_data(offset, BIRDtab)
  
  routes <- BIRDtab2 %>% select(RouteId) %>% distinct() %>% arrange()
  
  for(i in 1:nrow(routes)){

    BIRDtab3 <- BIRD[which(BIRD$RouteId != as.character(routes[i,1])),]
    
    resu <- run_model(off, BIRDtab3, formula)
    name <- glue("{species}_model_{off}yrs")
    assign(name, resu)
    print(name)
    name2 <- glue("data/models_res/{species}/sensi/{name}_{routes[i,1]}.rds", sep= "")
    dir.create(glue("data/models_res/{species}/sensi"))
    saveRDS(object = get(name), file = name2)
    rm(resu)
    rm(BIRDtab)
    
  }
  
  
  
  }
}





lapply(sps_list$SpeciesCode, run_combinations)
