# 6_model

# Input: 
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

set.seed(10)

SPECIES_DATA_PATH <- "data/src/sps_list.csv"
source("5_formulasModels.R")
sps_list <- read_csv(SPECIES_DATA_PATH)
hex.adj <- paste0(getwd(),"/data/hexmap.graph")

offsets <- seq(2,16,1)

run_model <- function(off, BIRDx, formula){

## Create an year offset for that species ------------------  
   BIRDx <- BIRDx %>% 
    # year_offset is standardizing yrhwa to the offset (years after infestation to the impact)
    mutate(year_offset = ifelse(YearInfested != 0, Year - YearInfested + off, 0),
           # infoff: 'infested' route according to the delay in the effect (offset)
           infoff = ifelse(year_offset < off, 0, ifelse(year_offset >= off, 1, NA)))

  rout_notinf <- BIRDx %>% 
    select(RouteId, Year, YearInfested, Infested) %>% 
    filter(YearInfested == 0) %>% 
    distinct() %>% 
    group_by(RouteId) %>% 
    mutate(maxYear = max(Year)) %>% 
    select(RouteId, maxYear) %>% 
    distinct()
  
  ## if a route was never infested, year_offset is 'equal' to the last year it was sampled
  for(i in 1:nrow(BIRDx)){
    if(BIRDx$YearInfested[i] == 0){
      off_noin <- rout_notinf[which(rout_notinf$RouteId == BIRDx$RouteId[i]), 2]
      BIRDx$year_offset[i] <- BIRDx$Year[i] - as.numeric(off_noin) + off - 1
    }
  }
  ## all filters ----------------------------
  BIRDx1 <- BIRDx %>% 
    filter(YearInfested != 0,                               # only routes that were infested at some point
           year_offset > -20 & year_offset < 20) %>%        # look only +-20 years before/after infestation
    group_by(RouteId) %>% 
    group_split()
  
  for(i in 1:length(BIRDx1)){                               # look only at routes that were infested for at least 10 years
    a <- BIRDx1[[i]]
    maxi <- max(a$year_offset)
    if(maxi < 10) {BIRDx1[[i]] <- NULL }
  }
  
  BIRDx2_1 <- data.table::rbindlist(BIRDx1) %>% 
    as_tibble()
  
  model <- inla(formula, family= "poisson", data= BIRDx2_1, 
                control.predictor= list(compute=TRUE), 
                control.compute= list(waic=TRUE, dic=TRUE, cpo=TRUE))
  return(model)
}

run_combinations <- function(species){
  for(i in 1:length(formulas)){
    formula <- formulas[[i]]
    for(j in 1:length(offsets)){
      off <- offsets[j]
      SPECIES_MOD_DAT <- glue("data/species/{species}.rds")
      BIRDtab <- readRDS(SPECIES_MOD_DAT)
      resu <- run_model(off, BIRDtab, formula)
      name <- glue("{species}_model{i}_{off}yrs")
      assign(name,resu)
      print(name)
      name2 <- glue("data/models_res/{species}/{name}.rds", sep= "")
      dir.create(glue("data/models_res/{species}"))
      saveRDS(object = get(name), file = name2)
      rm(resu)
      rm(BIRDtab)
    }
  }
}

#run_combinations("BHVI")


lapply(sps_list$SpeciesCode, run_combinations)





