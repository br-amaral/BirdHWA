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

run_model <- function(offset, BIRDx, formula){
   ## Create an year offset for that species ------------------  
  BIRDx <- BIRDx %>%  
    # remove 20 ears before and after infestation
    mutate(year_offset = ifelse(YearInfested != 0, Year - YearInfested, 0)) %>% 
    filter(year_offset > -20 & year_offset < 20) %>% 
    # Only routes infested for at least 10 years
    group_by(RouteId) %>% 
    mutate(max = max(year_offset)) %>%  
    filter(max > 9) %>% 
    ungroup() %>% 
    # year_offset is standardizing yrhwa to the offset (years after infestation to the impact) ADDING THE LAG
    mutate(year_offset = ifelse(YearInfested != 0, Year - YearInfested + offset, 0),
           # infoff: 'infested' route according to the delay in the effect (offset)
           infoff = ifelse(year_offset <= 0, 0, ifelse(year_offset > 0, 1, NA)))
  print(nrow(BIRDx))
  print(sum(BIRDx$SpeciesTotal))

  model <- inla(formula, 
                #family="poisson",
                family = "zeroinflatedpoisson0",
                #family = "zeroinflatedpoisson1",
                data=BIRDx, 
                control.predictor=list(compute=TRUE), 
                control.compute=list(waic=TRUE, dic=TRUE, cpo=TRUE))
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

lapply(sps_list$SpeciesCode, run_combinations)





