# 6_model

# Input: 
#        
# Output: 
#         

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
           # year_offset is standardizing yrhwa to the offset (years after infestation to the impact)
    mutate(year_offset = ifelse(YearInfested != 0, Year - YearInfested - offset, 0),
           # infoff: 'infested' route according to the delay in the effect (offset)
           infoff = ifelse(year_offset <= 0, 0, ifelse(year_offset > 0, 1, NA)))
  
  model <- inla(formula, family="poisson", data=BIRDx, 
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
      resu <- run_model(off,BIRDtab,formula)
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

lapply(sps_list$SpeciesCode[1], run_combinations)





