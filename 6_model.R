library(INLA)
library(tidyverse)

set.seed(10)

SPECIES_DATA_PATH <- "data/src/sps_list.csv"
source("formulas_models.R")
sps_list <- read_csv(SPECIES_DATA_PATH)
hex.adj <- paste(getwd(),"/data/hexmap.graph", sep="")

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
  summary(model)
  
  meaninf <- model$summary.fixed[3,1]
  lowinf <- model$summary.fixed[3,3]
  highinf <- model$summary.fixed[3,5]
  
  meaninfL <- model$summary.fixed[5,1]
  lowinfL <- model$summary.fixed[5,3]
  highinfL <- model$summary.fixed[5,5]
  
  waic <- model$waic$waic
  
  return(model)
}

run_combinations <- function(species){
  for(i in 1:length(formulas)){
    formula <- formulas[[i]]
    for(j in 1:length(offsets)){
      off <- offsets[j]
      SPECIES_MOD_DAT <- paste("data/species/", species, ".rds", sep= "")
      BIRDtab <- readRDS(SPECIES_MOD_DAT)
      resu <- run_model(off,BIRDtab,formula)
      name <- paste(species, "_model", i, "_", off, "yrs", sep= "")
      assign(name,resu)
      print(name)
      name2 <- paste("data/models_res/", species, "/", name, ".RDS", sep= "")
      dir.create(paste("data/models_res/", species, sep=""))
      saveRDS(object = get(name), file = name2)
      rm(resu)
      rm(BIRDtab)
    }
  }
}

lapply(sps_list$SpeciesCode[1], run_combinations)





