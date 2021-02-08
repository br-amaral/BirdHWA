library(INLA)

set.seed(10)

source("formulas_models.R")
hex.adj <- paste(getwd(),"/data/hexmap.graph", sep="")

offsets <- seq(2,16,1)

run_model <- function(offset, BIRDx, formula){

## Create an year offset for that species ------------------  
#  year_offset is standardizing yrhwa to the offset (years after infestation to the impact)
  for(i in 1:nrow(BIRDx)){
    if(!is.finite(BIRDx$YearInfested[i])) {BIRDx$YearInfested[i] <- 0}
    if(!is.finite(BIRDx$Infested[i])) {BIRDx$Infested[i] <- 0}
    if(!is.finite(BIRDx$yrhwa[i])) {BIRDx$yrhwa[i] <- 0}
  }
  
  BIRDx$year_offset <- (BIRDx$Year - BIRDx$YearInfested - offset)
  for(i in 1:nrow(BIRDx)){
    if(BIRDx$YearInfested[i]==0) {BIRDx$year_offset[i] <- 0}
  }
  
  ## infoff: 'infested' route according to the delay in the effect (offset)
  BIRDx$infoff <- rep(NA,nrow(BIRDx))
  for(i in 1:nrow(BIRDx)){
    if(BIRDx$year_offset[i]<=0) {BIRDx$infoff[i] <- 0}
    if(BIRDx$year_offset[i]>0) {BIRDx$infoff[i] <- 1}
  }
  
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
  
  return(list(model = model,
              meaninf = meaninf,
              lowinf = lowinf,
              highinf = highinf,
              meaninfL = meaninfL,
              lowinfL = lowinfL,
              highinfL = highinfL,
              waic = waic
              )
         )
}

run_combinations <- function(species){
  for(i in 1:length(formulas)){
    formula <- formulas[[i]]
    for(j in 1:length(offsets)){
      off <- offsets[j]
      SPECIES_MOD_DAT <- paste("data/species/", species, ".rds",sep= "")
      BIRDtab <- readRDS(SPECIES_MOD_DAT)
      resu <- run_model(off,BIRDtab,formula)
      name <- paste(species,"_model",i,"_",off,"yrs", sep="")
      assign(name,resu)
      print(name)
      name2 <- paste("data/models_res/",name,".RDS", sep="")
      saveRDS(object = get(name), file = name2)
    }
  }
}


run_combinations("MODO")






