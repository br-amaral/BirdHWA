# SENSITIVITY -----------------------------
# Sensitivity test of the best model and offset year
## sensitivity is to remove each route at a time and re-fit the model

# Input: same as the 6_model:
#        /data/hexmap.graph
#        data/src/sps_list.csv  
#        data/species/{species}.rds
#        5_formulasModels.R (sourcing)
# Output: 
#        data/models_resnew/{species}/sensi (folder) - sensitivity folder for each species
#        data/models_resnew/{species}/sensi/coefs_{species}.rds - coefficients for each sensitivity model
#        data/models_resnew/{species}/sensi/premsensi.rds - results of all coefficients saved each iteration (if get an error)

# Load packages ----------------------------
library(INLA)
library(tidyverse)
library(glue)
library(fs)

# load data ------------------------------
set.seed(10)

SPECIES_DATA_PATH <- path("data/src/sps_list.csv")
source("5_formulasModels.R")
hex.adj <- paste0(getwd(),"/data/hexmap.graph")
WAIC_PATH <- path("data/waicbest.rds")

waic_best3 <- read_rds(WAIC_PATH)
sps_list <- read_csv(SPECIES_DATA_PATH)

# best model and lag for each species info
yrmod <- waic_best3 %>% 
  dplyr::select(species, model, year) %>% 
  rename(species2 = species) %>% 
  mutate(control = c(rep(0,7),
                     rep(1,7)))

# functions -----------------------------
## create dataset for a species with the proper year offset
create_data <- function(offset2, BIRDx) {
  # Create an year offset for that species 
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

## run best model with best offset year ---------------------------------
run_model <- function(BIRDx_sub, formula) {
  model <- inla(formula, family="poisson", data=BIRDx_sub, 
                control.predictor=list(compute=TRUE), 
                control.compute=list(waic=TRUE, dic=TRUE, cpo=TRUE))
  return(model)
}

## run sensitivity analysis --------------------------------
run_sensi <- function(species) {
  SPECIES_MOD_DAT <- glue("data/species/{species}.rds")
  BIRDtab <- readRDS(SPECIES_MOD_DAT)
  
  species <- pull(species)
  
  offsets <- yrmod %>% 
    filter(species2 == species) %>% 
    dplyr::select(year) %>% 
    pull()
  
  mod <- yrmod %>% 
    filter(species2 == species) %>% 
    dplyr::select(model) %>% 
    pull()
  
  formula <- formulas[[mod]]
  
  BIRDtab2 <- create_data(offsets, BIRDtab)
  
  routes <- BIRDtab2 %>% dplyr::select(RouteId) %>% distinct() %>% arrange()

  dir.create(glue("data/models_resnew/{species}/sensi"))
  intercept <- matrix(NA, nrow = nrow(routes), ncol = 3) %>%
    as_tibble()
  
  colnames(intercept) <- c("mean", "low", "up")
  
  intercept <- intercept %>% 
    mutate(mean = as.numeric(mean),
           low = as.numeric(low),
           up = as.numeric(up))
  intercept <- as.data.frame(intercept)
  intercept <- cbind(routes,intercept)
  intercept[(nrow(routes)) + 1,] <- NA
  intercept[(nrow(routes)) + 1,1] <- 'full'
  
  year_offset <- infoff <- NewObserver <- temp_min_scale <- year_offset.infoff <-
    year_offset.temp_min_scale <- infoff.temp_min_scale <-  year_offset.infoff.temp_min_scale <- intercept
  off <- offsets
  

  routes <- routes[-which(routes$RouteId == 90005),]
  # remove one route at the time
  for(i in 1:nrow(routes)){
    
    BIRDtab3 <- BIRDtab2[which(BIRDtab2$RouteId != as.character(routes[i,1])),]
    
    resu <- run_model(BIRDtab3, formula)
    name <- glue("{species}_model_{off}yrs_{routes[i,1]}")
    assign(name, resu)
    print(name)
    name2 <- glue("data/models_resnew/{species}/sensi/{name}.rds", sep= "")
    
    coefs <- resu$summary.fixed[,c(1,3,5)]
    
    intercept[i,2:4] <- coefs["(Intercept)",]
    year_offset[i,2:4] <- coefs["year_offset",]
    infoff[i,2:4] <- coefs["infoff",]
    NewObserver[i,2:4] <- coefs["NewObserverTRUE",]
    temp_min_scale[i,2:4] <- coefs["temp_min_scale",]
    year_offset.infoff[i,2:4] <-  ifelse(!is.na(sum(coefs["year_offset:infoff",])), coefs["year_offset:infoff",], 
                                         ifelse(!is.na(sum(coefs["infoff:year_offset",])), coefs["infoff:year_offset",], NA))
    year_offset.temp_min_scale[i,2:4] <- ifelse(!is.na(sum(coefs["year_offset:temp_min_scale",])), coefs["year_offset:temp_min_scale",], 
                                                ifelse(!is.na(sum(coefs["temp_min_scale:year_offset",])), coefs["temp_min_scale:year_offset",], NA))
    infoff.temp_min_scale[i,2:4] <- ifelse(!is.na(sum(coefs["infoff:temp_min_scale",])), coefs["infoff:temp_min_scale",], 
                                                ifelse(!is.na(sum(coefs["temp_min_scale:infoff",])), coefs["temp_min_scale:infoff",], NA))
    year_offset.infoff.temp_min_scale[i,2:4] <- 
      ifelse(!is.na(sum(coefs["year_offset:temp_min_scale:infoff",])), coefs["year_offset:temp_min_scale:infoff",], 
             ifelse(!is.na(sum(coefs["year_offset:infoff:temp_min_scale",])), coefs["year_offset:infoff:temp_min_scale",],
                    ifelse(!is.na(sum(coefs["temp_min_scale:year_offset:infoff",])), coefs["temp_min_scale:year_offset:infoff",],
                           ifelse(!is.na(sum(coefs["temp_min_scale:infoff:year_offset",])), coefs["temp_min_scale:infoff:year_offset",],
                                  ifelse(!is.na(sum(coefs["infoff:temp_min_scale:year_offset",])), coefs["infoff:temp_min_scale:year_offset",],
                                         ifelse(!is.na(sum(coefs["infoff:temp_min_scale:infoff",])), coefs["infoff:temp_min_scale:infoff",], NA
                                         ))))))
    premsensi <- list(intercept,
                      year_offset,
                      infoff,
                      NewObserver,
                      temp_min_scale,
                      year_offset.infoff,
                      year_offset.temp_min_scale,
                      infoff.temp_min_scale,
                      year_offset.infoff.temp_min_scale
    )
    
    name3 <- glue("data/models_resnew/{species}/sensi/premsensi.rds")
    
    write_rds(premsensi, file = name3)
    
    #saveRDS(object = get(name), file = name2)
    rm(resu)
    rm(BIRDtab3)
  }
  
  resu2 <- run_model(BIRDtab2, formula)
  coefsf <- resu2$summary.fixed[,c(1,3,5)]
  
  intercept[(nrow(routes)) + 1,2:4] <- coefsf["(Intercept)",]
  year_offset[(nrow(routes)) + 1,2:4] <- coefsf["year_offset",]
  infoff[(nrow(routes)) + 1,2:4] <- coefsf["infoff",]
  NewObserver[(nrow(routes)) + 1,2:4] <- coefsf["NewObserverTRUE",]
  temp_min_scale[(nrow(routes)) + 1,2:4] <- coefsf["temp_min_scale",]
  year_offset.infoff[(nrow(routes)) + 1,2:4] <-  ifelse(!is.na(sum(coefsf["year_offset:infoff",])), coefsf["year_offset:infoff",], 
                                               ifelse(!is.na(sum(coefsf["infoff:year_offset",])), coefsf["infoff:year_offset",], NA))
  year_offset.temp_min_scale[(nrow(routes)) + 1,2:4] <- ifelse(!is.na(sum(coefsf["year_offset:temp_min_scale",])), coefsf["year_offset:temp_min_scale",], 
                                                      ifelse(!is.na(sum(coefsf["temp_min_scale:year_offset",])), coefsf["temp_min_scale:year_offset",], NA))
  infoff.temp_min_scale[(nrow(routes)) + 1,2:4] <- ifelse(!is.na(sum(coefsf["infoff:temp_min_scale",])), coefsf["infoff:temp_min_scale",],
                                                 ifelse(!is.na(sum(coefsf["temp_min_scale:infoff",])), coefsf["temp_min_scale:infoff",], NA))
  year_offset.infoff.temp_min_scale[(nrow(routes)) + 1,2:4] <- 
    ifelse(!is.na(sum(coefsf["year_offset:temp_min_scale:infoff",])), coefsf["year_offset:temp_min_scale:infoff",], 
           ifelse(!is.na(sum(coefsf["year_offset:infoff:temp_min_scale",])), coefsf["year_offset:infoff:temp_min_scale",],
                  ifelse(!is.na(sum(coefsf["temp_min_scale:year_offset:infoff",])), coefsf["temp_min_scale:year_offset:infoff",],
                         ifelse(!is.na(sum(coefsf["temp_min_scale:infoff:year_offset",])), coefsf["temp_min_scale:infoff:year_offset",],
                                ifelse(!is.na(sum(coefsf["infoff:temp_min_scale:year_offset",])), coefsf["infoff:temp_min_scale:year_offset",],
                                       ifelse(!is.na(sum(coefsf["infoff:temp_min_scale:infoff",])), coefsf["infoff:temp_min_scale:infoff",],NA
                                       ))))))
  
  intercept$par <- "intercept"
  year_offset$par <- "year_offset"
  infoff$par <- "infoff"
  NewObserver$par <- "NewObserver"
  temp_min_scale$par <- "temp_min_scale"
  year_offset.infoff$par <- "year_offset.infoff"
  year_offset.temp_min_scale$par <- "year_offset.temp_min_scale"
  infoff.temp_min_scale$par <- "infoff.temp_min_scale"
  year_offset.infoff.temp_min_scale$par <- "year_offset.infoff.temp_min_scale"
  
  intercept$par2 <- "B0"
  year_offset$par2 <- "B1"
  infoff$par2 <- "B2"
  NewObserver$par2 <- "B8"
  temp_min_scale$par2 <- "B4"
  year_offset.infoff$par2 <- "B3"
  year_offset.temp_min_scale$par2 <- "B5"
  infoff.temp_min_scale$par2 <- "B6"
  year_offset.infoff.temp_min_scale$par2 <- "B7"
  
  plot_tib <- rbind(intercept, year_offset, infoff, NewObserver, temp_min_scale, year_offset.infoff,
                    year_offset.temp_min_scale, infoff.temp_min_scale,  year_offset.infoff.temp_min_scale)
  
  saveRDS(plot_tib, file = glue("data/models_resnew/{species}/sensi/coefs_{species}.rds", sep= ""))
  
  }

# run functions to all species ----------------------------
for(i in 1:nrow(sps_list)){
  species <- sps_list[i,]
  run_sensi(species = species)
}



