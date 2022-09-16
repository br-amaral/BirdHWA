# Permutation test of the best model and offset year
## permutation is to randomize the infestation year x times and refit the model

# Input: same as the 6_model:
#        /data/hexmap.graph
#        data/src/sps_list.csv  
#        data/species/{species}.rds
#        5_formulasModels.R (sourcing)
# Output: 
#        data/models_resnew/{species}/perm (folder): folder to hold permutation analysis results
#        data/models_resnew/{species}/perm/coefs_{species}.rds: coefficients of each permutation iteration
#        data/models_resnew/{species}/perm/premperm.rds: coefficients of each iterations saved on the end of each iteration (in case R crashes)

# Load packages --------------------------------------
library(INLA)
library(tidyverse)
library(glue)
librarty(fs)

# import datasets and source code -----------------------------
source("5_formulasModels.R")

set.seed(10)

SPECIES_DATA_PATH <- path("data/src/sps_list.csv")
hex.adj <- paste0(getwd(),"/data/hexmap.graph")
WAIC_PATH <- path("data/waicbest.rds")

waic_best3 <- read_rds(WAIC_PATH)
sps_list <- read_csv(SPECIES_DATA_PATH)
colnmaes <- colnames

## best model and lag for each species info
yrmod <- waic_best3 %>% 
  dplyr::select(species, model, year) %>% 
  rename(species2 = species) %>% 
  mutate(control = c(rep(0,7),
                     rep(1,7)))

# function to create dataset with the best year offset for the species -----------------------
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

# Function to randomize infestation year and temperature for permutation ---------------------------------
permute_data <- function(offset2, BIRDy){
  
  # permute years WITHIN route (group by)
#  BIRDy <- BIRDy %>% 
#    group_by(RouteId) %>% 
#    mutate(yr_shuf = sample(Year)) %>% 
#    ungroup()
  
  BIRDy <- BIRDy %>% 
    group_by(RouteId) %>% 
    mutate(yr_shuf = sample(Year)) %>% 
    ungroup() %>% 
    rename(Year_old = Year,
           Year = yr_shuf)
  
  # permute data keeping the same number of infested routes in each year, but randomizing the routes where infestation arrived
  # find out how many routes first infested in each calendar year
  yr_inf <- table(BIRDy$RouteId, BIRDy$YearInfested) %>% 
    as.data.frame.matrix() %>% 
    rownames_to_column("VALUE") %>% 
    as_tibble() %>% 
    mutate_if(is.numeric, ~1 * (. != 0)) %>% 
    rename("RouteId" = VALUE)
  
  yr_inf_tot <- colSums(yr_inf[,2:ncol(yr_inf)])
  sum_yrinf <- sum(yr_inf_tot)
  
  routes <- unique(BIRDy$RouteId)
  new_inf_rou <- as.data.frame(matrix(NA, ncol = 2, nrow = length(routes)))
  colnames(new_inf_rou) <- c("RouteId", "NewYearInfested")
  new_inf_rou$RouteId <- routes
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  # sample x number of routes for each year
  for(i in 1:length(yr_inf_tot)){
    yr <- as.numeric(names(yr_inf_tot[i]))
    times <- as.numeric(yr_inf_tot[i])
    samp_rou <- sample(x = routes, size = times, replace = FALSE)
    
    new_inf_rou[which(new_inf_rou$RouteId %in% samp_rou),2] <- rep(yr, length(samp_rou))
    
    routes <- routes[which(routes %!in% samp_rou)]
    #print(length(routes))
  }
  
  BIRDy2 <- left_join(BIRDy, new_inf_rou, by = "RouteId") %>% 
    rename( old_yearinfested = YearInfested,
            YearInfested = NewYearInfested)
  
  # create yrhwa
  BIRDy3 <- BIRDy2 %>% 
    mutate(yrhwa = Year - YearInfested,
           Infested = ifelse((Year - YearInfested) >= 0, 1, 0),
           Infested = replace(Infested, !is.finite(Infested), 0),
           yrhwa = replace(yrhwa, !is.finite(yrhwa), 0))
  
  # permute temperatures 
  BIRDy3$temp_min_scale <- sample(BIRDy3$temp_min_scale)
  
  # Create an year offset for that species 
  BIRDy4 <- BIRDy3 %>%  
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
}

# Function to run the best model with the randomized datasets --------------------------
run_model <- function(BIRDx_sub, formula) {
  model <- inla(formula, family="poisson", data=BIRDx_sub, 
                control.predictor=list(compute=TRUE), 
                control.compute=list(waic=TRUE, dic=TRUE, cpo=TRUE))
  return(model)
}

# Function to run permutation analysis and save the coefficent estimates for each iteration -------------------------
run_perm <- function(species, perm) {
  
  SPECIES_MOD_DAT <- glue("data/species/{species}.rds")
  BIRDtab <- readRDS(SPECIES_MOD_DAT)
  
  species <- pull(species)
  
  if(species == "CERW") {
    BIRDtab <- BIRDtab %>% filter(RouteId != 82031)} else {BIRDtab <- BIRDtab}
  
  off <- offsets <- yrmod %>% 
    filter(species2 == species) %>% 
    dplyr::select(year) %>% 
    pull()
  
  mod <- yrmod %>% 
    filter(species2 == species) %>% 
    dplyr::select(model) %>% 
    pull()
  
  BIRDtab2 <- create_data(offsets, BIRDtab)
  
  dir.create(glue("data/models_resnew/{species}/perm"))
  
  intercept <- matrix(NA, nrow = perm, ncol = 4) %>%
    as_tibble()
  
  colnames(intercept) <- c("mod","mean", "low", "up")
  
  intercept <- intercept %>% 
    mutate(mod = "perm",
           mean = as.numeric(mean),
           low = as.numeric(low),
           up = as.numeric(up))
  intercept <- as.data.frame(intercept)
  intercept[perm + 1,] <- NA
  intercept[perm + 1,1] <- 'full'
  
  year_offset <- infoff <- NewObserver <- temp_min_scale <- year_offset.infoff <-
    year_offset.temp_min_scale <- infoff.temp_min_scale <-  year_offset.infoff.temp_min_scale <- intercept
  
  print(species)
  
  formula <- formulas[[mod]]
  
  for(i in 1:perm){
    
    BIRDtab3 <- permute_data(offsets, BIRDtab2)
    
    resu <- run_model(BIRDtab3, formula)
    name <- glue("{species}_model_{off}yrs_perm{i}")
    #assign(name, resu)
    print(i)
    #name2 <- glue("data/models_resnew/{species}/perm/{name}.rds", sep= "")
    
    coefs <- resu$summary.fixed[ ,c(1,3,5)]
    
    intercept[i,2:4] <- coefs["(Intercept)",]
    year_offset[i,2:4] <- coefs["year_offset",]
    infoff[i,2:4] <- coefs["infoff",]
    NewObserver[i,2:4] <- coefs["NewObserverTRUE",]
    temp_min_scale[i,2:4] <- coefs["temp_min_scale",]
    # guarantee all order of parameter names is being identified and extracted
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
    
    premperm <- list(intercept,
                     year_offset,
                     infoff,
                     NewObserver,
                     temp_min_scale,
                     year_offset.infoff,
                     year_offset.temp_min_scale,
                     infoff.temp_min_scale,
                     year_offset.infoff.temp_min_scale
    )
    
    name3 <- glue("data/models_resnew/{species}/perm/premperm.rds")
    
    write_rds(premperm, file = name3)
    
    #saveRDS(object = get(name), file = name2)
    rm(resu)
    rm(BIRDtab3)
    rm(name)
  }
  
  resu <- run_model(BIRDtab2, formula)
  coefsf <- resu$summary.fixed[,c(1,3,5)]
  
  intercept[(perm)+1,2:4] <- coefsf["(Intercept)",]
  year_offset[(perm)+1,2:4] <- coefsf["year_offset",]
  infoff[(perm)+1,2:4] <- coefsf["infoff",]
  NewObserver[(perm)+1,2:4] <- coefsf["NewObserverTRUE",]
  temp_min_scale[(perm)+1,2:4] <- coefsf["temp_min_scale",]
  year_offset.infoff[(perm)+1,2:4] <-  ifelse(!is.na(sum(coefsf["year_offset:infoff",])), coefsf["year_offset:infoff",], 
                                               ifelse(!is.na(sum(coefsf["infoff:year_offset",])), coefsf["infoff:year_offset",], NA))
  year_offset.temp_min_scale[(perm)+1,2:4] <- ifelse(!is.na(sum(coefsf["year_offset:temp_min_scale",])), coefsf["year_offset:temp_min_scale",], 
                                                      ifelse(!is.na(sum(coefsf["temp_min_scale:year_offset",])), coefsf["temp_min_scale:year_offset",], NA))
  infoff.temp_min_scale[(perm)+1,2:4] <- ifelse(!is.na(sum(coefsf["infoff:temp_min_scale",])), coefsf["infoff:temp_min_scale",],
                                                 ifelse(!is.na(sum(coefsf["temp_min_scale:infoff",])), coefsf["temp_min_scale:infoff",], NA))
  year_offset.infoff.temp_min_scale[(perm)+1,2:4] <- 
    ifelse(!is.na(sum(coefsf["year_offset:temp_min_scale:infoff",])), coefsf["year_offset:temp_min_scale:infoff",], 
           ifelse(!is.na(sum(coefsf["year_offset:infoff:temp_min_scale",])), coefsf["year_offset:infoff:temp_min_scale",],
                  ifelse(!is.na(sum(coefsf["temp_min_scale:year_offset:infoff",])), coefsf["temp_min_scale:year_offset:infoff",],
                         ifelse(!is.na(sum(coefsf["temp_min_scale:infoff:year_offset",])), coefsf["temp_min_scale:infoff:year_offset",],
                                ifelse(!is.na(sum(coefsf["infoff:temp_min_scale:year_offset",])), coefsf["infoff:temp_min_scale:year_offset",],
                                       ifelse(!is.na(sum(coefsf["infoff:temp_min_scale:infoff",])), coefsf["infoff:temp_min_scale:infoff",], NA
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
  
  premperm2 <- list(intercept,
                   year_offset,
                   infoff,
                   NewObserver,
                   temp_min_scale,
                   year_offset.infoff,
                   year_offset.temp_min_scale,
                   infoff.temp_min_scale,
                   year_offset.infoff.temp_min_scale
  )
  
  coefkey <- as_tibble(matrix(NA, ncol = 2, nrow = 9))
  coefkey$V1 <- c("intercept", "year_offset", "infoff", "NewObserver", "temp_min_scale",
                  "year_offset.infoff", "year_offset.temp_min_scale", "infoff.temp_min_scale",
                  "year_offset.infoff.temp_min_scale")
  
  coefkey$V2 <- c("(Intercept)", "year_offset", "infoff", "NewObserverTRUE", "temp_min_scale",                
                  "year_offset:infoff", "year_offset:temp_min_scale", "temp_min_scale:infoff",
                  "year_offset:temp_min_scale:infoff")
  
  coefs_mod <- coefkey %>% 
    filter(V2 %in% resu$names.fixed) %>% 
    mutate(get_p = paste0("coefs_mod$V1[", 1:n(), "]"))
  
  plot_tib <- plyr::ldply(premperm2)   
  
  saveRDS(plot_tib, file = glue("data/models_resnew/{species}/perm/coefs_{species}.rds", sep= ""))  

}

# run analysis for each species ----------------------------------
for(i in 1:nrow(sps_list)){
  species <- sps_list[i,]
  run_perm(species = species, perm = 1000) 
}

# troubleshooting: ----------------------------------
#   error on loop, use the data/models_resnew/{species}/perm/premperm.rds file 
#   to recover the coeficients for the ierations already ran and not restart"
#   rename the premperm (preliminary permutation) rds file in the folder, run from when it crashed,
#      and merge them using this code. Remeber to recreate/extarct each of the tibbles from the list
#      for each parameter (intercept <- premperm1[[1]], and so forth) to get rest of code running
#   this works for the sensitivity analysis as well
a <- purrr::map2(premsensi2,premsensi1,rbind)
for(i in 1:length(premsensi2)){
  premsensi1[[i]][1:652,] <-  premsensi2[[i]][1:652,]
}
