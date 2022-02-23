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

set.seed(10)
SPECIES_DATA_PATH <- "data/src/sps_list.csv"
source("5_formulasModels.R")
sps_list <- read_csv(SPECIES_DATA_PATH)
hex.adj <- paste0(getwd(),"/data/hexmap.graph")

# best model and lag for each species info
yrmod <- read_csv(file = "data/models_res/yrmod.csv") 

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

# function to randomize infestation year
permute_data <- function(offset2, BIRDy){
  
  # permute years, group by route
  BIRDy <- BIRDy %>% 
    group_by(RouteId) %>% 
    mutate(yr_shuf = sample(Year)) %>% 
    ungroup()
  
  #permute data keeping the same number of infested routes in each year, but randomizing the roues where infestation arrived
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

run_model <- function(BIRDx_sub, formula) {
  model <- inla(formula, family="poisson", data=BIRDx_sub, 
                control.predictor=list(compute=TRUE), 
                control.compute=list(waic=TRUE, dic=TRUE, cpo=TRUE))
  return(model)
}

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
  
  routes <- BIRDtab2 %>% select(RouteId) %>% distinct() %>% arrange()
  
  dir.create(glue("data/models_res/{species}/sensi"))
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
    
    name3 <- glue("data/models_res/{species}/sensi/premsensi.rds")
    
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
  
  saveRDS(plot_tib, file = glue("data/models_res/{species}/sensi/coefs_{species}.rds", sep= ""))
  
  }

run_perm <- function(species, perm) {
  
  SPECIES_MOD_DAT <- glue("data/species/{species}.rds")
  BIRDtab <- readRDS(SPECIES_MOD_DAT)
  
  species <- pull(species)
  
  off <- offsets <- yrmod %>% 
    filter(species2 == species) %>% 
    dplyr::select(year) %>% 
    pull()
  
  mod <- yrmod %>% 
    filter(species2 == species) %>% 
    dplyr::select(model) %>% 
    pull()
  
  BIRDtab2 <- create_data(offsets, BIRDtab)
  
  dir.create(glue("data/models_res/{species}/perm"))
  
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
  
  for(i in 1:perm){
    
    formula <- formulas[[mod]]
    
    BIRDtab3 <- permute_data(offsets, BIRDtab2)
    
    resu <- run_model(BIRDtab3, formula)
    name <- glue("{species}_model_{off}yrs_perm{i}")
    #assign(name, resu)
    print(i)
    #name2 <- glue("data/models_res/{species}/perm/{name}.rds", sep= "")
    
    coefs <- resu$summary.fixed[ ,c(1,3,5)]
    
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
    
    name3 <- glue("data/models_res/{species}/perm/premperm.rds")
    
    write_rds(premperm, file = name3)
    
    #saveRDS(object = get(name), file = name2)
    rm(resu)
    rm(BIRDtab3)
    rm(name)
  }
  
  resu <- run_model(BIRDtab2, formula)
  coefsf <- resu$summary.fixed[,c(1,3,5)]
  
  intercept[(perm) +1,2:4] <- coefsf["(Intercept)",]
  year_offset[(perm) +1,2:4] <- coefsf["year_offset",]
  infoff[(perm) +1,2:4] <- coefsf["infoff",]
  NewObserver[(perm) +1,2:4] <- coefsf["NewObserverTRUE",]
  temp_min_scale[(perm) +1,2:4] <- coefsf["temp_min_scale",]
  year_offset.infoff[(perm) +1,2:4] <-  ifelse(!is.na(sum(coefsf["year_offset:infoff",])), coefsf["year_offset:infoff",], 
                                       ifelse(!is.na(sum(coefsf["infoff:year_offset",])), coefsf["infoff:year_offset",], NA))
  year_offset.temp_min_scale[(perm) +1,2:4] <- ifelse(!is.na(sum(coefsf["year_offset:temp_min_scale",])), coefsf["year_offset:temp_min_scale",], 
                                              ifelse(!is.na(sum(coefsf["temp_min_scale:year_offset",])), coefsf["temp_min_scale:year_offset",], NA))
  infoff.temp_min_scale[(perm) +1,2:4] <- ifelse(!is.na(sum(coefsf["infoff:temp_min_scale",])), coefsf["infoff:temp_min_scale",],
                                         ifelse(!is.na(sum(coefsf["temp_min_scale:infoff",])), coefsf["temp_min_scale:infoff",], NA))
  year_offset.infoff.temp_min_scale[(perm) +1,2:4] <- 
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
  
  plot_tib <- matrix(ncol = 6, nrow= 0)
  colnames(plot_tib) <- c("mod","mean","low","up","par", "par2")
    
  for(i in 1:nrow(coefs_mod)){
    plot_tib <- rbind(plot_tib, get(eval(parse(text =coefs_mod$get_p[i]))))
  }
  
  saveRDS(plot_tib, file = glue("data/models_res/{species}/perm/coefs_{species}.rds", sep= ""))  
  #saveRDS(pt, file = glue("data/models_res/{species}/perm/permplot_{species}.rds", sep= ""))  
  
}

for(i in 1:nrow(sps_list)){
  species <- sps_list[i,]
  run_sensi(species = species)
  run_perm(species = species, perm = 1000)
}



