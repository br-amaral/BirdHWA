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

# species <- "BHVI"
offsets <- 2
mod <- 1

set.seed(10)
SPECIES_DATA_PATH <- "data/src/sps_list.csv"
source("5_formulasModels.R")
sps_list <- read_csv(SPECIES_DATA_PATH)
hex.adj <- paste0(getwd(),"/data/hexmap.graph")
formula <- get(glue("formula{mod}"))

create_data <- function(offset2, BIRDx) {
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

# function to randomize infestation year
permute_data <- function(offset2, BIRDy){
  # all
  numb_rou <- length(unique(BIRDy$RouteId))
  infes_years <- sort(unique(BIRDy$YearInfested))
  len_inf <- length(infes_years) - 2
  infes_years <- c(infes_years, rep(0,len_inf))
  infyr_vec <- sample(x = infes_years, size = numb_rou, replace = T)
  perm_key <- as.data.frame(cbind(unique(BIRDy$RouteId),infyr_vec))
  colnames(perm_key) <- c("RouteId", "YearInfestedPerm")
  BIRDy2 <- left_join(BIRDy, perm_key, by = "RouteId") %>% 
    rename( old_yearinfested = YearInfested,
            YearInfested = YearInfestedPerm)
  
  ## permute data keeping the same number of infested routes in each year, but randomizing the roues where infestation arrived
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
  new_inf_rou <- as_tibble(matrix(NA, ncol = 2, nrow = length(routes)))
  colnames(new_inf_rou) <- c("RouteId", "NewYearInfested")
  new_inf_rou$RouteId <- routes
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  # sample x number of routes for each year
  for(i in 1:length(yr_inf_tot)){
    yr <- as.numeric(names(yr_inf_tot[i]))
    times <- as.numeric(yr_inf_tot[i])
    samp_rou <- sample(x = routes, size = times, replace = FALSE)
    
    new_inf_rou[which(new_inf_rou$RouteId %in% samp_rou),2] <- yr
    
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
  
  ## Create an year offset for that species ------------------  
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

run_sensi <- function(species, offsets) {
  SPECIES_MOD_DAT <- glue("data/species/{species}.rds")
  BIRDtab <- readRDS(SPECIES_MOD_DAT)
  BIRDtab2 <- create_data(offsets, BIRDtab)
  off <- offsets
  
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
    
    #saveRDS(object = get(name), file = name2)
    rm(resu)
    rm(BIRDtab3)
    rm(coefs)
  }
  
  resu <- run_model(BIRDtab2, formula)
  coefs <- resu$summary.fixed[,c(1,3,5)]
  
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
  
  plot_tib2 <- plot_tib[1:9, ]
  plot_tib2[1:9, ] <- NA
  plot_tib2$mean <- coefs$mean
  plot_tib2$low <- coefs$`0.025quant`
  plot_tib2$up <- coefs$`0.975quant`
  plot_tib2$par2 <- c("B0", "B1", "B2", "B8", "B4", "B3", "B5", "B6", "B7")
  
  ptt <- ggplot(data = plot_tib, aes(x = par2, y = mean)) +
    geom_jitter(col = "gray") +
    #geom_errorbar(aes(ymin=low, ymax=up),
    #              size=.3,    # Thinner lines
    #              width=.2) 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="none") +
    ylab("Estimates") +
    xlab("Coefficients") +
    geom_point(data = plot_tib2, aes(x = par2, y = mean)) +
    geom_errorbar(data = plot_tib2, aes(ymin=low, ymax=up),
                  size=.3, width=0.1) +
    ggtitle("Sensitivity Analysis")
  
  saveRDS(plot_tib, file = glue("data/models_res/{species}/sensi/coefs_{species}.rds", sep= ""))
  saveRDS(ptt, file = glue("data/models_res/{species}/sensi/sensiplot_{species}.rds", sep= ""))  
  
}

run_perm <- function(species, perm, offsets) {
  off <- offsets
  SPECIES_MOD_DAT <- glue("data/species/{species}.rds")
  BIRDtab <- readRDS(SPECIES_MOD_DAT)
  BIRDtab2 <- create_data(offsets, BIRDtab)
  
  dir.create(glue("data/models_res/{species}/perm"))
  
  intercept <- matrix(NA, nrow = perm, ncol = 3) %>%
    as_tibble()
  
  colnames(intercept) <- c("mean", "low", "up")
  
  intercept <- intercept %>% 
    mutate(mean = as.numeric(mean),
           low = as.numeric(low),
           up = as.numeric(up))
  intercept <- as.data.frame(intercept)
  
  year_offset <- infoff <- NewObserver <- temp_min_scale <- year_offset.infoff <-
    year_offset.temp_min_scale <- infoff.temp_min_scale <-  year_offset.infoff.temp_min_scale <- intercept
  
  for(i in 1:perm){
    BIRDtab3 <- permute_data(offsets, BIRDtab2)
    
    resu <- run_model(BIRDtab3, formula)
    name <- glue("{species}_model_{off}yrs_perm{i}")
    #assign(name, resu)
    print(i)
    #name2 <- glue("data/models_res/{species}/perm/{name}.rds", sep= "")
    
    coefs <- resu$summary.fixed[,c(1,3,5)]
    
    intercept[i,1:3] <- coefs["(Intercept)",]
    year_offset[i,1:3] <- coefs["year_offset",]
    infoff[i,1:3] <- coefs["infoff",]
    NewObserver[i,1:3] <- coefs["NewObserverTRUE",]
    temp_min_scale[i,1:3] <- coefs["temp_min_scale",]
    year_offset.infoff[i,1:3] <- coefs["year_offset:infoff",]
    year_offset.temp_min_scale[i,1:3] <- coefs["year_offset:temp_min_scale",]
    infoff.temp_min_scale[i,1:3] <- coefs["infoff:temp_min_scale",]
    year_offset.infoff.temp_min_scale[i,1:3] <- coefs["year_offset:infoff:temp_min_scale",]
    
    #saveRDS(object = get(name), file = name2)
    rm(resu)
    rm(BIRDtab3)
    rm(name)
  }
  resu <- run_model(BIRDtab2, formula)
  coefs <- resu$summary.fixed[,c(1,3,5)]
  
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
  
  plot_tib2 <- plot_tib[1:9,]
  plot_tib2[1:9,] <- NA
  plot_tib2$mean <- coefs$mean
  plot_tib2$low <- coefs$`0.025quant`
  plot_tib2$up <- coefs$`0.975quant`
  plot_tib2$par2 <- c("B0", "B1", "B2", "B8", "B4", "B3", "B5", "B6", "B7")
  
  pt <- ggplot(data = plot_tib, aes(x = par2, y = mean)) +
    geom_jitter(col = "gray") +
    #geom_errorbar(aes(ymin=low, ymax=up),
    #              size=.3,    # Thinner lines
    #              width=.2) 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="none") +
    ylab("Estimates") +
    xlab("Coefficients") +
    geom_point(data = plot_tib2, aes(x = par2, y = mean)) +
    geom_errorbar(data = plot_tib2, aes(ymin=low, ymax=up),
                  size=.3, width=0.1) +
    ggtitle("Permutation Analysis")
  
  saveRDS(plot_tib, file = glue("data/models_res/{species}/perm/coefs_{species}.rds", sep= ""))  
  saveRDS(pt, file = glue("data/models_res/{species}/perm/permplot_{species}.rds", sep= ""))  
  
}

run_sensi(species = species, offsets = 2)
run_perm(species = species, perm = 100, offsets = 2)
