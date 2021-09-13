library(lme4)
library(tidyverse)
library(dummies)
library(simglm)
library(INLA)
library(glue)

## Simulate bird numbers with existing covariate data -------------------------------
modres <- readRDS("~/Documents/HETH_model1_2yrs.rds")

BIRDtab <- read_rds("C:/Users/bzr69/OneDrive - The Pennsylvania State University/Aug5_run_models_control/data/species/HETH.rds")
BIRDtab <- read_rds("~/Documents/HETH.rds")

# fixed
b0 <- modres$summary.fixed$mean[1]
b1 <- modres$summary.fixed$mean[2]        # year_offset
b2 <- modres$summary.fixed$mean[3]        # infoff
b3 <- modres$summary.fixed$mean[4]        # NewObserver
b4 <- modres$summary.fixed$mean[5]        # temp_min_scale
# add interaction
b5 <- modres$summary.fixed$mean[6]        # year_offset:infoff  
b6 <- modres$summary.fixed$mean[7]        # year_offset:temp_min_scale 
b7 <- modres$summary.fixed$mean[8]        # infoff:temp_min_scale   
b8 <- modres$summary.fixed$mean[9]        # year_offset:infoff:temp_min_scale

offset <- 2

X <- BIRDtab %>%  
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
         infoff = ifelse(year_offset <= 0, 0, ifelse(year_offset > 0, 1, NA)),
         ObserverRoute = as.character(ObserverRoute),
         temp_min_scale = as.numeric(temp_min_scale)
         ) 
colnames(X)[9] <- "data"

X2 <- X %>% 
  select(year_offset,
         infoff,
         NewObserver,
         temp_min_scale) %>% 
  mutate(year_offset.infoff = year_offset * infoff,
         temp_min_scale.year_offset = temp_min_scale * year_offset,
         temp_min_scale.infoff = temp_min_scale * infoff,
         temp_min_scale.infoff.year_offset = temp_min_scale * infoff * year_offset) %>% 
  as.matrix()

  Z1 <- model.matrix(~ 0 + X$ObserverRoute)  
  Z2 <- model.matrix(~ 0 + as.character(X$Year))
  Z3 <- model.matrix(~ 0 + as.character(X$hexID))
  
  g1 <- modres$summary.random$ObserverRoute[,2]
  g2 <- modres$summary.random$Year[,2]
  g3 <- modres$summary.random$hexID[,1:2] %>% 
    as.tibble() %>% 
    filter(ID %in% X$hexID) %>% 
    select(mean) %>% 
    pull(mean)

  betas <- c(b1, b2, b3, b4, b5, b6, b7, b8)
    
  lambda <- exp(b0 + X2 %*% betas +
               Z1 %*% g1 +    #X$ObserverRoute
               Z2 %*% g2 +    #X$Year
               Z3 %*% g3
           )
  
  y <- rpois(length(lambda),lambda)
  
  hist(y)
  hist(BIRDtab$SpeciesTotal)

  X3 <- cbind(as.data.frame(X2), X$ObserverRoute, X$Year, X$hexID)
  colnames(X3)[9:11] <- c("ObserverRoute", "Year", "hexID")
  
  BIRDx <- cbind(y, X3)
  colnames(BIRDx)[1] <- "SpeciesTotal"
    
  formula1 <- SpeciesTotal ~ 1 + 
    year_offset + 
    infoff +
    year_offset : infoff +
    NewObserver +
    temp_min_scale +
    temp_min_scale : year_offset +
    temp_min_scale : infoff +
    temp_min_scale : infoff : year_offset +
    f(ObserverRoute, model="iid") + 
    f(Year, model="iid") +
    f(hexID, model="bym", graph=hex.adj, constr=TRUE)  
    
  hex.adj <- paste0("~/Documents/hexmap.graph")

  m1in <- inla(formula1, family="poisson", data=BIRDx, 
               control.predictor=list(compute=TRUE), 
               control.compute=list(waic=TRUE, dic=TRUE, cpo=TRUE))
  
  final <- cbind(c(b0, betas), modres$summary.fixed$`0.025quant`, modres$summary.fixed$`0.975quant`,
                 m1in$summary.fixed$mean, m1in$summary.fixed$`0.025quant`, m1in$summary.fixed$`0.975quant`) 
  final <- as.tibble(final)
  colnames(final) <- c("esti","estiLO", "estiUP", "simu", "simuLO", "simuUP")
  
  ggplot(aes(x = esti, y = simu), data = final) +
    geom_abline(intercept = 0, slope = 1, colour = "gray") +
    geom_point(size = 2) +
    theme_bw() +
    geom_errorbar(aes(xmax = simuLO,
                       xmin = simuUP),
                   height = .1, size = 0.5) +
    geom_errorbarh(aes(xmax = estiLO,
                       xmin = estiUP),
                   height = .1, size = 0.5,
                   data = final[,1:3]) 
    
  reps <- 100
  final2 <- as_tibble(matrix(NA, nrow = nrow(final), ncol = reps + 3))
  final2[,1] <- modres$summary.fixed$mean
  final2[,2] <- modres$summary.fixed$`0.025quant`
  final2[,3] <- modres$summary.fixed$`0.975quant`
  
  for(i in 1:reps){
    m1in <- inla(formula1, family="poisson", data=BIRDx, 
                 control.predictor=list(compute=TRUE), 
                 control.compute=list(waic=TRUE, dic=TRUE, cpo=TRUE))
    
    final2[,i+3] <- m1in$summary.fixed$mean
    rm(m1in)
    print(i)
    
  }
  
  final2 <- cbind(final2, c("b0", "b1", "b2", "b3", "b4", "b5", 
                            "b6", "b7", "b8"))
  colnames(final2)[reps+4] <- "coef"
  
  b0_si <- final2[1,-(reps+4)]%>% 
    t() %>% 
    as_tibble() %>% 
    mutate(coe = "b0") %>% 
    rename(coefval = `1`)
  
  b1_si <- final2[2,-(reps+4)] %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(coe = "b1") %>% 
    rename(coefval = `2`)
  
  b2_si <- final2[3,-(reps+4)]%>% 
    t() %>% 
    as_tibble() %>% 
    mutate(coe = "b2") %>% 
    rename(coefval = `3`)
  
  b3_si <- final2[4,-(reps+4)]%>% 
    t() %>% 
    as_tibble() %>% 
    mutate(coe = "b3") %>% 
    rename(coefval = `4`)
  
  b4_si <- final2[5,-(reps+4)]%>% 
    t() %>% 
    as_tibble() %>% 
    mutate(coe = "b4") %>% 
    rename(coefval = `5`)
  
  b5_si <- final2[6,-(reps+4)]%>% 
    t() %>% 
    as_tibble() %>% 
    mutate(coe = "b5") %>% 
    rename(coefval = `6`)
  
  b6_si <- final2[7,-(reps+4)]%>% 
    t() %>% 
    as_tibble() %>% 
    mutate(coe = "b6") %>% 
    rename(coefval = `7`)
  
  b7_si <- final2[8,-(reps+4)]%>% 
    t() %>% 
    as_tibble() %>% 
    mutate(coe = "b7") %>% 
    rename(coefval = `8`)
  
  b8_si <- final2[9,-(reps+4)]%>% 
    t() %>% 
    as_tibble() %>% 
    mutate(coe = "b8") %>% 
    rename(coefval = `9`)
  
  bs_si <- rbind(b0_si, b1_si, b2_si, b3_si, b4_si,
                 b5_si, b6_si, b7_si, b8_si)
  
  betas2 <- c(b0, betas) %>% 
    as_tibble
  coef_vec <- as.vector(final2$coef)
  betas2 <- cbind(betas2, coef_vec)
  colnames(betas2)[2] <- "coef"
  
  final_props <- as_tibble(matrix(NA, nrow = 9, ncol = reps))
  
  for(k in 1:nrow(final_props)){
    for(l in 1:ncol(final_props)){
      
      if(is.na(final2[k,l + 3])) {final_props[k,l] <- 0} else {
        
        if(between(final2[k,l + 3], final2[k,2], final2[k,3]) ) {
          final_props[k,l] <- 1} else {final_props[k,l] <- 0}
      }
    }
  }
  
  final_props2 <- rep(NA, nrow(betas2))
  
  for(m in 1:nrow(betas2)) {
     final_props2[m] <- (sum(final_props[m,]))/reps
  }
  
  coeflabs <- c(glue("intercept\n{final_props2[1]}"),
                glue("year_offset\n{final_props2[2]}"),
                glue("infoff\n{final_props2[3]}"),
                glue("NewObserver\n{final_props2[4]}"),
                glue("temp_min\n{final_props2[5]}"),
                glue("year_offset\ninfoff\n{final_props2[6]}"),
                glue("year_offset\ntemp_min\n{final_props2[7]}"),
                glue("infoff\ntemp_min\n{final_props2[8]}"),
                glue("year_offset\ninfoff\ntemp_min\n{final_props2[9]}"))
  
  ggplot() +
    geom_linerange(data = final2, 
                   mapping = aes(x = coef,
                                 ymin = V2, ymax = V3,
                                 lwd = 6),
                   color = "grey", alpha = 0.7) +
    scale_y_continuous(limits=c(min(final2[,-(reps+4)]),
                                max(final2[,-(reps+4)]))) +
    geom_crossbar(data = final2, 
                  aes(ymin = V1 , ymax = V1,
                      x = coef, y = V1),
                  size = 0.1, col="black", width = .5) +
    geom_crossbar(data = betas2, 
                  aes(ymin = value , ymax = value,
                      x = coef, y = value),
                  size = 0.1, col="red", width = .5, linetype = "dotted") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="none",
          axis.title.x=element_blank()) +
    ggtitle("Simulation Coefficient Overlap") +
    geom_point(data = bs_si,
               aes(x = coe,
                   y = coefval),
               size = 0.75) +
    scale_x_discrete(labels= coeflabs)
  
# plotar no topo quantos de quantos cairam dentro do intervalo de confianca
  
  
  
  
  
  
  
  
  
## remove set seed and run this 1000 times? which species, best models? different models?
  
