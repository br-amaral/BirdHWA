# 13_Simulation --------------------------------------------------------
#
# Input:  data/species/HETH.rds: hermit thrush BBS data formatted
#         data/hexmap.graph: 
#
# Output: data/models_resnew/{species}/sims{species}.rds: 
#         Figures/FigS5/simulation.svg:
# 

# load libraries ---------------------
library(lme4)
library(tidyverse)
library(dummies)
library(simglm)
library(INLA)
library(glue)
library(gridExtra)

species <- "HETH"
sims <- 1000
offset <- 2

## Simulate bird numbers with existing covariate data -------------------------------
BIRDtab <- readRDS(glue("data/species/{species}.rds"))

formula3 <- SpeciesTotal ~ 1 + 
  year_offset + 
  year_offset : infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  temp_min_scale : infoff : year_offset +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  


hex.adj <- paste0("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/hexmap.graph")

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

intercept <- matrix(NA, nrow = sims + 1, ncol = 4) %>%
  as_tibble()

colnames(intercept) <- c("sim","mean", "low", "up")

intercept <- intercept %>% 
  mutate(mean = as.numeric(mean),
         low = as.numeric(low),
         up = as.numeric(up))
intercept <- as.data.frame(intercept)
intercept[1,] <- NA
intercept[1,1] <- 'real'

year_offset <- infoff <- NewObserver <- temp_min_scale <- year_offset.infoff <-
  year_offset.temp_min_scale <- infoff.temp_min_scale <-  year_offset.infoff.temp_min_scale <- intercept

modres <- inla(formula3, family="poisson", data=X, 
             control.predictor=list(compute=TRUE), 
             control.compute=list(waic=TRUE, dic=TRUE, cpo=TRUE))

coefsf <- modres$summary.fixed[,c(1,3,5)]

intercept[1,2:4] <- b0 <- coefsf["(Intercept)",]
year_offset[ 1,2:4] <- b1 <- coefsf["year_offset",]
infoff[1,2:4] <- b2 <- coefsf["infoff",]
NewObserver[1,2:4] <- b8 <- coefsf["NewObserverTRUE",]
temp_min_scale[1,2:4] <- b4 <- coefsf["temp_min_scale",]
year_offset.infoff[1,2:4] <- b3 <- coefsf["year_offset:infoff",]
year_offset.temp_min_scale[1,2:4] <- b5 <- coefsf["year_offset:temp_min_scale",]
infoff.temp_min_scale[1,2:4] <- b6 <- coefsf["infoff:temp_min_scale",]
year_offset.infoff.temp_min_scale[1,2:4] <- b7 <- coefsf["year_offset:infoff:temp_min_scale",]

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

betas <- as.numeric(c(b1$mean, b2$mean, b8$mean, b4$mean,
                      b3$mean, b5$mean, b6$mean, b7$mean))
for(i in 1:length(betas)){
  if(is.na(betas[i])){
    betas[i] <- 0
  }
}

lambda <- exp(b0$mean + X2 %*% betas +
                Z1 %*% g1 +    #X$ObserverRoute
                Z2 %*% g2 +    #X$Year
                Z3 %*% g3
              )

X3 <- cbind(as.data.frame(X2), X$ObserverRoute, X$Year, X$hexID)
colnames(X3)[9:11] <- c("ObserverRoute", "Year", "hexID")

for(i in 1:sims+1){
  
  y <- rpois(length(lambda),lambda)
  BIRDx <- cbind(y, X3)
  colnames(BIRDx)[1] <- "SpeciesTotal"
  
  m1in <- inla(formula3, family="poisson", data=BIRDx, 
               control.predictor=list(compute=TRUE), 
               control.compute=list(waic=TRUE, dic=TRUE, cpo=TRUE))
  
  coefs <- m1in$summary.fixed[,c(1,3,5)]
  
  intercept[i,1] <- year_offset[i,1] <- infoff[i,1] <- NewObserver[i,1] <- temp_min_scale[i,1] <- year_offset.infoff[i,1] <- 
    year_offset.temp_min_scale[i,1] <- infoff.temp_min_scale[i,1] <- year_offset.infoff.temp_min_scale[i,1] <- i
  
  intercept[i,2:4] <- coefs["(Intercept)",]
  year_offset[i,2:4] <- coefs["year_offset",]
  infoff[i,2:4] <- coefs["infoff",]
  NewObserver[i,2:4] <- coefs["NewObserver",]
  temp_min_scale[i,2:4] <- coefs["temp_min_scale",]
  year_offset.infoff[i,2:4] <- coefs["year_offset:infoff",]
  year_offset.temp_min_scale[i,2:4] <- coefs["year_offset:temp_min_scale",]
  infoff.temp_min_scale[i,2:4] <- coefs["infoff:temp_min_scale",]
  year_offset.infoff.temp_min_scale[i,2:4] <- coefs["year_offset:infoff:temp_min_scale",]
  
  premperm <- list(intercept,
                   year_offset,
                   infoff,
                   NewObserver,
                   temp_min_scale,
                   year_offset.infoff,
                   year_offset.temp_min_scale,
                   infoff.temp_min_scale,
                   year_offset.infoff.temp_min_scale)
  
  name3 <- glue("data/models_resnew/{species}/premsims.rds")
  
  write_rds(premperm, file = name3)

  rm(m1in)
  print(i)
  
}

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

coefs <- rbind(intercept, year_offset, infoff, NewObserver, temp_min_scale, year_offset.infoff,
                  year_offset.temp_min_scale, infoff.temp_min_scale,  year_offset.infoff.temp_min_scale)

for(i in 1:nrow(coefs)){
  if(is.na(coefs$mean[i])){
    coefs$mean[i] <- 0
  }
}

plot_tib <- coefs

saveRDS(plot_tib, file = glue("data/models_resnew/{species}/sims{species}.rds", sep= ""))  

# Plotting!   -------------------------
coefs <- plot_tib <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/models_resnew/HETH/simsHETH.rds")

TEMPQUANT_PATH <- glue("data/tempquant.csv")

temp_qunts <- read_csv(TEMPQUANT_PATH)

temp_qunts2 <- temp_qunts %>% 
  as_tibble() %>% 
  rename(Species = species, t1 = ...2, t2 = ...3, t3 = ...4) %>% 
  dplyr::filter(trimws(Species) == trimws(species))

coefs2 <- coefs %>%
  select(par, mean) %>% 
  group_by(par) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = par, values_from = mean) %>%
  select(-row)

pmat <- coefs2 %>% 
  mutate(temp1 = pull(temp_qunts2[ ,2]),
         temp2 = pull(temp_qunts2[ ,3]),
         temp3 = pull(temp_qunts2[ ,4]),
         time = 20
  )

# estimates -----------
prop_tabX <- as_tibble(matrix(NA,nrow = nrow(pmat), ncol = 6))
colnames(prop_tabX) <- c("noinf1", "inf1", "noinf2", "inf2", "noinf3", "inf3")

for (i in 1:nrow(prop_tabX)){
  
  ifelse(!is.na(pmat$intercept[i]), b0 <- pmat$intercept[i], b0 <- 0)
  ifelse(!is.na(pmat$year_offset[i]), b1 <- pmat$year_offset[i], b1 <- 0)
  ifelse(!is.na(pmat$infoff[i]), b2 <- pmat$infoff[i], b2 <- 0)
  ifelse(!is.na(pmat$temp_min_scale[i]), b3 <- pmat$temp_min_scale[i], b3 <- 0)
  ifelse(!is.na(pmat$year_offset.infoff[i]), b4 <- pmat$year_offset.infoff[i], b4 <- 0)
  ifelse(!is.na(pmat$year_offset.temp_min_scale[i]), b5 <- pmat$year_offset.temp_min_scale[i], b5 <- 0)
  ifelse(!is.na(pmat$infoff.temp_min_scale[i]), b6 <- pmat$infoff.temp_min_scale[i], b6 <- 0)
  ifelse(!is.na(pmat$year_offset.infoff.temp_min_scale[i]), b7 <- pmat$year_offset.infoff.temp_min_scale[i], b7 <- 0)
  
  no_infes <- pmat[i,] %>% 
    mutate(prediction1 = exp(b0 + (b1 * time) + (b3 * temp1) + (b5 * year_offset * temp1)),
           prediction2 = exp(b0 + (b1 * time) + (b3 * temp2) + (b5 * year_offset * temp2)),
           prediction3 = exp(b0 + (b1 * time) + (b3 * temp3) + (b5 * year_offset * temp3)),
           HWA = 'infest'
    )
  
  infes <- pmat[i,] %>% 
    mutate(prediction1 = exp(b0 + (b1 * time) + (b2 * infoff) + (b3 * temp1) +
                               (b4 * time * infoff) + (b5 * time * temp1) +
                               (b6 * infoff * temp1) + (b7 * time * infoff * temp1)),
           prediction2 = exp(b0 + (b1 * time) + (b2 * infoff) + (b3 * temp2) +
                               (b4 * time * infoff) + (b5 * time * temp2) +
                               (b6 * infoff * temp2) + (b7 * time * infoff * temp2)),
           prediction3 = exp(b0 + (b1 * time) + (b2 * infoff) + (b3 * temp3) +
                               (b4 * time * infoff) + (b5 * time * temp3) +
                               (b6 * infoff * temp3) + (b7 * time * infoff * temp3)),
           HWA = 'no_infest'
    )
  prop_tabX$noinf1[i] <- no_infes$prediction1
  prop_tabX$inf1[i] <- infes$prediction1
  prop_tabX$noinf2[i] <- no_infes$prediction2
  prop_tabX$inf2[i] <- infes$prediction2
  prop_tabX$noinf3[i] <- no_infes$prediction3
  prop_tabX$inf3[i] <- infes$prediction3
  rm(no_infes, infes,
     b0, b1, b2, b3, b4, b5, b6, b7)
}

prop_tabX <- prop_tabX %>% 
  mutate(t1 = log(noinf1/inf1),
         t2 = log(noinf2/inf2),
         t3 = log(noinf3/inf3),
         Species = species)

master_full_per <- prop_tabX[1,] %>% 
  pivot_longer(`t1`:`t3`, names_to = "temp", values_to = "pop202") %>% 
  select(Species, temp, pop202) %>% 
  mutate(sps_temp = glue("{Species}_{temp}", remove = FALSE))

prop_tabX2 <- prop_tabX[2:nrow(prop_tabX),] %>% 
  pivot_longer(`t1`:`t3`, names_to = "temp", values_to = "pop202") %>% 
  select(Species, temp, pop202) %>% 
  mutate(sps_temp = glue("{Species}_{temp}", remove = FALSE))

temp_order <- as_tibble(matrix(c("t1","t2","t3",1,2,3), nrow = 3)) %>% 
  rename(temp = V1,
         orde = V2)

prop_tabX3 <- left_join(prop_tabX2, temp_order, by = "temp") %>% 
  arrange(orde)

master_pro2 <- prop_tabX3 %>% 
  rename(species = Species) %>% 
  group_by(temp) %>% 
  mutate(#up = mean(prop) + ((1.96*sqrt(var(prop) / length(prop)))),
    #lo = mean(prop) - ((1.96*sqrt(var(prop) / length(prop)))),
    up = mean(pop202) + ((1.96*sqrt(var(pop202)))),
    lo = mean(pop202) - ((1.96*sqrt(var(pop202))))) %>% 
  ungroup()

table95 <- master_pro2 %>% 
  dplyr::select(species, sps_temp, up, lo) %>% 
  distinct() 

# export figure --------------------------------
svg(glue("Figures/FigS5/simulation.svg"), 
    width = 10, height = 4.5)

ggplot(data = prop_tabX3, aes(x= sps_temp, y = pop202,
                                color = "white"), size = 2) +
  coord_flip() +
  geom_segment(aes(y=lo, yend=up, x=sps_temp, xend=sps_temp),
               size = 18, data = table95, alpha = 0.3, color = "yellow") +
  #geom_point() +
  geom_hline(yintercept = 0, col = "gray43", size = 0.8) +
  geom_hline(yintercept = -0.3, col = "gray43", linetype = "dotted", size = 0.8) +
  #geom_hline(yintercept = 0.3, col = "gray43", linetype = "dotted", size = 0.8) +
  geom_boxplot(width = 0.6, fill = "white", lwd=0.4,
               data = master_pro2, 
               aes(x= sps_temp, y = pop202),
               color= "black", size=0.08,
               outlier.size = 0.5) + #coord_flip() + theme_bw()
  #geom_point(aes(shape = temp, color = temp), size = 2) +
  #geom_boxplot() + 
  #facet_wrap(~temp) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position="none",
        #legend.justification = "right",
        #legend.margin=margin(0,0,0,0),
        #legend.box.margin=margin(-5,0,-5,-7),
        #axis.title.x = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5)) +
  geom_point(data = master_full_per, 
             aes(x= sps_temp, y = pop202,
                 shape = temp, color = temp), size = 4, alpha = 0.95) +
  scale_shape_manual(values=c("t1" = 16,
                              "t2" = 17,
                              "t3" = 15)) +
  scale_fill_manual(values=c("olivedrab4",
                             "violetred",
                             "darkorange3")) +
  scale_color_manual(values = c("t1" = "olivedrab4",
                                "t2" = "violetred",
                                "t3" = "darkorange3"),
                     labels = c("t1" = "0.2",
                                "t2" = "0.5",
                                "t3" = "0.8"),
                     name = "Temperature\nQuantiles") +
  scale_y_continuous(breaks = c(-3,-2,-1,0,1), limits  = c(-3,0.05)
  ) +
  scale_x_discrete(labels = c("0.2", "0.5", "0.8")) +
  labs(title="Simulation analysis") +
  ylab("Log(Not infested route \n      Infested route)")+
  xlab("Temperature quantiles \n")

dev.off()
