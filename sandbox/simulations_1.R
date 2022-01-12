library(lme4)
library(tidyverse)
library(dummies)
library(simglm)
library(INLA)
library(glue)
library(gridExtra)

## Simulate bird numbers with existing covariate data -------------------------------
modres <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/models_res/HETH/HETH_fullmodel.rds")
BIRDtab <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/species/HETH.rds")

reps <- 1000

# fixed
b0 <- modres$summary.fixed$mean[1]
b1 <- modres$summary.fixed$mean[2]        # year_offset
b2 <- modres$summary.fixed$mean[3]        # infoff
b8 <- modres$summary.fixed$mean[4]        # NewObserver
b4 <- modres$summary.fixed$mean[5]        # temp_min_scale
# add interaction
b3 <- modres$summary.fixed$mean[6]        # year_offset:infoff  
b5 <- modres$summary.fixed$mean[7]        # year_offset:temp_min_scale 
b6 <- modres$summary.fixed$mean[8]        # infoff:temp_min_scale   
b7 <- modres$summary.fixed$mean[9]        # year_offset:infoff:temp_min_scale

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


#hist(y)
#hist(BIRDtab$SpeciesTotal)

X3 <- cbind(as.data.frame(X2), X$ObserverRoute, X$Year, X$hexID)
colnames(X3)[9:11] <- c("ObserverRoute", "Year", "hexID")

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

hex.adj <- paste0("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/hexmap.graph")

final <- cbind(c(b0, betas), modres$summary.fixed$`0.025quant`, modres$summary.fixed$`0.975quant`) 
final <- as.tibble(final)
colnames(final) <- c("esti","estiLO", "estiUP")

final2 <- as_tibble(matrix(NA, nrow = nrow(final), ncol = reps))

for(i in 1:reps){
  
  y <- rpois(length(lambda),lambda)
  BIRDx <- cbind(y, X3)
  colnames(BIRDx)[1] <- "SpeciesTotal"
  
  m1in <- inla(formula1, family="poisson", data=BIRDx, 
               control.predictor=list(compute=TRUE), 
               control.compute=list(waic=TRUE, dic=TRUE, cpo=TRUE))
  
  final2[,i] <- m1in$summary.fixed$mean
  if(i > 1){BIRDx2 <- rbind(BIRDx2,BIRDx)} else {
    BIRDx2 <- BIRDx
  }
  rm(m1in)
  print(i)
  
}

final2 <- cbind(final2, c("b0", "b1", "b2", "b3", "b4", "b5", 
                          "b6", "b7", "b8"))
colnames(final2)[reps+1] <- "coef"

b0_si <- final2[1,-(reps+1)]%>% 
  t() %>% 
  as_tibble() %>% 
  mutate(coe = "b0") %>% 
  rename(coefval = `1`) %>% 
  slice(-c(1,2,3))

b1_si <- final2[2,-(reps+1)] %>% 
  t() %>% 
  as_tibble() %>% 
  mutate(coe = "b1") %>% 
  rename(coefval = `2`) %>% 
  slice(-c(1,2,3))

b2_si <- final2[3,-(reps+1)]%>% 
  t() %>% 
  as_tibble() %>% 
  mutate(coe = "b2") %>% 
  rename(coefval = `3`) %>% 
  slice(-c(1,2,3))

b3_si <- final2[6,-(reps+1)]%>% 
  t() %>% 
  as_tibble() %>% 
  mutate(coe = "b5") %>% 
  rename(coefval = `6`) %>% 
  slice(-c(1,2,3))

b4_si <- final2[5,-(reps+1)]%>% 
  t() %>% 
  as_tibble() %>% 
  mutate(coe = "b4") %>% 
  rename(coefval = `5`) %>% 
  slice(-c(1,2,3))

b5_si <- final2[7,-(reps+1)]%>% 
  t() %>% 
  as_tibble() %>% 
  mutate(coe = "b8") %>% 
  rename(coefval = `7`) %>% 
  slice(-c(1,2,3))

b6_si <- final2[8,-(reps+1)]%>% 
  t() %>% 
  as_tibble() %>% 
  mutate(coe = "b6") %>% 
  rename(coefval = `8`) %>% 
  slice(-c(1,2,3))

b7_si <- final2[9,-(reps+1)]%>% 
  t() %>% 
  as_tibble() %>% 
  mutate(coe = "b7") %>% 
  rename(coefval = `9`) %>% 
  slice(-c(1,2,3))

b8_si <- final2[4,-(reps+1)]%>% 
  t() %>% 
  as_tibble() %>% 
  mutate(coe = "b3") %>% 
  rename(coefval = `4`) %>% 
  slice(-c(1,2,3))


bs_si <- rbind(b0_si, b1_si, b2_si, b3_si, b4_si,
               b5_si, b6_si, b7_si, b8_si)

betas2 <- c(b0, betas) %>% 
  as_tibble
coef_vec <- as.vector(final2$coef)
betas2 <- cbind(betas2, coef_vec)
colnames(betas2)[2] <- "coef"

final_props <- as_tibble(matrix(NA, nrow = 9, ncol = reps)) %>% 
  mutate_if(is.logical,as.numeric)

for(k in 1:nrow(final_props)){
  for(l in 1:ncol(final_props)){
    
    if(is.na(final2[k,l])) {final_props[k,l] <- 0} else {
      
      if(between(final2[k,l], final[k,2], final[k,3]) ) {
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

final$par <- c("b0", "b1", "b2", "b3", "b4", "b5", 
                      "b6", "b7", "b8")
ggplot() +
  geom_linerange(data = final, 
                 mapping = aes(x = par, y = esti,
                               ymin = estiLO, ymax = estiUP,
                               lwd = 6),
                 color = "grey", alpha = 0.7, size = 12) +
  geom_crossbar(data = final, mapping = aes(x = par, y = esti, 
                                            ymin = esti , ymax = esti)) +
  #scale_y_continuous(limits=c(min(final2[,-(reps+1)]),
  #                            max(final2[,-(reps+1)]))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        axis.title.x=element_blank()) +
  ggtitle("Simulation Coefficient Overlap") +
  geom_point(data = bs_si,
             aes(x = coe,
                 y = coefval),
             size = 0.75,
             position = position_jitter(width = 0.08)) +
  scale_x_discrete(labels= coeflabs)  



t1s <- quantile(BIRDx$SpeciesTotal, c(0.2, 0.5, 0.8))[1]
t2s <- quantile(BIRDx$SpeciesTotal, c(0.2, 0.5, 0.8))[2]
t3s <- quantile(BIRDx$SpeciesTotal, c(0.2, 0.5, 0.8))[3]

t1d <- quantile(BIRDtab$SpeciesTotal, c(0.2, 0.5, 0.8))[1]
t2d <- quantile(BIRDtab$SpeciesTotal, c(0.2, 0.5, 0.8))[2]
t3d <- quantile(BIRDtab$SpeciesTotal, c(0.2, 0.5, 0.8))[3]


a <- ggplot(data = BIRDx, aes(x = SpeciesTotal)) +
  geom_histogram() +
  theme_bw() +
  geom_vline(xintercept = t1s, size=0.5, color = "black") +
  geom_vline(xintercept = t2s, size=0.5, color = "black") +
  geom_vline(xintercept = t3s, size=0.5, color = "black")

b <- ggplot(data = BIRDtab, aes(x = SpeciesTotal)) +
  geom_histogram() +
  theme_bw() +
  geom_vline(xintercept = t1d, size=0.5, color = "black") +
  geom_vline(xintercept = t2d, size=0.5, color = "black") +
  geom_vline(xintercept = t3d, size=0.5, color = "black")

grid.arrange(a, b, nrow=2)



sps_preds <- readRDS("data/preds/preds_HETH.rds") %>% 
  mutate(species = "HETH",
         prediction = NA) 

for(i in 1:nrow(sps_preds)){
  if(sps_preds$HWA == "no_infest") {
  sps_preds$prediction[i] <- exp(
        b0 + (b1 * sps_preds$year_off_t) + (b3 * sps_preds$temp_t) +
          (b5 * sps_preds$year_off_t * sps_preds$temp_t))
  }
  if(sps_preds$HWA == "infest") {
    sps_preds$prediction[i] <- 
  }
}

sps_preds2 <- sps_preds %>% 
  filter(year == 20)
sps_preds2$prop <- NA

for(i in seq(from=1, to=nrow(sps_preds2), by=2)) {
  sps_preds2$prop[i] <- sps_preds2$prop[i+1] <- log(sps_preds2$prediction[i]/sps_preds2$prediction[i+1])
}

sps_preds3 <- sps_preds2 %>% 
  filter(HWA == "infest")   ## get only a copy from prop

ggplot(data = sps_preds3, aes(y= species, x = prop)) +
  geom_vline(xintercept = 0,
             col = "gray43",
             linetype = "dotted",
             size = 1) +
  geom_point(aes(shape = temp, color = temp), size = 2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #legend.title = element_blank(),
        legend.position = "right",
        legend.justification = "right",
        #legend.margin=margin(0,0,0,0),
        #legend.box.margin=margin(-5,0,-5,-7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5) +
  #scale_x_continuous(breaks = seq(-3, 1, 0.5),
  #                   limits = c(-3, 1, 0.5)) +
  scale_color_manual(values = c("t1" = "blue4",
                                "t2" = "violetred",
                                "t3" = "darkorange3"),
                     labels = c("t1" = "0.2",
                                "t2" = "0.5",
                                "t3" = "0.8"),
                     name = "Temperature\nQuantiles")
