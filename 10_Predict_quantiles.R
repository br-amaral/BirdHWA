# Predict quantiles
# Input: summary_results file for each species
# Output: plot of temperature distribution
#         plot of predictions of temperature quadrants
#         table with species predictions (preds_{species}.rds)
#         table with species coefficients (coefs_{species}.rds)
# WARNING: can't run everything at once, because 'maxi' (upper limit of prediction plots) varies according
#            to species in different temperatures. has to be added manually once you look at the results

library(tidyverse)
library(gridExtra)
library(glue)

# species <- spsr <- 'BTNW'
SUM_RES_PATH <- glue("data/models_res/{species}/summary_results2.rds")

# Make predictions with the fixed values and temperature quantiles
summary_results2 <- my_tibble <- read_rds(SUM_RES_PATH)

(waic_best <- summary_results2[which(summary_results2$waic == min(summary_results2$waic)),1:4])

year_ <- waic_best$year[1]
mod_ <- waic_best$model[1]

pred_tab <- as_tibble(seq(-10,20,1)) %>% 
  rename(year = value)

create_pred_off <- function(offset_v){
  pred_tabX <- pred_tab %>% 
    mutate(infoff_t =  ifelse(year <= offset_v, 0, 1),
           year_off_t = year - offset_v)
  return(pred_tabX)
}

offset <- year_ 

SPECIES_MOD_DAT <- glue("data/species/{species}.rds")
BIRDtab <- readRDS(SPECIES_MOD_DAT)

BIRDx <- BIRDtab %>%  
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
         infoff = ifelse(year_offset <= 0, 0, ifelse(year_offset > 0, 1, NA))) 

ggplot(BIRDx, aes(x = year_offset, y = SpeciesTotal, colour = Infested)) +
  geom_point() +
  geom_smooth(aes(fill = Infested)) +
  ggtitle("-20 e +20 filtro") +
  theme_bw()

ggplot(BIRDx, aes(x = year_offset, y = SpeciesTotal)) +
  #  geom_point() +
  geom_smooth() +
  ggtitle(species) + xlim(-20,20) 

x <- as.numeric(as.matrix(BIRDx[which(BIRDx$year_offset<10),1]))

BIRDx2 <- BIRDx[which(as.numeric(BIRDx$RouteId) %in%  x) ,]

BIRDx2INF <- BIRDx2[which(BIRDx2$Infested == T),]
BIRDx2NO <- BIRDx2[which(BIRDx$Infested == F),]

mean(BIRDx2INF$temp_min_scale)
mean(BIRDx2NO$temp_min_scale)

#par(mfrow = c(2,1))
#hist(BIRDx2INF$temp_min_scale)
#hist(BIRDx2NO$temp_min_scale)

temps <- rbind(
  BIRDx2INF %>% distinct(RouteId, .keep_all = TRUE) %>% select(temp_min_scale, Infested),
  BIRDx2NO %>% distinct(RouteId, .keep_all = TRUE) %>% select(temp_min_scale, Infested)
)

(t1 <- quantile(BIRDx2INF$temp_min_scale, c(0.2, 0.5, 0.8))[1])
(t2 <- quantile(BIRDx2INF$temp_min_scale, c(0.2, 0.5, 0.8))[2])
(t3 <- quantile(BIRDx2INF$temp_min_scale, c(0.2, 0.5, 0.8))[3])

ggplot(temps, aes(x = temp_min_scale, fill = Infested)) +
  geom_histogram(aes(y = stat(count)/length(temps$temp_min_scale)),
                 position = "identity", alpha = .7,
                 bins = 15) + 
  scale_fill_manual(values=c("grey10", "grey90")) +
  geom_vline(xintercept = t1, size=0.5, color = "black") +
  geom_vline(xintercept = t2, size=0.5, color = "black") +
  geom_vline(xintercept = t3, size=0.5, color = "black") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size= 12),
        axis.title = element_text(size = 12),
        legend.position = "none",
        #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        #axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) + 
  xlab("Mean minimum temperature") +
  ylab("Frequency") +
  scale_y_continuous(limits = c(0,0.2))

predict.inla2 <- function(species, modelN, temp, max) {
  my_tibble2 <- my_tibble %>% 
    filter(species == species,
           model == modelN) %>% 
    select(species,
           model,
           offset = year,
           intercept,
           year_off_t = year_offset,
           infoff,
           temp_min_scale,
           year_offset_infoff,
           year_offset_temp_min_scale,
           infoff_temp_min_scale,
           year_offset_infoff_temp_min_scale) 
  
  pred_tab2 <- create_pred_off(2)
  pred_tab3 <- create_pred_off(3)
  pred_tab4 <- create_pred_off(4)
  pred_tab5 <- create_pred_off(5)
  pred_tab6 <- create_pred_off(6)
  pred_tab7 <- create_pred_off(7)
  pred_tab8 <- create_pred_off(8)
  pred_tab9 <- create_pred_off(9)
  pred_tab10 <- create_pred_off(10)
  pred_tab11 <- create_pred_off(11)
  pred_tab12 <- create_pred_off(12)
  pred_tab13 <- create_pred_off(13)
  pred_tab14 <- create_pred_off(14)
  pred_tab15 <- create_pred_off(15)
  pred_tab16 <- create_pred_off(16)
  
  pred_tab_name <- glue("pred_tab{year_}")
  
  plot.pred(off = year_, pars_tib = my_tibble2, pred_tabX = get(pred_tab_name), temp = temp, max = max)
  
}

plot.pred <- function(off, pars_tib, pred_tabX, temp, max){
  pars_tib <- pars_tib %>% 
    filter(offset == off)
  if(nrow(pars_tib) != 1) {stop("oooppsssss error row 19 extracting model pars")}
  
  # mean -----------
  ifelse(!is.na(pars_tib$intercept[[1]][1]), b0 <- pars_tib$intercept[[1]][1], b0 <- 0)
  ifelse(!is.na(pars_tib$year_off_t[[1]][1]), b1 <- pars_tib$year_off_t[[1]][1], b1 <- 0)
  ifelse(!is.na(pars_tib$infoff[[1]][1]), b2 <- pars_tib$infoff[[1]][1], b2 <- 0)
  ifelse(!is.na(pars_tib$temp_min_scale[[1]][1]), b3 <- pars_tib$temp_min_scale[[1]][1], b3 <- 0)
  ifelse(!is.na(pars_tib$year_offset_infoff[[1]][1]), b4 <- pars_tib$year_offset_infoff[[1]][1], b4 <- 0)
  ifelse(!is.na(pars_tib$year_offset_temp_min_scale[[1]][1]), b5 <- pars_tib$year_offset_temp_min_scale[[1]][1], b5 <- 0)
  ifelse(!is.na(pars_tib$infoff_temp_min_scale[[1]][1]), b6 <- pars_tib$infoff_temp_min_scale[[1]][1], b6 <- 0)
  ifelse(!is.na(pars_tib$year_offset_infoff_temp_min_scale[[1]][1]), b7 <- pars_tib$  year_offset_infoff_temp_min_scale[[1]][1], b7 <- 0)
    
  pred_tabX <- pred_tabX %>% 
    mutate(temp_t = temp)
  
  no_infes <- pred_tabX %>% 
    mutate(prediction = exp(
      b0 + (b1 * year) + (b3 * temp_t) +
        (b5 * year * temp_t)),
      HWA = 'infest'
    )
  
  infes <- pred_tabX %>% 
    mutate(prediction = exp(
      b0 + (b1 * year) + (b2 * infoff_t) + (b3 * temp_t) +
        (b4 * year * infoff_t) + (b5 * year * temp_t) +
        (b6 * infoff_t * temp_t) + (b7 * year * infoff_t * temp_t)),
      HWA = 'no_infest'
    )
  
  # up 
  b0u <- pars_tib$intercept[[1]][3]
  b1u <- pars_tib$year_off_t[[1]][3]
  b2u <- pars_tib$infoff[[1]][3]
  b3u <- pars_tib$temp_min_scale[[1]][3]
  b4u <- pars_tib$year_offset_infoff[[1]][3]
  b5u <- pars_tib$year_offset_temp_min_scale[[1]][3]
  b6u <- pars_tib$infoff_temp_min_scale[[1]][3]
  b7u <- pars_tib$year_offset_infoff_temp_min_scale[[1]][3]
  
  no_infesu <- no_infes %>% 
    mutate(predictionU = exp(
      b0u + (b1u * year) + (b3u * temp_t) +
        (b5u * year * temp_t)),
      HWA = 'infest'
    )
  
  infesu <- infes %>% 
    mutate(predictionU = exp(
      b0u + (b1u * year) + (b2u * infoff_t) + (b3u * temp_t) +
        (b4u * year * infoff_t) + (b5u * year * temp_t) +
        (b6u * infoff_t * temp_t) + (b7u * year * infoff_t * temp_t)),
      HWA = 'no_infest'
    )
  # low -----------------
  b0l <- pars_tib$intercept[[1]][2]
  b1l <- pars_tib$year_off_t[[1]][2]
  b2l <- pars_tib$infoff[[1]][2]
  b3l <- pars_tib$temp_min_scale[[1]][2]
  b4l <- pars_tib$year_offset_infoff[[1]][2]
  b5l <- pars_tib$year_offset_temp_min_scale[[1]][2]
  b6l <- pars_tib$infoff_temp_min_scale[[1]][2]
  b7l <- pars_tib$year_offset_infoff_temp_min_scale[[1]][2]
  
  no_infesl <- no_infes %>% 
    mutate(predictionL = exp(
      b0l + (b1l * year) + (b3l * temp_t) +
        (b5l * year * temp_t)),
      HWA = 'infest'
    )
  
  infesl <- infes %>% 
    mutate(predictionL = exp(
      b0l + (b1l * year) + (b2l * infoff_t) + (b3l * temp_t) +
        (b4l * year * infoff_t) + (b5l * year * temp_t) +
        (b6l * infoff_t * temp_t) + (b7l * year * infoff_t * temp_t)),
      HWA = 'no_infest'
    )
  
  # save coefs rds
  pars_tib2 <- rep(NA, 8)
  pars_tib2[1] <- b0
  pars_tib2[2] <- b1
  pars_tib2[3] <- b2
  pars_tib2[4] <- b3
  pars_tib2[5] <- b4
  pars_tib2[6] <- b5
  pars_tib2[7] <- b6
  pars_tib2[8] <- b7
  
  pars_tib2 <- as_tibble(pars_tib2)
  pars_tib2$par <- colnames(pars_tib)[4:11]
  pars_tib2 <- pars_tib2 %>% 
    relocate(par) %>% 
    mutate(low = 0,
           up = 0)
  
  pars_tib2[1,3] <- b0l
  pars_tib2[2,3] <- b1l
  pars_tib2[3,3] <- b2l
  pars_tib2[4,3] <- b3l
  pars_tib2[5,3] <- b4l
  pars_tib2[6,3] <- b5l
  pars_tib2[7,3] <- b6l
  pars_tib2[8,3] <- b7l
  
  pars_tib2[1,4] <- b0u
  pars_tib2[2,4] <- b1u
  pars_tib2[3,4] <- b2u
  pars_tib2[4,4] <- b3u
  pars_tib2[5,4] <- b4u
  pars_tib2[6,4] <- b5u
  pars_tib2[7,4] <- b6u
  pars_tib2[8,4] <- b7u
  
  write_rds(pars_tib2, file = glue("data/models_res/{species}/coefs_{species}.rds"))
  
  # ---------
  plot_preds <- rbind(no_infes, infes)
  
  off_gap <- infes %>% 
    filter(year %in% c(off, off+1))
  
  plot_preds <- plot_preds %>% 
    filter(!(HWA == 'no_infest' & year_off_t < 0)) %>% 
    arrange(desc(HWA)) 
  
  #write_csv(plot_preds, file = glue("data/{spsr}_{temp}preds.csv"))
  
  pp <- ggplot(aes(x = year, y = prediction, col = HWA), data = plot_preds) +
          geom_line(size = 0.8) + 
          #geom_line(aes(x = year, y = prediction), data = off_gap,
          #          col = 'white', size=2, alpha=1) +
          geom_vline(xintercept = 0, size=0.8, color = "gray43") +
          geom_vline(xintercept = off, linetype="dotted", color = "gray43", size=0.8) +
          geom_point(size = 1.5) + 
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                #axis.text = element_text(size= 16),
                #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                #axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                #legend.title = element_text(size = 18),
                #legend.text = element_text(size = 16),
                legend.position = "none",
                #axis.title = element_text(size = 16)
                axis.title.x = element_blank(),
                axis.title.y = element_blank()) + 
          #xlab("Years since HWA infestation") +
          #ylab("Bird Abundance") +
          scale_x_continuous(breaks = c(-10,-5,0,5,10,15,20)) +
          ylim(0, max) +
          scale_colour_manual("legend",
                            values = c("no_infest" = "gray82", "infest" = "gray28"),
                            labels = c("Not infested", "Infested"))
          # confidence interval
          #geom_line(aes(x = year, y = predictionL, col = HWA), data = plot_preds) +
          #geom_line(aes(x = year, y = predictionU, col = HWA), data = plot_preds)
  return(list(plot_preds = plot_preds,
              plott = pp))
}


maxi <- 4

a <- predict.inla2(spsr, mod_, t1, maxi)[2]
b <- predict.inla2(spsr, mod_, t2, maxi)[2]
c <- predict.inla2(spsr, mod_, t3, maxi)[2]

a <- a$plott
b <- b$plott
c <- c$plott

grid.arrange(a, b, c, ncol = 3)
print(species)

unique(BIRDtab$min_tempMe)/100
unique(BIRDtab$sd_tempMi)/100

quantile(BIRDx2INF$temp_min_scale, c(0.2, 0.5, 0.8))

pred_t1 <- predict.inla2(spsr, mod_, t1, maxi)[1]
pred_t1 <- pred_t1$plot_preds
pred_t2 <- predict.inla2(spsr, mod_, t2, maxi)[1]
pred_t2 <- pred_t2$plot_preds
pred_t3 <- predict.inla2(spsr, mod_, t3, maxi)[1]
pred_t3 <- pred_t3$plot_preds

pred_t1$temp <- "t1"
pred_t2$temp <- "t2"
pred_t3$temp <- "t3"

pred <- rbind(pred_t1, pred_t2, pred_t3)

write_rds(pred, file = glue("data/preds/preds_{species}.rds"))
