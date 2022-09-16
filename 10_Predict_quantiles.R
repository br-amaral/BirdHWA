# Predict quantiles
# Input: 
#   summary_results file for each species

#   data/src/sps_list.csv: list of evaluated species
#   data/models_resnew/{species}/summary_results2.rds: tibble with species coefficient estimates for best model and offset
#   data/species/{species}.rds: species dataset used to run the models

# Output: 
#   data/models_resnew/{spsr}/{spsr}_{temp_n}preds.csv"): matrix with species predictions for -10 to 20 years in different temperatures
#   data/{spsr}_coefs.csv: table with species coefficient estimates
#   Figures/FigS2/{species}_tempquant.svg: plot of the distribution of temperature within species range
#   Figures/FigS1/{species}_preds.svg: plot of species predictions in different temperature quantiles

# Load packages --------------------------
library(tidyverse)
library(gridExtra)
library(glue)
library(fs)

SPECIES_DATA_PATH <- path("data/src/sps_list.csv")
WAIC_PATH <- path("data/waicbest.rds")

(sps_list <- read_csv(SPECIES_DATA_PATH))
waic_best3 <- read_rds(WAIC_PATH)

# upper limit of the prediction plot of each species
limits <- c(2, 1.6, 2, 3, 0.7, 0.5, 1, 6,
            4, 0.4, 21.1, 8.5, 16, 35)

# functions ---------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

predict.inla2 <- function(species, modelN, temp, max, temp_n) {
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
           year_offset_infoff_temp_min_scale,
           NewObserver) 
  
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
  
  plot.pred(off = year_, pars_tib = my_tibble2, pred_tabX = get(pred_tab_name), temp = temp, max = max, temp_n = temp_n)
  
}

plot.pred <- function(off, pars_tib, pred_tabX, temp, max, temp_n){
  pars_tib <- pars_tib %>% 
    filter(offset == off)
  if(nrow(pars_tib) != 1) {stop("oooppsssss error row 59 extracting model pars")}
  
  # mean 
  ifelse(!is.na(pars_tib$intercept[[1]][1]), 
         b0 <- pars_tib$intercept[[1]][1], b0 <- 0)
  ifelse(!is.na(pars_tib$year_off_t[[1]][1]),
         b1 <- pars_tib$year_off_t[[1]][1], b1 <- 0)
  ifelse(!is.na(pars_tib$infoff[[1]][1]), 
         b2 <- pars_tib$infoff[[1]][1], b2 <- 0)
  ifelse(!is.na(pars_tib$temp_min_scale[[1]][1]), 
         b3 <- pars_tib$temp_min_scale[[1]][1], b3 <- 0)
  ifelse(!is.na(pars_tib$year_offset_infoff[[1]][1]), 
         b4 <- pars_tib$year_offset_infoff[[1]][1], b4 <- 0)
  ifelse(!is.na(pars_tib$year_offset_temp_min_scale[[1]][1]), 
         b5 <- pars_tib$year_offset_temp_min_scale[[1]][1], b5 <- 0)
  ifelse(!is.na(pars_tib$infoff_temp_min_scale[[1]][1]), 
         b6 <- pars_tib$infoff_temp_min_scale[[1]][1], b6 <- 0)
  ifelse(!is.na(pars_tib$year_offset_infoff_temp_min_scale[[1]][1]), 
         b7 <- pars_tib$year_offset_infoff_temp_min_scale[[1]][1], b7 <- 0)
  ifelse(!is.na(pars_tib$NewObserver[[1]][1]), 
         b8 <- pars_tib$NewObserver[[1]][1], b8 <- 0)
  
  pred_tabX <- pred_tabX %>% 
    mutate(temp_t = temp)
  
  no_infes <- pred_tabX %>% 
    mutate(prediction = exp(
      b0 + (b1 * year_off_t) + (b3 * temp_t) +
        (b5 * year_off_t * temp_t)),
      HWA = 'infest'
    )
  
  infes <- pred_tabX %>% 
    mutate(prediction = exp(
      b0 + (b1 * year_off_t) + (b2 * infoff_t) + (b3 * temp_t) +
        (b4 * year_off_t * infoff_t) + (b5 * year_off_t * temp_t) +
        (b6 * infoff_t * temp_t) + (b7 * year_off_t * infoff_t * temp_t)),
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
  b8u <- pars_tib$NewObserver[[1]][3]
  
  # low 
  b0l <- pars_tib$intercept[[1]][2]
  b1l <- pars_tib$year_off_t[[1]][2]
  b2l <- pars_tib$infoff[[1]][2]
  b3l <- pars_tib$temp_min_scale[[1]][2]
  b4l <- pars_tib$year_offset_infoff[[1]][2]
  b5l <- pars_tib$year_offset_temp_min_scale[[1]][2]
  b6l <- pars_tib$infoff_temp_min_scale[[1]][2]
  b7l <- pars_tib$year_offset_infoff_temp_min_scale[[1]][2]
  b8l <- pars_tib$NewObserver[[1]][2]
  
  plot_preds <- rbind(no_infes, infes)
  
  off_gap <- infes %>% 
    filter(year %in% c(off, off+1))
  
  plot_preds <- plot_preds %>% 
    filter(!(HWA == 'no_infest' & year_off_t < 0)) %>% 
    arrange(desc(HWA)) 
  write_csv(plot_preds, file = glue("data/models_resnew/{spsr}/{spsr}_{temp_n}preds.csv"))
  
  coefs <- as_tibble(cbind(rbind('b0', 'b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8'),
                           rbind('intercept','year_off_t', 'infoff','temp_min_scale',
                                 'year_offset_infoff','year_offset_temp_min_scale','infoff_temp_min_scale',
                                 'year_offset_infoff_temp_min_scale','NewObserver'),
                           rbind(b0, b1, b2, b3, b4, b5, b6, b7, b8),
                           rbind(b0l, b1l, b2l, b3l, b4l, b5l, b6l, b7l, b8l),
                           rbind(b0u, b1u, b2u, b3u, b4u, b5u, b6u, b7u, b8u)))
  colnames(coefs) <- c("betas","coef_name","mean","low","up")
  coefs$mean <- as.numeric(coefs$mean)
  coefs$low <- as.numeric(coefs$low)
  coefs$up <- as.numeric(coefs$up)
  
  coefs <- rbind(coefs, 
                 c("mod_year", pull(species), as.numeric(mod_), as.numeric(year_), NA))
  
  write_csv(coefs, file = glue("data/models_resnew/{spsr}/coefs_{spsr}.csv"))
  
  if(as.numeric(mod_) %!in% c(3,6,8,10,11)) {
    plot_preds <- plot_preds %>% 
      filter(!(HWA == "no_infest" & year <= year_))
  }
  
  ggplot(aes(x = year, y = prediction, col = HWA), data = plot_preds) +
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
          axis.title.y = element_blank(),
          axis.text = element_text(size = 12)) + 
    #xlab("Years since HWA infestation") +
    #ylab("Bird Abundance") +
    scale_x_continuous(breaks = c(-10,-5,0,5,10,15,20)) +
    ylim(0, max) +
    scale_colour_manual("legend",
                        values = c("no_infest" = "darkorange", "infest" = "darkorchid"),
                        labels = c("Not infested", "Infested"))
  # confidence interval
  #geom_line(aes(x = year, y = predictionL, col = HWA), data = plot_preds) +
  #geom_line(aes(x = year, y = predictionU, col = HWA), data = plot_preds)
  
}

# loop in species ----------------------------------------
for (i in 1:nrow(sps_list)) {
  (species <- spsr <- sps_list[i,1])
  
  SUM_RES_PATH <- glue("data/models_resnew/{species}/summary_results2.rds")
  SPECIES_MOD_DAT <- glue("data/species/{species}.rds")
  
  # Make predictions with the fixed values and temperature quantiles
  summary_results2 <- my_tibble <- read_rds(SUM_RES_PATH)
  
  (waic_best <- waic_best3 %>% 
      filter(species == pull(spsr)))
  
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
  
  #ggplot(BIRDx, aes(x = year_offset, y = SpeciesTotal, colour = Infested)) +
    #geom_point() +
    #geom_smooth(aes(fill = Infested)) +
    #ggtitle("-20 e +20 filtro") +
    #theme_bw()
  
  #ggplot(BIRDx, aes(x = year_offset, y = SpeciesTotal)) +
    #geom_smooth() +
    #ggtitle(species) + xlim(-20,20) 
  
  x <- as.numeric(as.matrix(BIRDx[which(BIRDx$year_offset<10),1]))
  
  BIRDx2 <- BIRDx[which(as.numeric(BIRDx$RouteId) %in% x), ]
  
  BIRDx2INF <- BIRDx2[which(BIRDx2$Infested == T),]
  BIRDx2NO <- BIRDx2[which(BIRDx$Infested == F),]
  
  mean(BIRDx2INF$temp_min_scale, na.rm = T)
  mean(BIRDx2NO$temp_min_scale, na.rm = T)
  
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
  
  svg(glue("Figures/FigS2/{species}_tempquant.svg"), 
      width = 6.5, height = 5)
  
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
          axis.text = element_text(size= 28),
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
  
  dev.off() 
  
  maxi <- limits[i]
  
  a <- predict.inla2(spsr, mod_, t1, maxi, temp_n = "t1")
  b <- predict.inla2(spsr, mod_, t2, maxi, temp_n = "t2")
  c <- predict.inla2(spsr, mod_, t3, maxi, temp_n = "t3")
  grid.arrange(a, b, c, ncol = 3)
  
  svg(glue("Figures/FigS1/{species}_preds.svg"), 
      width = 13, height = 3)
  grid.arrange(a, b, c, ncol = 3)
  dev.off()
  print(species)
  
  unique(BIRDtab$min_tempMe)/100
  unique(BIRDtab$sd_tempMi)/100
  
  quantile(BIRDx2INF$temp_min_scale, c(0.2, 0.5, 0.8))

}

