species <- spsr #"HETH"

SUM_NAME <- glue("data/models_res/{species}/{species}_bestmodres.rds")
SPECIES_MOD_DAT <- glue("data/species/{species}.rds")
BEST_MOD <- glue("data/models_res/{species}/{species}_bestmod.rds")
BEST_OFF <- glue("data/models_res/{species}/{species}_bestoff.rds")

BIRDx <- readRDS(SPECIES_MOD_DAT)
my_tibble <- readRDS(SUM_NAME)
year_ <- off <- readRDS(BEST_OFF)
mod_ <- readRDS(BEST_MOD)

# Make predictions with the fixed values and temperature quantiles
pred_tab <- as_tibble(seq(-10,20,1)) %>% 
  rename(year = value)

create_pred_off <- function(offset_v){
  pred_tabX <- pred_tab %>% 
    mutate(infoff_t =  ifelse(year <= offset_v, 0, 1),
           year_off_t = year - offset_v)
  return(pred_tabX)
}

## Create an year offset for that species ------------------  
BIRDx <- BIRDx %>% 
  # year_offset is standardizing yrhwa to the offset (years after infestation to the impact)
  mutate(year_offset = ifelse(YearInfested != 0, Year - YearInfested - off, 0),
         # infoff: 'infested' route according to the delay in the effect (offset)
         infoff = ifelse(year_offset <= 0, 0, ifelse(year_offset > 0, 1, NA)))

rout_notinf <- BIRDx %>% 
  select(RouteId, Year, YearInfested, Infested) %>% 
  filter(YearInfested == 0) %>% 
  distinct() %>% 
  group_by(RouteId) %>% 
  mutate(maxYear = max(Year)) %>% 
  select(RouteId, maxYear) %>% 
  distinct()

## if a route was never infested, year_offset is 'equal' to the last year it was sampled
for(i in nrow(BIRDx)){
  if(BIRDx$YearInfested[i] == 0){
    off_noin <- rout_notinf[which(rout_notinf$RouteId == BIRDx$RouteId[i]), 2]
    BIRDx$year_offset[i] <- BIRD$Year[i] - as.numeric(off_noin)
  }
}
## only infested routes
BIRDx <- BIRDx %>% 
  filter(YearInfested != 0,
         year_offset > -20 & year_offset < 20) %>% 
  group_by(RouteId) %>% 
  mutate(max = max(year_offset)) %>% 
  filter(max > 9) %>% 
  ungroup()

t1 <- quantile(BIRDx$temp_min_scale, c(0.2, 0.5, 0.8))[1]
t2 <- quantile(BIRDx$temp_min_scale, c(0.2, 0.5, 0.8))[2]
t3 <- quantile(BIRDx$temp_min_scale, c(0.2, 0.5, 0.8))[3]

temps <- BIRDx %>% 
  select(temp_min_scale, RouteId) %>% 
  distinct()

ggplot(temps, aes(x = temp_min_scale)) +
  geom_histogram(aes(y = stat(count)/length(temps$temp_min_scale)),
                 position = "identity", alpha = .7,
                 bins = 15) + 
  geom_vline(xintercept = t1, size=0.5, color = "black") +
  geom_vline(xintercept = t2, size=0.5, color = "black") +
  geom_vline(xintercept = t3, size=0.5, color = "black") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size= 12),
        axis.title = element_text(size = 12),
        legend.position = "none",
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0))) + 
  xlab("Mean minimum temperature") +
  ylab("Frequency") 

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
  ifelse(!is.na(pars_tib$year_offset_infoff_temp_min_scale[[1]][1]), b7 <- pars_tib$year_offset_infoff_temp_min_scale[[1]][1], b7 <- 0)
  
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
  
  no_infes <- no_infes %>% 
    mutate(predictionU = exp(
      b0u + (b1u * year_off_t) + (b3u * temp_t) +
        (b5u * year_off_t * temp_t)),
      HWA = 'infest'
    )
  
  infes <- infes %>% 
    mutate(predictionU = exp(
      b0u + (b1u * year_off_t) + (b2u * infoff_t) + (b3u * temp_t) +
        (b4u * year_off_t * infoff_t) + (b5u * year_off_t * temp_t) +
        (b6u * infoff_t * temp_t) + (b7u * year_off_t * infoff_t * temp_t)),
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
  
  no_infes <- no_infes %>% 
    mutate(predictionL = exp(
      b0l + (b1l * year_off_t) + (b3l * temp_t) +
        (b5l * year_off_t * temp_t)),
      HWA = 'infest'
    )
  
  infes <- infes %>% 
    mutate(predictionL = exp(
      b0l + (b1l * year_off_t) + (b2l * infoff_t) + (b3l * temp_t) +
        (b4l * year_off_t * infoff_t) + (b5l * year_off_t * temp_t) +
        (b6l * infoff_t * temp_t) + (b7l * year_off_t * infoff_t * temp_t)),
      HWA = 'no_infest'
    )
  # ---------
  plot_preds <- rbind(no_infes, infes)
  
  off_gap <- infes %>% 
    filter(year %in% c(off, off+1))
  
  plot_preds <- plot_preds %>% 
    filter(!(HWA == 'no_infest' & year_off_t < 0)) %>% 
    arrange(desc(HWA)) 
    
  ggplot(aes(x = year, y = prediction, col = HWA), data = plot_preds) +
    geom_vline(xintercept = 0, size=1, color = "darkgray") +
    geom_vline(xintercept = off, linetype="dotted", color = "black", size=1) +
    geom_line() +
    #geom_line(aes(x = year, y = prediction), data = off_gap,
    #          col = 'white', size=2, alpha=1) + 
    geom_point(size = 2) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size= 16),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16),
          legend.position = "none",
          axis.title = element_text(size = 16)) + 
    xlab("Years since HWA infestation") +
    ylab("Bird Abundance") +
    scale_x_continuous(breaks = c(-10,-5,0,5,10,15,20)) +
    ylim(0, max)
  # confidence interval
  #geom_line(aes(x = year, y = predictionL, col = HWA), data = plot_preds) +
  #geom_line(aes(x = year, y = predictionU, col = HWA), data = plot_preds)
  
}

maxi <- 0.4

predict.inla2(spsr, mod_, t1, maxi)
predict.inla2(spsr, mod_, t2, maxi)
predict.inla2(spsr, mod_, t3, maxi)

unique(BIRDtab$min_tempMe)/100
unique(BIRDtab$sd_tempMi)/100

quantile(BIRDx$temp_min_scale, c(0.2, 0.5, 0.8))



