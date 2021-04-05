library(tidyverse)
library(gridExtra)

# Make predictions with the fixed values ---------------------
my_tibble <- summary_results2
pred_tab <- as_tibble(seq(-10,20,1)) %>% 
  rename(year = value)

create_pred_off <- function(offset_v){
  pred_tabX <- pred_tab %>% 
    mutate(infoff_t =  ifelse(year <= offset_v, 0, 1),
           year_off_t = year - offset_v)
  return(pred_tabX)
}

# off = 5; pars_tib = my_tibble2; pred_tabX = pred_tab2; temp = temp
plot.pred <- function(off, pars_tib, pred_tabX, temp){
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
  
  ggplot(aes(x = year, y = prediction, col = HWA), data = plot_preds) +
    geom_vline(xintercept = 0, size=0.5, color = "darkgray") +
    geom_vline(xintercept = off, linetype="dotted", color = "black", size=0.5) +
    geom_line() +
    geom_line(aes(x = year, y = prediction), data = off_gap,
              col = 'white', size=2, alpha=1) + 
    geom_point(size = 1) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.position = "none") + 
    xlab("Years since HWA infestation") +
    ylab("Bird Abundance") +
    scale_x_continuous(breaks = c(-10,-5,0,5,10,15,20))
    # confidence interval
    #geom_line(aes(x = year, y = predictionL, col = HWA), data = plot_preds) +
    #geom_line(aes(x = year, y = predictionU, col = HWA), data = plot_preds)
  
}
#   modelN <- temp <- 2
predict.inla <- function(species, modelN, temp) {
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
  
  p2 <- plot.pred(off = 2, pars_tib = my_tibble2, pred_tabX = pred_tab2, temp = temp)
  p3 <- plot.pred(off = 3, pars_tib = my_tibble2, pred_tabX = pred_tab3, temp = temp)
  p4 <- plot.pred(off = 4, pars_tib = my_tibble2, pred_tabX = pred_tab4, temp = temp)
  p5 <- plot.pred(off = 5, pars_tib = my_tibble2, pred_tabX = pred_tab5, temp = temp)
  p6 <- plot.pred(off = 6, pars_tib = my_tibble2, pred_tabX = pred_tab6, temp = temp)
  p7 <- plot.pred(off = 7, pars_tib = my_tibble2, pred_tabX = pred_tab7, temp = temp)
  p8 <- plot.pred(off = 8, pars_tib = my_tibble2, pred_tabX = pred_tab8, temp = temp)
  p9 <- plot.pred(off = 9, pars_tib = my_tibble2, pred_tabX = pred_tab9, temp = temp)
  p10 <- plot.pred(off = 10, pars_tib = my_tibble2, pred_tabX = pred_tab10, temp = temp)
  p11 <- plot.pred(off = 11, pars_tib = my_tibble2, pred_tabX = pred_tab11, temp = temp)
  p12 <- plot.pred(off = 12, pars_tib = my_tibble2, pred_tabX = pred_tab12, temp = temp)
  p13 <- plot.pred(off = 13, pars_tib = my_tibble2, pred_tabX = pred_tab13, temp = temp)
  p14 <- plot.pred(off = 14, pars_tib = my_tibble2, pred_tabX = pred_tab14, temp = temp)
  p15 <- plot.pred(off = 15, pars_tib = my_tibble2, pred_tabX = pred_tab15, temp = temp)
  p16 <- plot.pred(off = 16, pars_tib = my_tibble2, pred_tabX = pred_tab16, temp = temp)
  
  grid.arrange(p2, p3, p4, p5, 
               p6, p7, p8, p9, 
               p10, p11, p12, p13, 
               p14, p15, p16,
               nrow = 3)
}

predict.inla(spsr, 1, 2)
predict.inla(spsr, 2, 2)
predict.inla(spsr, 3, 2)

predict.inla(spsr, 1, 0)
predict.inla(spsr, 2, 0)
predict.inla(spsr, 3, 0)

predict.inla(spsr, 1, -2)
predict.inla(spsr, 2, -2)
predict.inla(spsr, 3, -2)

