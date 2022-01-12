library(ggplot2)
library(gridExtra)
library(INLA)
library(lme4)
library(tidyverse)
library(glue)

species <- "BHVI"
n_data <- read_rds(glue("data/species/{species}.rds")) 
## Create an year offset for that species ------------------  
offset <- 16
BIRD <- n_data %>% 
  # year_offset is standardizing yrhwa to the offset (years after infestation to the impact)
  mutate(year_offset = ifelse(YearInfested != 0, Year - YearInfested - offset, 0),
         # infoff: 'infested' route according to the delay in the effect (offset)
         infoff = ifelse(year_offset <= 0, 0, ifelse(year_offset > 0, 1, NA)))

rout_notinf <- BIRD %>% 
  select(RouteId, Year, YearInfested, Infested) %>% 
  filter(YearInfested == 0) %>% 
  distinct() %>% 
  group_by(RouteId) %>% 
  mutate(maxYear = max(Year)) %>% 
  select(RouteId, maxYear) %>% 
  distinct()

for(i in 1:nrow(BIRD)){
  if(BIRD$YearInfested[i] == 0){
    off_noin <- rout_notinf[which(rout_notinf$RouteId == BIRD$RouteId[i]), 2]
    BIRD$year_offset[i] <- BIRD$Year[i] - as.numeric(off_noin)
  }
}

BIRDxi <- BIRD %>% 
  filter(YearInfested != 0,
         year_offset > -20 & year_offset < 20) %>% 
  group_by(RouteId) %>% 
  mutate(max = max(year_offset)) %>% 
  filter(max > 9) %>% 
  ungroup() %>% 
  select(-max)

BIRDxb <- BIRD %>% 
  filter(year_offset > -20 & year_offset < 20) %>% 
  group_by(RouteId) %>% 
  filter(YearInfested == 0) 

BIRDxb <- rbind(BIRDxb, BIRDxi)

temps <- BIRDxi %>% 
  select(temp_min_scale, RouteId) %>% 
  distinct()
temps2 <- BIRDxb %>% 
  select(temp_min_scale, RouteId) %>% 
  distinct()

t1 <- quantile(temps$temp_min_scale, c(0.2, 0.5, 0.8))[1]
t2 <- temp_t <- quantile(temps$temp_min_scale, c(0.2, 0.5, 0.8))[2]
t3 <- quantile(temps$temp_min_scale, c(0.2, 0.5, 0.8))[3]

t1t <- quantile(temps2$temp_min_scale, c(0.2, 0.5, 0.8))[1]
t2t <- quantile(temps2$temp_min_scale, c(0.2, 0.5, 0.8))[2]
t3t <- quantile(temps2$temp_min_scale, c(0.2, 0.5, 0.8))[3]
#quantile(temps$temp_min_scale, c(0.2, 0.5, 0.8))

par(mfrow=c(1,2))
hist(temps$temp_min_scale)
abline(v=t1, lwd = 2)
abline(v=t2, lwd = 2)
abline(v=t3, lwd = 2)

hist(temps2$temp_min_scale)
abline(v=t1t, lwd = 2)
abline(v=t2t, lwd = 2)
abline(v=t3t, lwd = 2)

ggplot(temps2, aes(x = temp_min_scale#, fill = Infested
                  )) +
  geom_histogram(aes(y = stat(count)/length(temps$temp_min_scale)),
                 position = "identity", alpha = 1,
                 bins = 15) + 
  scale_fill_manual(values=c("grey60", "grey90")) +
  geom_histogram(data = temps, aes(y = stat(count)/length(temps$temp_min_scale)),
                 position = "identity",
                 bins = 15, fill = "gray", alpha = 1) +
  geom_vline(xintercept = t1, size=0.5, color = "black", linetype="dotted") +
  geom_vline(xintercept = t2, size=0.5, color = "black", linetype="dotted") +
  geom_vline(xintercept = t3, size=0.5, color = "black", linetype="dotted") +
  geom_vline(xintercept = t1t, size=0.5, color = "black") +
  geom_vline(xintercept = t2t, size=0.5, color = "black") +
  geom_vline(xintercept = t3t, size=0.5, color = "black") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size= 12),
        axis.title = element_text(size = 12),
        legend.position = "none",
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0))) + 
  xlab("Mean minimum temperature") +
  ylab("Frequency") +
  ggtitle(glue("{species} - darkgray and full line is total vs infest in lightgray and dashed"))

BIRDx <- BIRDxi

ggplot(BIRDx, aes(x = year_offset, y = SpeciesTotal)) +
#   geom_point() +
  geom_smooth() +
  ggtitle(species) + xlim(-20,20)


mod1 <- glmer(SpeciesTotal ~ year_offset * infoff * temp_min_scale + NewObserver + (1 | ObserverRoute) , data = BIRDx, family = poisson(link = "log"))

mod2 <- glmer(SpeciesTotal ~ year_offset * infoff * temp_min_scale + NewObserver + (1 | ObserverId), data = BIRDx, family = poisson(link = "log"))

mod3 <- glmer(SpeciesTotal ~ year_offset * infoff * temp_min_scale + NewObserver + (1 | ObserverRoute) + (1 | Year), data = BIRDx, family = poisson(link = "log"))

mod4 <- glmer(SpeciesTotal ~ year_offset * infoff * temp_min_scale + NewObserver + (1 | ObserverId) + (1 | Year), data = BIRDx, family = poisson(link = "log"))

mod5 <- glmer(SpeciesTotal ~ year_offset * infoff + NewObserver + (1 | ObserverRoute) , data = BIRDx, family = poisson(link = "log"))

mod6 <- glmer(SpeciesTotal ~ year_offset * infoff + NewObserver + (1 | ObserverId), data = BIRDx, family = poisson(link = "log"))

mod7 <- glm(SpeciesTotal ~ year_offset * infoff * temp_min_scale + NewObserver + ObserverRoute , data = BIRDx, family = poisson(link = "log"))

mod <- row.names(BIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7)) ; cbind(mod,BIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7)) %>% arrange(BIC)

t2
t2t
# summary(mod1)
# summary(mod2)
# summary(mod3)
# summary(mod4)
# summary(mod5)
# summary(mod6)

## run model removing a route at a time
#sensitivity <- function(BIRD){
  BIRD <- BIRDx
  routes <- BIRD %>% select(RouteId) %>% distinct() %>% arrange()
  res_tib1 <- as.list(matrix(NA, nrow = nrow(routes)))
  res_tib2 <- as.list(matrix(NA, nrow = nrow(routes)))
  
  #colnames(res_tib) <- c("RouteId", "summary","coef")
  #res_tib$RouteId <- routes
  
  for(i in 1:nrow(routes)){
    BIRD_sen <- BIRD[which(BIRD$RouteId != as.character(routes[i,1])),]
  
    res <- summary(glmer(SpeciesTotal ~ year_offset * infoff * temp_min_scale + NewObserver + (1 | ObserverRoute) , data = BIRD_sen, family = poisson(link = "log")))
    
    res_tib1[[i]] <- res
    res_tib2[[i]] <- res$coefficients
    print(i)
    rm(res)
  
  }
  
  #rownames(res_tib2[[1]])
  year_offset <- as.data.frame(matrix(NA, nrow = nrow(routes), ncol = 3)) %>% 
    rename(RouteId = V1, estimate = V2, sd = V3)
  year_offset$RouteId <- as.numeric(as.vector(as.matrix(routes)))
  
  intercept <- infoff <- temp_min_scale <- NewObserverTRUE <- year_offset_infoff <- year_offset_temp_min_scale <- 
    infoff_temp_min_scale <- year_offset_infoff_temp_min_scale <- year_offset
  
  for(i in 1:nrow(routes)){
    intercept[i,2:3] <- t(as_tibble(as.numeric(res_tib2[[i]][1,c(1:2)])))
    year_offset[i,2:3] <- t(as_tibble(as.numeric(res_tib2[[i]][2,c(1:2)])))
    infoff[i,2:3] <- t(as_tibble(as.numeric(res_tib2[[i]][3,c(1:2)])))
    temp_min_scale[i,2:3] <- t(as_tibble(as.numeric(res_tib2[[i]][4,c(1:2)])))
    NewObserverTRUE[i,2:3] <- t(as_tibble(as.numeric(res_tib2[[i]][5,c(1:2)])))
    year_offset_infoff[i,2:3] <- t(as_tibble(as.numeric(res_tib2[[i]][6,c(1:2)])))
    year_offset_temp_min_scale[i,2:3] <- t(as_tibble(as.numeric(res_tib2[[i]][7,c(1:2)])))
    infoff_temp_min_scale[i,2:3] <- t(as_tibble(as.numeric(res_tib2[[i]][8,c(1:2)])))
    year_offset_infoff_temp_min_scale[i,2:3] <- t(as_tibble(as.numeric(res_tib2[[i]][9,c(1:2)])))
  }
  
  plot_sensi <- function(mat, title1){
    tit <- title1
    mat <- mat %>% 
      mutate(up = estimate + (sd * 1.96),
             low = estimate - (sd * 1.96))
    matmean <- mean(mat$estimate)
    matsd <- sd(mat$estimate)
    
    p1 <- ggplot(data = mat, aes(as.factor(RouteId), estimate)) +
      geom_hline(aes(yintercept = matmean, colour="red"),
                 show.legend = FALSE) +
      geom_hline(aes(yintercept = matmean - (matsd * 1.96), colour="blue"),
                 show.legend = FALSE) +
      geom_hline(aes(yintercept = matmean + (matsd * 1.96), colour="blue"),
                 show.legend = FALSE) +
      geom_point() +
      theme_bw() +
      labs(title = tit) +
      theme(plot.title = element_text(hjust = 0.5))
      
    p2 <- ggplot(data = mat, aes(as.factor(RouteId), estimate)) +
      geom_hline(aes(yintercept = matmean, colour="red"),
                       show.legend = FALSE) +
      geom_hline(aes(yintercept = matmean - (matsd * 1.96), colour="blue"),
                 show.legend = FALSE) +
      geom_hline(aes(yintercept = matmean + (matsd * 1.96), colour="blue"),
                 show.legend = FALSE) +
      geom_errorbar(aes(ymin= low, ymax= up),
                    colour = "grey65") +
      geom_point() +
      theme_bw() +
      labs(title = tit) +
      theme(plot.title = element_text(hjust = 0.5))
    
    grid.arrange(p1, p2, nrow = 1)
    
    print(mat[which(mat$estimate > ( matmean + (matsd * 1.96)) | mat$estimate < ( matmean - (matsd * 1.96))),1])
  
#    return(list = c(res_tib1, res_tib2))
    }
  
  plot_sensi(year_offset, "year_offset")
  plot_sensi(infoff, "infoff")
  plot_sensi(temp_min_scale, "temp_min_scale")
  plot_sensi(NewObserverTRUE, "NewObserverTRUE")
  plot_sensi(year_offset_infoff, "year_offset_infoff")
  plot_sensi(year_offset_temp_min_scale, "year_offset_temp_min_scale")
  plot_sensi(infoff_temp_min_scale, "infoff_temp_min_scale")
  plot_sensi(year_offset_infoff_temp_min_scale, "year_offset_infoff_temp_min_scale")
  
  mean1 <- mean(year_offset$estimate)
  sd1 <- sd(year_offset$estimate)
  year_offset <- year_offset %>% 
    mutate(leverage = ifelse(estimate > ( mean1 + (sd1 * 1.96)) | estimate < ( mean1 - (sd1 * 1.96)),
                         T, F))
  lev1 <- year_offset %>% 
    filter(leverage == T) %>% 
    select(RouteId) %>% 
    as_vector() %>% 
    as.numeric()
  
  mean2 <- mean(infoff$estimate)
  sd2 <- sd(infoff$estimate)
  infoff <- infoff %>% 
    mutate(leverage = ifelse(estimate > ( mean2 + (sd2 * 1.96)) | 
                               estimate < ( mean2 - (sd2 * 1.96)),
                             T, F))
  lev2 <- infoff %>% 
    filter(leverage == T) %>% 
    select(RouteId) %>% 
    as_vector() %>% 
    as.numeric()
  
  mean3 <- mean(temp_min_scale$estimate)
  sd3 <- sd(temp_min_scale$estimate)
  temp_min_scale <- temp_min_scale %>% 
    mutate(leverage = ifelse(estimate > ( mean3 + (sd3 * 1.96)) | 
                               estimate < ( mean3 - (sd3 * 1.96)),
                             T, F))
  lev3 <- temp_min_scale %>% 
    filter(leverage == T) %>% 
    select(RouteId) %>% 
    as_vector() %>% 
    as.numeric()
  
  mean4 <- mean(NewObserverTRUE$estimate)
  sd4 <- sd(NewObserverTRUE$estimate)
  NewObserverTRUE <- NewObserverTRUE %>% 
    mutate(leverage = ifelse(estimate > ( mean4 + (sd4 * 1.96)) | 
                               estimate < ( mean4 - (sd4 * 1.96)),
                             T, F))
  lev4 <- NewObserverTRUE %>% 
    filter(leverage == T) %>% 
    select(RouteId) %>% 
    as_vector() %>% 
    as.numeric()
  
  mean5 <- mean(year_offset_infoff$estimate)
  sd5 <- sd(year_offset_infoff$estimate)
  year_offset_infoff <- year_offset_infoff %>% 
    mutate(leverage = ifelse(estimate > ( mean5 + (sd5 * 1.96)) | 
                               estimate < ( mean5 - (sd5 * 1.96)),
                             T, F))
  lev5 <- year_offset_infoff %>% 
    filter(leverage == T) %>% 
    select(RouteId) %>% 
    as_vector() %>% 
    as.numeric()
  
  mean6 <- mean(year_offset_temp_min_scale$estimate)
  sd6 <- sd(year_offset_temp_min_scale$estimate)
  year_offset_temp_min_scale <- year_offset_temp_min_scale %>% 
    mutate(leverage = ifelse(estimate > ( mean6 + (sd6 * 1.96)) | 
                               estimate < ( mean6 - (sd6 * 1.96)),
                             T, F))
  lev6 <- year_offset_temp_min_scale %>% 
    filter(leverage == T) %>% 
    select(RouteId) %>% 
    as_vector() %>% 
    as.numeric()
  
  mean7 <- mean(infoff_temp_min_scale$estimate)
  sd7 <- sd(infoff_temp_min_scale$estimate)
  infoff_temp_min_scale <- infoff_temp_min_scale %>% 
    mutate(leverage = ifelse(estimate > ( mean7 + (sd7 * 1.96)) | 
                               estimate < ( mean7 - (sd7 * 1.96)),
                             T, F))
  lev7 <- infoff_temp_min_scale %>% 
    filter(leverage == T) %>% 
    select(RouteId) %>% 
    as_vector() %>% 
    as.numeric()
  
  mean8 <- mean(year_offset_infoff_temp_min_scale$estimate)
  sd8 <- sd(year_offset_infoff_temp_min_scale$estimate)
  year_offset_infoff_temp_min_scale <- year_offset_infoff_temp_min_scale %>% 
    mutate(leverage = ifelse(estimate > ( mean8 + (sd8 * 1.96)) | 
                               estimate < ( mean8 - (sd8 * 1.96)),
                             T, F))
  lev8 <- year_offset_infoff_temp_min_scale %>% 
    filter(leverage == T) %>% 
    select(RouteId) %>% 
    as_vector() %>% 
    as.numeric()
  
  levs <- c(lev1, lev2, lev3, lev4,
                lev5, lev6, lev7, lev8) %>% 
    unique() %>% 
    sort()

  # Frequency of routes that if are removed really change the predictions
  par(mfrow= c(1,1))
  table(c(lev1, lev2, lev3, lev4,lev5, lev6, lev7, lev8)) %>% 
    barplot(cex.names = 0.5,las=2)
  
  n <- max(length(lev1), length(lev2), length(lev3), length(lev4),
           length(lev5), length(lev6), length(lev7), length(lev8))
  length(lev1) <- length(lev2) <- length(lev3) <- length(lev4) <-
  length(lev5) <- length(lev6) <- length(lev7) <- length(lev8) <- n                      
 
  names_pars <- c("year_offset",
                  "infoff",
                  "temp_min_scale",
                  "NewObserverTRUE",
                  "year_offset_infoff",
                  "year_offset_temp_min_scale",
                  "infoff_temp_min_scale",
                  "year_offset_infoff_temp_min_scale")
  
  levs2 <- tibble(year_offset = lev1, infoff = lev2, temp_min_scale = lev3, NewObserverTRUE = lev4,
                  year_offset_infoff = lev5, year_offset_temp_min_scale = lev6, 
                  infoff_temp_min_scale = lev7, year_offset_infoff_temp_min_scale = lev8) %>% 
    pivot_longer(`year_offset`:`year_offset_infoff_temp_min_scale`, names_to = "par", values_to = "rou") %>% 
    arrange(rou) %>% 
    filter(!(is.na(rou)))
  levs2 <- levs2[order(match(levs2$par,  names_pars)),]
  a <- (as.factor(levs2$par))
  a <- factor(a, levels = names_pars)
  a <- as.integer(a)
  levs2$par_1 <- as.numeric(a)
  #}

# res_tib1 has the model summary and res_tib2 has the coeficients
## plot routes that are outliers
hist(levs2$rou, breaks = length(unique(levs2$rou)), freq = TRUE)
ggplot(levs2, aes(x = rou, y = jitter(par_1), color = rou)) + 
  geom_point(position = position_dodge(width = 0.4)) +
  scale_y_discrete(limit = seq(1,8,1),
                   labels = c("year_offset","infoff","
                              temp_min_scale","NewObserverTRUE","year_offset_infoff","
                              year_offset_temp_min_scale","infoff_temp_min_scale","year_offset_infoff_temp_min_scale"))

## predictions  ------------------------

plot.pred <- function(mod, max){
  # mean -----------
  b0 <- intercept[mod,2]
  b1 <- year_offset[mod,2]
  b2 <- infoff[mod,2]
  b3 <- temp_min_scale[mod,2]
  b4 <- NewObserverTRUE[mod,2]
  b5 <- year_offset_infoff[mod,2]
  b6 <- year_offset_temp_min_scale[mod,2]
  b7 <- infoff_temp_min_scale[mod,2]
  b8 <- year_offset_infoff_temp_min_scale[mod,2]
  
  pred_tab <- as_tibble(seq(-10,20,1)) %>% 
    rename(year = value) %>% 
    mutate(infoff_t =  ifelse(year <= 0, 0, 1),
           year_off_t = year - 0,
           temp_t = t2)
  
  no_infes <- pred_tab %>% 
    mutate(prediction = exp(
      b0 + 
        (b1 * year_off_t) + 
        (b3 * temp_t) + ##=
        (b6 * year_off_t * temp_t)),
      HWA = 'infest'
    )
  
  infes <- pred_tab %>% 
    mutate(prediction = exp(
      b0 + 
        (b1 * year_off_t) +
        (b2 * infoff_t) +
        (b3 * temp_t) + ##=
        (b5 * year_off_t * infoff_t) + 
        (b6 * year_off_t * temp_t) +
        (b7 * infoff_t * temp_t) +
        (b8 * year_off_t * infoff_t * temp_t)),
      HWA = 'no_infest'
    )
  
  # up 
  b0u <- intercept[mod, 2] + (intercept[mod, 3] * 1.96)
  b1u <- year_offset[mod,2] + (year_offset[mod, 3] * 1.96)
  b2u <- infoff[mod, 2] + (infoff[mod, 3] * 1.96)
  b3u <- temp_min_scale[mod, 2] + (temp_min_scale[mod, 3] * 1.96)
  b4u <- NewObserverTRUE[mod, 2] + (NewObserverTRUE[mod, 3] * 1.96)
  b5u <- year_offset_infoff[mod, 2] + (year_offset_infoff[mod, 3] * 1.96)
  b6u <- year_offset_temp_min_scale[mod, 2] + (year_offset_temp_min_scale[mod, 3] * 1.96)
  b7u <- infoff_temp_min_scale[mod, 2] + (infoff_temp_min_scale[mod, 3] * 1.96)
  b8u <- year_offset_infoff_temp_min_scale[mod, 2] + (year_offset_infoff_temp_min_scale[mod, 3] * 1.96)
  
  no_infesU <- no_infes %>% 
    mutate(predictionU = exp(
      b0u + (b1u * year_off_t) + (b3u * temp_t) +
        (b5u * year_off_t * temp_t)),
      HWA = 'infest'
    )
  
  infesU <- infes %>% 
    mutate(predictionU = exp(
      b0u + (b1u * year_off_t) + (b2u * infoff_t) + (b3u * temp_t) +
        (b4u * year_off_t * infoff_t) + (b5u * year_off_t * temp_t) +
        (b6u * infoff_t * temp_t) + (b7u * year_off_t * infoff_t * temp_t)),
      HWA = 'no_infest'
    )
  # low -----------------
  b0l <- intercept[mod, 2] - (intercept[mod, 3] * 1.96)
  b1l <- year_offset[mod,2] - (year_offset[mod, 3] * 1.96)
  b2l <- infoff[mod, 2] - (infoff[mod, 3] * 1.96)
  b3l <- temp_min_scale[mod, 2] - (temp_min_scale[mod, 3] * 1.96)
  b4l <- NewObserverTRUE[mod, 2] - (NewObserverTRUE[mod, 3] * 1.96)
  b5l <- year_offset_infoff[mod, 2] - (year_offset_infoff[mod, 3] * 1.96)
  b6l <- year_offset_temp_min_scale[mod, 2] - (year_offset_temp_min_scale[mod, 3] * 1.96)
  b7l <- infoff_temp_min_scale[mod, 2] - (infoff_temp_min_scale[mod, 3] * 1.96)
  b8l <- year_offset_infoff_temp_min_scale[mod, 2] - (year_offset_infoff_temp_min_scale[mod, 3] * 1.96)
  
  no_infesL <- pred_tab %>% 
    mutate(prediction = exp(
      b0l + 
        (b1l * year_off_t) + 
        (b3l * temp_t) + ##=
        (b6l * year_off_t * temp_t))
    )
  
  infesL <- pred_tab %>% 
    mutate(prediction = exp(
        b0l + 
        (b1l * year_off_t) +
        (b2l * infoff_t) +
        (b3l * temp_t) + ##=
        (b5l * year_off_t * infoff_t) + 
        (b6l * year_off_t * temp_t) +
        (b7l * infoff_t * temp_t) +
        (b8l * year_off_t * infoff_t * temp_t))
    )
  # ---------
  plot_preds <- rbind(no_infes, infes)
  
  plot_preds <- plot_preds %>% 
    filter(!(HWA == 'no_infest' & year_off_t < 0)) %>% 
    arrange(desc(HWA)) 
  
  ggplot(aes(x = year, y = prediction, col = HWA), data = plot_preds) +
    geom_vline(xintercept = 0, size=1, color = "darkgray") +
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
    ggtitle(mod) +
    scale_x_continuous(breaks = c(-10,-5,0,5,10,15,20)) #+
    #ylim(0, max)
  # confidence interval
  #geom_line(aes(x = year, y = predictionL, col = HWA), data = plot_preds) +
  #geom_line(aes(x = year, y = predictionU, col = HWA), data = plot_preds)
  
}

check <- cbind(routes, seq(1,nrow(routes),1)) 
colnames(check) <- c("RouteId","mod")
check <- check %>% 
  filter(RouteId %in% BIRDx$RouteId)

i <- 1

plot.pred(check$mod[i],0)
check$RouteId[i]
i <- i + 1

j <- 238
check$RouteId[j]

## same model as glmr
formula <- SpeciesTotal ~ 1 +  year_offset + infoff + year_offset : infoff + NewObserver + 
  temp_min_scale + temp_min_scale : year_offset + temp_min_scale : infoff + temp_min_scale : infoff : year_offset +
  f(ObserverRoute, model= "iid") 

model <- inla(formula, family= "poisson", data= BIRDx, 
              control.predictor= list(compute= TRUE), 
              control.compute= list(waic= TRUE, dic= TRUE, cpo= TRUE))
mod_inla <- summary(model)

## full model
formula1 <- SpeciesTotal ~ 1 +  year_offset + infoff + year_offset : infoff + NewObserver +
  temp_min_scale + temp_min_scale : year_offset + temp_min_scale : infoff + temp_min_scale : infoff : year_offset +
  f(ObserverRoute, model= "iid") + f(Year, model= "iid") + f(hexID, model= "bym", graph= hex.adj, constr= TRUE)   

hex.adj <- paste0(getwd(),"/data/hexmap.graph")

model1 <- inla(formula1, family= "poisson", data= BIRDx, 
              control.predictor= list(compute= TRUE), 
              control.compute= list(waic= TRUE, dic= TRUE, cpo= TRUE))
mod_inla1 <- summary(model1)

## full model MINUS OBS_ROUTE
formula2 <- SpeciesTotal ~ 1 +  year_offset + infoff + year_offset : infoff + NewObserver +
  temp_min_scale + temp_min_scale : year_offset + temp_min_scale : infoff + temp_min_scale : infoff : year_offset +
  f(Year, model= "iid") + f(hexID, model= "bym", graph= hex.adj, constr= TRUE)

model2 <- inla(formula2, family= "poisson", data= BIRDx, 
               control.predictor= list(compute= TRUE), 
               control.compute= list(waic= TRUE, dic= TRUE, cpo= TRUE))

mod_inla2 <- summary(model2)

## full model MINUS SPACE
formula3 <- SpeciesTotal ~ 1 +  year_offset + infoff + year_offset : infoff + NewObserver +
  temp_min_scale + temp_min_scale : year_offset + temp_min_scale : infoff + temp_min_scale : infoff : year_offset +
  f(ObserverRoute, model= "iid") + f(Year, model= "iid") 
  
model3 <- inla(formula3, family= "poisson", data= BIRDx, 
               control.predictor= list(compute= TRUE), 
               control.compute= list(waic= TRUE, dic= TRUE, cpo=  TRUE))

mod_inla3 <- summary(model3)

## full model MINUS SPACE and OBSROU fixed
formula4 <- SpeciesTotal ~ 1 +  year_offset + infoff + year_offset : infoff + NewObserver +
  temp_min_scale : year_offset + temp_min_scale : infoff + temp_min_scale : infoff : year_offset +
  ObserverRoute + f(Year, model= "iid") 

model4 <- inla(formula4, family= "poisson", data= BIRDx, 
               control.predictor= list(compute= TRUE), 
               control.compute= list(waic= TRUE, dic= TRUE, cpo= TRUE))

mod_inla4 <- summary(model4)

summary(mod1)
mod_inla
mod_inla1
mod_inla2
mod_inla3
head(mod_inla4$fixed)
tail(mod_inla4$fixed)

write_rds(mod_inla, file= "mod_inla_inf.rds")
write_rds(mod_inla1, file= "mod_inla1_inf.rds")
write_rds(mod_inla2, file= "mod_inla2_inf.rds")
write_rds(mod_inla3, file= "mod_inla3_inf.rds")
write_rds(mod_inla4, file= "mod_inla4_inf.rds")
write_rds(mod1, file= "mod1_inf.rds")
write_rds(res_tib2, file= "res_tib2_inf.rds")
write_rds(res_tib1, file= "res_tib1_inf.rds")



write_rds(mod_inla, file="mod_inla_all.rds")
write_rds(mod_inla1, file="mod_inla1_all.rds")
write_rds(mod_inla2, file="mod_inla2_all.rds")
write_rds(mod_inla3, file="mod_inla3_all.rds")
write_rds(mod_inla4, file="mod_inla4_all.rds")
write_rds(mod1, file="mod1_all.rds")
write_rds(res_tib2, file="res_tib2_all.rds")
write_rds(res_tib1, file="res_tib1_all.rds")

year_offset
infoff
temp_min_scale
NewObserverTRUE
year_offset_infoff
year_offset_temp_min_scale
infoff_temp_min_scale
year_offset_infoff_temp_min_scale

BIRDx %>% group_by(RouteId) %>% mutate(RouteId = as.factor(RouteId)) %>% 
       summarise(sum = sum(SpeciesTotal)) %>% plot()

a <- BIRDx %>% 
  filter(RouteId %in% c(49001, 49002, 49023, 49044, 61074, 61108, 87023, 
                        91030, 46051, 47900, 61121, 63909, 72057, 88018)) %>% 
  #group_by(RouteId) %>% 
  mutate(RouteId = as.factor(RouteId)) %>% 
  select(RouteId, SpeciesTotal)
  #summarise(sum = sum(SpeciesTotal)) %>% 
qplot(a$RouteId, a$SpeciesTotal)





