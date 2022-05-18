library(ggplot2)
library(tidyverse)
library(glue)

SPECIES_DATA_PATH <- "data/src/sps_list.csv"
sps_list <- read_csv(SPECIES_DATA_PATH)
order <- rev(rep(c("ACFL", "BHVI", "BLBW", "BTNW", "HETH", "MAWA", "RBNU",
                   "BLJA", "CERW", "EAPH", "REVI", "SCTA", "WBNU", "WOTH"), each = 3))

yrmod <- read_csv(file = "data/modyear.csv") 

# sensitivity    -----------
sensi_pro <- function(species) {
  COEF_PATH_SENSI <- glue("data/models_resnew/{species}/perm/coefs_{species}.rds")
  COEF_PATH <- glue("data/models_resnew/{species}/perm/coefs_{species}.rds", sep= "")#glue("data/models_resnew/{species}/{species}_fullmodel.rds")
  
  plot_tib <- readRDS(COEF_PATH_SENSI) 
  
  plot_tib2 <- plot_tib %>% 
    filter(mod == "full") %>% 
    mutate(mod2 = 0)
  
  plot_tib3 <- plot_tib %>% 
    filter(!mod == "full")  %>% 
    mutate(mod2 = NA)
  plot_tib3$mod2 <- rep(seq(1:1000), (nrow(plot_tib3)/1000))
  
  plot_tib <- rbind(plot_tib2, plot_tib3)
  
  ggplot(data = plot_tib3, aes(x = par2, y = mean)) +
    #geom_jitter(col = "gray") +
    geom_boxplot() +
    #coord_flip()
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="none") +
    ylab("Estimates") +
    xlab("Coefficients") +
    geom_point(data = plot_tib2, aes(x = par2, y = mean), col = "red") +
    geom_segment(data = plot_tib2, aes(x = par2, y = low, xend = par2, yend = up), col = "red") +
    ggtitle("Sensitivity Analysis")
  
  # pop change
  plot_tib_per <- plot_tib %>% 
    arrange(mod2) %>% 
    pivot_wider(names_from = par2, values_from = mean)
  for(i in 1:nrow(plot_tib_per)){
    for(j in 6:ncol(plot_tib_per)){
      if(is.na(plot_tib_per[i,j])) {plot_tib_per[i,j] <- 0}
    }
  }
  
  # remove NA columns
  plot_tib_per$value <- NA
  
  for(i in 1:nrow(plot_tib_per)){
    plot_tib_per$value[i] <- sum(plot_tib_per[i,6:(ncol(plot_tib_per)-1)])
  }
  
  plot_tib_per <- plot_tib_per %>% 
    select(mod, mod2, low, up, par, value)
  
  mods <- seq(0,1000,1)
  
  # template for predictions
  preds <- rbind(
    read_csv(glue("data/models_resnew/{species}/{species}_t1preds.csv")),
    read_csv(glue("data/models_resnew/{species}/{species}_t2preds.csv")),
    read_csv(glue("data/models_resnew/{species}/{species}_t3preds.csv"))
  ) %>% 
    filter(year == 20) %>% 
    select(year,infoff_t, temp_t, prediction, HWA)
  preds$prop <- preds$prediction <- NA
  preds$species <- as.character(species)
  
  mods_change <- as.data.frame(matrix(NA, ncol = 4, nrow = length(mods)))
  colnames(mods_change) <- c("perm", "t1", "t2", "t3")
  mods_change$perm <- mods
  mods_change$mod2 <- NA
  
  for(k in 1:length(mods + 1)){
    
    mod_ <- mods[k]
    
    pars_tib <- plot_tib_per[which(plot_tib_per$mod2 == mod_),] %>% 
      #filter(mod2 == mod) %>% 
      pivot_wider(names_from = par, values_from = value) %>% 
      select(-c(mod, mod2, low, up))
    
    pars_tib <- colSums(pars_tib, na.rm= T)
    
    pars_tib <- as.data.frame(t(pars_tib))
    
    b0 <- b1 <- b2 <- b3 <- b4 <- b5 <- b6 <- b7 <- 0
    ifelse(!is.na(pars_tib$intercept), b0 <- pars_tib$intercept, b0 <- 0)
    ifelse(!is.na(pars_tib$year_offset), b1 <- pars_tib$year_offset, b1 <- 0)
    ifelse(!is.na(pars_tib$infoff), b2 <- pars_tib$infoff, b2 <- 0)
    ifelse(!is.na(pars_tib$temp_min_scale), b3 <- pars_tib$temp_min_scale, b3 <- 0)
    ifelse(!is.na(pars_tib$year_offset.infoff), b4 <- pars_tib$year_offset.infoff, b4 <- 0)
    ifelse(!is.na(pars_tib$year_offset.temp_min_scale), b5 <- pars_tib$year_offset.temp_min_scale, b5 <- 0)
    ifelse(!is.na(pars_tib$infoff.temp_min_scale), b6 <- pars_tib$infoff.temp_min_scale, b6 <- 0)
    ifelse(!is.na(pars_tib$year_offset.infoff.temp_min_scale), b7 <- pars_tib$  year_offset.infoff.temp_min_scale, b7 <- 0)
    
    no_infes <- preds %>% 
      mutate(prediction = exp(
        b0 + (b1 * year) + (b3 * temp_t) +
          (b5 * year * temp_t)),
        HWA = 'infest'
      ) %>% 
      distinct()
    
    infes <- preds %>% 
      mutate(prediction = exp(
        b0 + (b1 * year) + (b2 * infoff_t) + (b3 * temp_t) +
          (b4 * year * infoff_t) + (b5 * year * temp_t) +
          (b6 * infoff_t * temp_t) + (b7 * year * infoff_t * temp_t)),
        HWA = 'no_infest'
      ) %>% 
      distinct()
    
    sps_preds2 <- rbind(infes, no_infes) %>% 
      arrange(temp_t)
    
    for(j in seq(from=1, to=nrow(sps_preds2), by=2)) {  # infested divided by not_infested
      sps_preds2$prop[j] <- sps_preds2$prop[j+1] <- log(sps_preds2$prediction[j]/sps_preds2$prediction[j+1])
    }
    
    sps_preds3 <- sps_preds2 %>% 
      filter(HWA == "infest")   ## get only a copy from prop
    
    mods_change[k,2:4] <- t(sps_preds3$prop)
    mods_change$mod2[k] <- mods[k]
    
    #print(i) 
  }
  mods_change2 <- mods_change %>%
    pivot_longer(`t1`:`t3`, names_to = "temp", values_to = "prop",
                 names_transform = list(Year = as.numeric),
                 values_ptypes = list(Infested = logical())) 
  mods_change2$species <- as.character(species)
  
  assign(glue("per_prop_{species}"), mods_change2)
  
  return(get(glue("per_prop_{species}")))
}

a <- sensi_pro(sps_list[1,]) %>% 
  mutate(ord = 2)
b <- sensi_pro(sps_list[2,]) %>% 
  mutate(ord = 3)
c <- sensi_pro(sps_list[3,]) %>% 
  mutate(ord = 4)
d <- sensi_pro(sps_list[4,]) %>% 
  mutate(ord = 5)
e <- sensi_pro(sps_list[5,]) %>% 
  mutate(ord = 6)
f <- sensi_pro(sps_list[6,]) %>% 
  mutate(ord = 7)
g <- sensi_pro(sps_list[7,]) %>% 
  mutate(ord = 1)
h <- sensi_pro(sps_list[8,])  %>% 
  mutate(ord = 10)
k <- sensi_pro(sps_list[9,])  %>% 
  mutate(ord = 13)
l <- sensi_pro(sps_list[10,]) %>% 
  mutate(ord = 9)
m <- sensi_pro(sps_list[11,]) %>% 
  mutate(ord = 15)
n <- sensi_pro(sps_list[12,]) %>% 
  mutate(ord = 12)
o <- sensi_pro(sps_list[13,]) %>% 
  mutate(ord = 8)
q <- sensi_pro(sps_list[14,]) %>% 
  mutate(ord = 14)

master_pro <- rbind(m,q,k,n,h,l,o,f,e,d,c,b,a,g)  %>% 
  mutate(ord = as.character(ord))

master_full <- master_pro %>% 
  filter(perm == 0) 

master_pro <- master_pro %>% 
  filter(perm != 0) 

master_full$temp2 <- NA
for(i in 1:nrow(master_full)){
  if(master_full$temp[i] == "t1") {master_full$temp2[i] <- "tt1" }
  if(master_full$temp[i] == "t2") {master_full$temp2[i] <- "tt2" }
  if(master_full$temp[i] == "t3") {master_full$temp2[i] <- "tt3" }
}

master_pro2 <- master_pro %>% 
  select(perm, prop, species, temp) %>% 
  unite(sps_temp, species:temp, remove = FALSE)

temp_order <- as_tibble(matrix(c("t1","t2","t3",1,2,3), nrow = 3)) %>% 
  rename(temp = V1,
         orde = V2)

master_pro2 <- left_join(master_pro2, temp_order, by = "temp") 

master_full2 <- left_join(master_full, temp_order, by = "temp") %>% 
  unite(sps_temp, c(species,temp), remove = FALSE) 

facs <- master_full2$sps_temp
master_full2$sps_temp <- factor(master_full2$sps_temp, 
                                levels = c("WOTH_t1", "WOTH_t2", "WOTH_t3", "WEWA_t1", "WEWA_t2", "WEWA_t3", "WBNU_t1", "WBNU_t2", "WBNU_t3",
                                           "SCTA_t1", "SCTA_t2", "SCTA_t3", "REVI_t1", "REVI_t2", "REVI_t3", "EAPH_t1", "EAPH_t2", "EAPH_t3",
                                           "CERW_t1", "CERW_t2", "CERW_t3", "BLJA_t1", "BLJA_t2", "BLJA_t3", "RBNU_t1", "RBNU_t2", "RBNU_t3",
                                           "MAWA_t1", "MAWA_t2", "MAWA_t3", "HETH_t1", "HETH_t2", "HETH_t3", "BTNW_t1", "BTNW_t2", "BTNW_t3",
                                           "BLBW_t1", "BLBW_t2", "BLBW_t3", "BHVI_t1", "BHVI_t2", "BHVI_t3", "ACFL_t1", "ACFL_t2", "ACFL_t3"))
master_pro2$sps_temp <- factor(master_pro2$sps_temp, levels = facs)

ggplot(data = master_full2, aes(x= sps_temp, y = prop,
                                color = "white"), size = 2) +
  geom_boxplot(width = 0.9999999, fill = "white",
               data = master_pro2, 
               aes(x= sps_temp, y = prop),
               color= "black") +
  geom_hline(yintercept = 0,
             col = "gray43",
             linetype = "dotted",
             size = 1) +
  #geom_point(aes(shape = temp, color = temp), size = 2) +
  #geom_boxplot() + 
  #facet_wrap(~temp) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "right",
        legend.justification = "right",
        #legend.margin=margin(0,0,0,0),
        #legend.box.margin=margin(-5,0,-5,-7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5)) +
  geom_point(data = master_full2, 
             aes(x= sps_temp, y = prop,
                 shape = temp2, color = temp2), size = 1.5) +
  scale_shape_manual(values=c("tt1" = 16,
                              "tt2" = 17,
                              "tt3" = 15)) +
  scale_fill_manual(values=c("olivedrab4",
                             "violetred",
                             "darkorange3")) +
  scale_color_manual(values = c("tt1" = "olivedrab3",
                                "tt2" = "palevioletred2",
                                "tt3" = "tan1"),
                     labels = c("tt1" = "0.2",
                                "tt2" = "0.5",
                                "tt3" = "0.8"),
                     name = "Temperature\nQuantiles") +
  labs(title="Sensitivity analysis") +
  coord_flip()

# boxplot
master_full3 <- master_full2 %>% 
  mutate(tab = "full") %>% 
  select(sps_temp, prop, orde, tab, temp, temp2, species)

master_pro3 <- master_pro2 %>% 
  mutate(tab = "pro", temp2 = NA) %>% 
  select(sps_temp, prop, orde, tab, temp, temp2, species)

master_pro_full <- rbind(master_full3, master_pro3) %>% 
  group_by(sps_temp) #%>%
#mutate(across(contains("prop"),scale))

master_full3 <- master_pro_full %>% 
  filter(tab == "full")

master_pro3 <- master_pro_full %>% 
  filter(tab == "pro")

svg(glue("Figures/sensitivity.svg"), 
    width = 9, height = 10)

ggplot(data = master_pro3, aes(y= sps_temp, x = prop)) +
  geom_vline(xintercept = 0,
             col = "gray43",
             linetype = "dotted",
             size = 0.8) +
  geom_vline(xintercept = -0.3,
             col = "gray43",
             size = 0.8) +
  geom_vline(xintercept = 0.3,
             col = "gray43",
             size = 0.8) +
  #geom_point(aes(shape = temp, color = temp), size = 2) +
  geom_boxplot(weight = 10, alpha = 0.7) + 
  # facet_wrap(~temp) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "right",
        legend.justification = "right",
        #legend.margin=margin(0,0,0,0),
        #legend.box.margin=margin(-5,0,-5,-7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #scale_x_continuous(breaks = seq(-3, 1, 0.5),
  #                   limits = c(-3, 1, 0.5)) +
  geom_point(data = master_full3, aes(y= sps_temp, x = prop,
                                      shape = temp2, color = temp2), size = 3) +
  scale_shape_manual(values=c("tt1" = 16,
                              "tt2" = 17,
                              "tt3" = 15)) +
  scale_fill_manual(values=c("olivedrab4",
                             "violetred",
                             "darkorange3")) +
  scale_color_manual(values = c("tt1" = "olivedrab3",
                                "tt2" = "palevioletred2",
                                "tt3" = "tan1"),
                     labels = c("tt1" = "0.2",
                                "tt2" = "0.5",
                                "tt3" = "0.8"),
                     name = "Temperature\nQuantiles") +
  labs(title="Sensitivity analysis") 

dev.off()

# permutation ----------
pred_per_sps <- function(species){
  TEMPQUANT_PATH <- glue("data/tempquant.csv")
  COEF_PERM_PATH <- glue("data/models_resnew/{species}/perm/coefs_{species}.rds", sep= "")
  
  coefs <- read_rds(COEF_PERM_PATH)  
  temp_qunts <- read_csv(TEMPQUANT_PATH)
  
  temp_qunts2 <- temp_qunts %>% 
    as_tibble() %>% 
    rename(Species = species, temp1 = ...2, temp2 = ...3, temp3 = ...4) %>% 
    dplyr::filter(trimws(Species) == trimws(species))
  
  coefs2 <- coefs %>% 
    filter(mod != "full")
  
  coefs3 <- coefs %>% 
    filter(mod == "full") 
  
  coefs2 <- coefs2 %>%
    select(par, mean) %>% 
    group_by(par) %>%
    mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = par, values_from = mean) %>%
    select(-row) %>% 
    mutate(type = "perm")
  
  coefs3 <- coefs3 %>%
    select(par, mean) %>% 
    group_by(par) %>%
    mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = par, values_from = mean) %>%
    select(-row) %>% 
    mutate(type = "full")
  
  coefs <- rbind(coefs2, coefs3)
  
  pmat <- coefs %>% 
    mutate(time = 20,
           Species = species
    )
  
  offsets <- yrmod %>% 
    rename(Species = species) %>% 
    filter(Species == species) %>% 
    dplyr::select(year) %>% 
    pull()
  
  pmat <- left_join(pmat, temp_qunts2, by= "Species") %>% 
    dplyr::select(-Species) %>% 
    mutate(infoff_t = offsets)
  
  # estimates -----------
  prop_tabX <- as_tibble(matrix(NA,nrow = nrow(pmat), ncol = 7))
  colnames(prop_tabX) <- c("noinf1", "inf1", "noinf2", "inf2", "noinf3", "inf3", "type")
  
  for (i in 1:nrow(prop_tabX)){
    
    ifelse(is.null(pmat$intercept[i]), b0 <- 0, 
           ifelse(!is.na(pmat$intercept[i]), b0 <- as.numeric(pmat$intercept[i]), b0 <- 0))
    ifelse(is.null(pmat$year_offset[i]), b1 <- 0, 
           ifelse(!is.na(pmat$year_offset[i]), b1 <- as.numeric(pmat$year_offset[i]), b1 <- 0))
    ifelse(is.null(pmat$infoff[i]), b2 <- 0,
           ifelse(!is.na(pmat$infoff[i]), b2 <- as.numeric(pmat$infoff[i]), b2 <- 0))
    ifelse(is.null(pmat$temp_min_scale[i]), b3 <- 0, 
           ifelse(!is.na(pmat$temp_min_scale[i]), b3 <- as.numeric(pmat$temp_min_scale[i]), b3 <- 0))
    ifelse(is.null(pmat$year_offset.infoff[i]), b4 <- 0, 
           ifelse(!is.na(pmat$year_offset.infoff[i]), b4 <- as.numeric(pmat$year_offset.infoff[i]), b4 <- 0))
    ifelse(is.null(pmat$year_offset.temp_min_scale[i]), b5 <- 0, 
           ifelse(!is.na(pmat$year_offset.temp_min_scale[i]), b5 <- as.numeric(pmat$year_offset.temp_min_scale[i]), b5 <- 0))
    ifelse(is.null(pmat$infoff.temp_min_scale[i]), b6 <- 0,
           ifelse(!is.na(pmat$infoff.temp_min_scale[i]), b6 <- as.numeric(pmat$infoff.temp_min_scale[i]), b6 <- 0))
    ifelse(is.null(pmat$year_offset.infoff.temp_min_scale[i]), b7 <- 0,
           ifelse(!is.na(pmat$year_offset.infoff.temp_min_scale[i]), b7 <- as.numeric(pmat$year_offset.infoff.temp_min_scale[i]), b7 <- 0))
    
    no_infes <- pmat[i,] %>% 
      mutate(prediction1 = exp(b0 + (b1 * time) + (b3 * temp1) + (b5 * year_offset * temp1)),
             prediction2 = exp(b0 + (b1 * time) + (b3 * temp2) + (b5 * year_offset * temp2)),
             prediction3 = exp(b0 + (b1 * time) + (b3 * temp3) + (b5 * year_offset * temp3)),
             HWA = 'infest'
      )
    
    infes <- pmat[i,] %>% 
      mutate(prediction1 = exp(b0 + (b1 * time) + (b2 * infoff_t) + (b3 * temp1) +
                                 (b4 * time * infoff_t) + (b5 * time * temp1) +
                                 (b6 * infoff_t * temp1) + (b7 * time * infoff_t * temp1)),
             prediction2 = exp(b0 + (b1 * time) + (b2 * infoff_t) + (b3 * temp2) +
                                 (b4 * time * infoff_t) + (b5 * time * temp2) +
                                 (b6 * infoff_t * temp2) + (b7 * time * infoff_t * temp2)),
             prediction3 = exp(b0 + (b1 * time) + (b2 * infoff_t) + (b3 * temp3) +
                                 (b4 * time * infoff_t) + (b5 * time * temp3) +
                                 (b6 * infoff_t * temp3) + (b7 * time * infoff_t * temp3)),
             HWA = 'no_infest'
      )
    prop_tabX$noinf1[i] <- no_infes$prediction1
    prop_tabX$inf1[i] <- infes$prediction1
    prop_tabX$noinf2[i] <- no_infes$prediction2
    prop_tabX$inf2[i] <- infes$prediction2
    prop_tabX$noinf3[i] <- no_infes$prediction3
    prop_tabX$inf3[i] <- infes$prediction3
    prop_tabX$type[i] <- pmat$type[i]
    rm(no_infes, infes,
       b0, b1, b2, b3, b4, b5, b6, b7)
  }
  
  prop_tabX <- prop_tabX %>% 
    mutate(t1 = log(noinf1/inf1),
           t2 = log(noinf2/inf2),
           t3 = log(noinf3/inf3),
           Species = species)
  
  prop_tabX2 <- prop_tabX %>% 
    pivot_longer(`t1`:`t3`, names_to = "temp", values_to = "pop202") %>% 
    select(Species, temp, pop202, type) %>% 
    mutate(sps_temp = glue("{Species}_{temp}", remove = FALSE))
  
  temp_order <- as_tibble(matrix(c("t1","t2","t3",1,2,3), nrow = 3)) %>% 
    rename(temp = V1,
           orde = V2)
  
  fullm <- prop_tabX2 %>% filter(type == "full")
  
  prop_tabX2 <- prop_tabX2 %>% filter(type == "perm")
  
  prop_tabX3 <- left_join(prop_tabX2, temp_order, by = "temp") %>% 
    arrange(orde)
  
  x <- list(prop_tabX3, fullm)
  
  return(x)
  
}

per1 <- pred_per_sps(trimws(sps_list[1,]))[[1]]
per2 <- pred_per_sps(trimws(sps_list[2,]))[[1]]
per3 <- pred_per_sps(trimws(sps_list[3,]))[[1]]
per4 <- pred_per_sps(trimws(sps_list[4,]))[[1]]
per5 <- pred_per_sps(trimws(sps_list[5,]))[[1]]
per6 <- pred_per_sps(trimws(sps_list[6,]))[[1]]
per7 <- pred_per_sps(trimws(sps_list[7,]))[[1]]
per8 <- pred_per_sps(trimws(sps_list[8,]))[[1]]
per9 <- pred_per_sps(trimws(sps_list[9,]))[[1]]
per10 <- pred_per_sps(trimws(sps_list[10,]))[[1]]
per11 <- pred_per_sps(trimws(sps_list[11,]))[[1]]
per12 <- pred_per_sps(trimws(sps_list[12,]))[[1]]
per13 <- pred_per_sps(trimws(sps_list[13,]))[[1]]
per14 <- pred_per_sps(trimws(sps_list[14,]))[[1]]

prop_tabX3 <- rbind(per1, per2, per3, per4, per5, per6, per7, per8, per9, 
                    per10, per11, per12, per13, per14) %>% 
  arrange(match(Species, master_full2$sps_temp))
prop_tabX3 <- prop_tabX3 %>% 
  mutate(orde = seq(1:nrow(prop_tabX3)),
         sps_temp = factor(sps_temp, levels = facs))

full1 <- pred_per_sps(trimws(sps_list[1,]))[[2]]
full2 <- pred_per_sps(trimws(sps_list[2,]))[[2]]
full3 <- pred_per_sps(trimws(sps_list[3,]))[[2]]
full4 <- pred_per_sps(trimws(sps_list[4,]))[[2]]
full5 <- pred_per_sps(trimws(sps_list[5,]))[[2]]
full6 <- pred_per_sps(trimws(sps_list[6,]))[[2]]
full7 <- pred_per_sps(trimws(sps_list[7,]))[[2]]
full8 <- pred_per_sps(trimws(sps_list[8,]))[[2]]
full9 <- pred_per_sps(trimws(sps_list[9,]))[[2]]
full10 <- pred_per_sps(trimws(sps_list[10,]))[[2]]
full11 <- pred_per_sps(trimws(sps_list[11,]))[[2]]
full12 <- pred_per_sps(trimws(sps_list[12,]))[[2]]
full13 <- pred_per_sps(trimws(sps_list[13,]))[[2]]
full14 <- pred_per_sps(trimws(sps_list[14,]))[[2]]

master_full_per <- rbind(full1, full2, full3, full4, full5, full6, full7, full8, full9, 
                         full10, full11, full12, full13, full14) %>% 
  arrange(match(Species, master_full2$sps_temp))
master_full_per <- master_full_per %>% 
  mutate(orde = seq(1:nrow(master_full_per)),
         temp2 = glue("t{temp}"))

ggplot(data = prop_tabX3 %>% filter(Species != "CERW"), aes(y = sps_temp, x = pop202)) +
  geom_boxplot() + 
  #geom_point(aes(shape = temp), colour = "grey", size = 2) +
  geom_vline(xintercept = 0, col = "gray43",
             linetype = "dotted", size = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "right",
        legend.justification = "right",
        #legend.margin=margin(0,0,0,0),
        #legend.box.margin=margin(-5,0,-5,-7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title.align = 0.5) +
  geom_point(data = master_full_per, 
             aes(y = sps_temp, x = pop202,
                 shape = temp2, color = temp2), size = 2) +
  scale_shape_manual(values=c("tt1" = 16,
                              "tt2" = 17,
                              "tt3" = 15)) +
  scale_fill_manual(values=c("olivedrab4",
                             "violetred",
                             "darkorange3")) +
  scale_color_manual(values = c("tt1" = "olivedrab3",
                                "tt2" = "palevioletred2",
                                "tt3" = "tan1"),
                     labels = c("tt1" = "0.2",
                                "tt2" = "0.5",
                                "tt3" = "0.8"),
                     name = "Temperature\nQuantiles") +
  labs(title="Permutation analysis")


#svg(glue("Figures/sensitivity.svg"), 
#    width = 9, height = 10)

ggplot(data = prop_tabX3 
       %>% filter(Species != "CERW")
       , aes(y = sps_temp, x = pop202)) +
  geom_vline(xintercept = 0,
             col = "gray43",
             linetype = "dotted",
             size = 0.8) +
  geom_vline(xintercept = -0.3,
             col = "gray43",
             size = 0.8) +
  geom_vline(xintercept = 0.3,
             col = "gray43",
             size = 0.8) +
  #geom_point(aes(shape = temp, color = temp), size = 2) +
  geom_boxplot(weight = 10, alpha = 0.7) + 
  # facet_wrap(~temp) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "right",
        legend.justification = "right",
        #legend.margin=margin(0,0,0,0),
        #legend.box.margin=margin(-5,0,-5,-7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #scale_x_continuous(breaks = seq(-3, 1, 0.5),
  #                   limits = c(-3, 1, 0.5)) +
  geom_point(data = master_full3, aes(y= sps_temp, x = prop,
                                      shape = temp2, color = temp2), size = 3) +
  scale_shape_manual(values=c("tt1" = 16,
                              "tt2" = 17,
                              "tt3" = 15)) +
  scale_fill_manual(values=c("olivedrab4",
                             "violetred",
                             "darkorange3")) +
  scale_color_manual(values = c("tt1" = "olivedrab3",
                                "tt2" = "palevioletred2",
                                "tt3" = "tan1"),
                     labels = c("tt1" = "0.2",
                                "tt2" = "0.5",
                                "tt3" = "0.8"),
                     name = "Temperature\nQuantiles") +
  labs(title="Sensitivity analysis") 

#dev.off()
