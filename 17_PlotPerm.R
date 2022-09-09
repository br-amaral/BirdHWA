# PERMUTATION PLOT
# Plot the histograms of the permutation analysis plus the results of the 'regular' model

# Input:
#   data/src/sps_list.csv: list of species being analysed 
#   data/modyear.csv: best model and offset year for each species
#   data/models_resnew/{species}/perm/coefs_{species}.rds: coefficients for each iteration of the permutation analysis
#   data/models_resnew/{species}/{species}_t{1, 2 or 3}preds.csv: predictions for populations for each temperature quantile 
#                                                                    according to the best model and year offset
# Output:
#   Figures/FigS4/permutation.svg": histogram of predictions of all permutation iterations contrasted with 'regular' results

# Load packages -------------------------
library(ggplot2)
library(tidyverse)
library(glue)

# Load data -----------------------------

SPECIES_DATA_PATH <- "data/src/sps_list.csv"
sps_list <- read_csv(SPECIES_DATA_PATH)
order <- rev(rep(c("ACFL", "BHVI", "BLBW", "BTNW", "HETH", "MAWA", "RBNU",
                   "BLJA", "CERW", "EAPH", "REVI", "SCTA", "WBNU", "WOTH"), each = 3))

yrmod <- read_csv(file = "data/modyear.csv") 
perm <- 1000

# Permutation function ----------------------
#   function to import coefficients for each iteration of the permutation analysis and plot them
#   clone of the script of the sensitivity analysis (15.5_PlotSensi.R)
permute_func <- function(species) {
  COEF_PATH_SENSI <- glue("data/models_resnew/{species}/perm/coefs_{species}.rds")

  plot_tib <- readRDS(COEF_PATH_SENSI) 
  
  plot_tib2 <- plot_tib %>% 
    filter(mod == "full") %>% 
    mutate(mod2 = 0)
  
  plot_tib3 <- plot_tib %>% 
    filter(!mod == "full")  %>% 
    mutate(mod2 = NA)
  plot_tib3$mod2 <- rep(seq(1:perm), (nrow(plot_tib3)/perm))
  
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
  
  mods <- seq(0,perm,1)
  
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

# Import coefficients, make predictions and build figure ----------------------------
a <- permute_func(sps_list[1,]) %>% 
  mutate(ord = 2)
b <- permute_func(sps_list[2,]) %>% 
  mutate(ord = 3)
c <- permute_func(sps_list[3,]) %>% 
  mutate(ord = 4)
d <- permute_func(sps_list[4,]) %>% 
  mutate(ord = 5)
e <- permute_func(sps_list[5,]) %>% 
  mutate(ord = 6)
f <- permute_func(sps_list[6,]) %>% 
  mutate(ord = 7)
g <- permute_func(sps_list[7,]) %>% 
  mutate(ord = 1)
h <- permute_func(sps_list[8,])  %>% 
  mutate(ord = 10)
k <- permute_func(sps_list[9,])  %>% 
  mutate(ord = 13)
l <- permute_func(sps_list[10,]) %>% 
  mutate(ord = 9)
m <- permute_func(sps_list[11,]) %>% 
  mutate(ord = 15)
n <- permute_func(sps_list[12,]) %>% 
  mutate(ord = 12)
o <- permute_func(sps_list[13,]) %>% 
  mutate(ord = 8)
q <- permute_func(sps_list[14,]) %>% 
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

master_pro2 <- master_pro2 %>% 
  group_by(sps_temp) %>% 
  mutate(#up = mean(prop) + ((1.96*sqrt(var(prop) / length(prop)))),
         #lo = mean(prop) - ((1.96*sqrt(var(prop) / length(prop)))),
         up = mean(prop) + ((1.96*sqrt(var(prop)))),
         lo = mean(prop) - ((1.96*sqrt(var(prop))))) %>% 
  ungroup()
  
table95 <- master_pro2 %>% 
  dplyr::select(species, sps_temp, up, lo) %>% 
  distinct() 
table95[which(table95$species == "CERW"), 3:4] <- matrix(rep(c(-3.37,1.6),3), nrow = 3, byrow = T)

# Export figure ----------------------
svg(glue("Figures/FigS4/permutation.svg"), 
    width = 10, height = 6.3)

ggplot(data = master_full2, aes(x= sps_temp, y = prop,
                                color = "white"), size = 2) +
  geom_segment(aes(y=lo, yend=up ,x=sps_temp, xend=sps_temp),
               size = 4, data = table95, alpha = 0.3, color = "yellow") +
  geom_hline(yintercept = 0,
             col = "gray43",
             size = 0.8) +
  geom_hline(yintercept = -0.3,
             col = "gray43",
             size = 0.8, linetype = "dotted") +
  geom_hline(yintercept = 0.3,
             col = "gray43",
             size = 0.8, linetype = "dotted") +
  geom_boxplot(width = 0.75, fill = "white",
               data = master_pro2, 
               aes(x= sps_temp, y = prop),
               color= "black",weight = 10, alpha = 0.7) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position="none",
        #legend.margin=margin(0,0,0,0),
        #legend.box.margin=margin(-5,0,-5,-7),
        #axis.title.x = element_blank(),
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
  scale_color_manual(values = c("tt1" = "olivedrab4",
                                "tt2" = "palevioletred2",
                                "tt3" = "tan1"),
                     labels = c("tt1" = "0.2",
                                "tt2" = "0.5",
                                "tt3" = "0.8"),
                     name = "Temperature\nQuantiles") +
  labs(title="Permutation analysis") +
  ylab("Log(Not infested route \n      Infested route)") +
  scale_y_continuous(breaks = c(-3,-2,-1,0,1), limits  = c(-3.37,1.6)) +
  scale_x_discrete(labels = c("\n", "WOTH", "\n", "\n", "REVI", "\n", "\n", "WBNU", "\n", "\n", "SCTA", "\n",
                              "\n", "EAPH", "\n", "\n", "CERW", "\n", "\n", "BLJA", "\n", "\n", "RBNU", "\n",
                              "\n", "MAWA", "\n", "\n", "HETH", "\n", "\n", "BTNW", "\n", "\n", "BLBW", "\n",
                              "\n", "BHVI", "\n", "\n", "ACFL", "\n")) + 
  coord_flip() +
  ylab("Log(Not infested route       Infested route)") 


dev.off()
