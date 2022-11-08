# 11_PlotPercent -----------------------------------------------------
# make plot of percent of population change after 20 years of infestation
#
# Input: "data/models_resnew/{spsr}/{spsr}_{temp_n}preds.csv" for each species
#
# Output: plot of population rate of change for each temperature quantile
#

library(tidyverse)
library(glue)
library(gridExtra)
library(egg)

SPSLIST_PATH <- glue("data/src/sps_list.csv")
SPSPRED_PATH <- glue("data/models_resnew/")

spslist <- read_csv(SPSLIST_PATH)

sps_preds <- matrix(ncol= 8, nrow = 0)
colnames(sps_preds) <- c("year","infoff_t","year_off_t","temp_t","prediction","HWA","temp","species" )
sps_preds <- as_tibble(sps_preds)
ts <- c("t1","t2","t3")

for(i in 1:nrow(spslist)){
  spp <- spslist[i,]
  for(j in 1:3){
    t <- ts[j]
    a <- read_csv(glue(SPSPRED_PATH,"{spp}/{spp}_{t}preds.csv")) %>% 
      dplyr::select(year, infoff_t, year_off_t, temp_t, prediction, HWA) %>% 
      mutate(temp = t,
             species = pull(spp)) 
    sps_preds <- rbind(sps_preds,a)
  }
  rm(spp)
}

sps_preds2 <- sps_preds %>% 
  filter(year == 20)
sps_preds2$prop <- NA
#sps_preds2$propper <- NA

for(i in seq(from=1, to=nrow(sps_preds2), by=2)) {
  sps_preds2$prop[i] <- sps_preds2$prop[i+1] <- log(sps_preds2$prediction[i]/sps_preds2$prediction[i+1])
  #sps_preds2$propper[i] <- sps_preds2$propper[i+1] <- (sps_preds2$prediction[i] - sps_preds2$prediction[i+1])/sps_preds2$prediction[i+1]
  
}

sps_preds3 <- sps_preds2 %>% 
  filter(HWA == "infest") %>%    ## get only a copy from prop
  unite(sps_temp, c(species,temp), remove = FALSE) 

order <- rev(c("ACFL", "BHVI", "BLBW", "BTNW", "HETH", "MAWA", "RBNU",
               "BLJA", "CERW", "EAPH", "REVI", "SCTA", "WBNU", "WOTH"))

sps_preds3$species <- factor(sps_preds3$species, levels = order) 

facs <- sps_preds3$sps_temp
sps_preds3$sps_temp <- factor(sps_preds3$sps_temp, 
                                levels = c("WOTH_t1", "WOTH_t2", "WOTH_t3", "WEWA_t1", "WEWA_t2", "WEWA_t3", "WBNU_t1", "WBNU_t2", "WBNU_t3",
                                           "SCTA_t1", "SCTA_t2", "SCTA_t3", "REVI_t1", "REVI_t2", "REVI_t3", "EAPH_t1", "EAPH_t2", "EAPH_t3",
                                           "CERW_t1", "CERW_t2", "CERW_t3", "BLJA_t1", "BLJA_t2", "BLJA_t3", "RBNU_t1", "RBNU_t2", "RBNU_t3",
                                           "MAWA_t1", "MAWA_t2", "MAWA_t3", "HETH_t1", "HETH_t2", "HETH_t3", "BTNW_t1", "BTNW_t2", "BTNW_t3",
                                           "BLBW_t1", "BLBW_t2", "BLBW_t3", "BHVI_t1", "BHVI_t2", "BHVI_t3", "ACFL_t1", "ACFL_t2", "ACFL_t3"))

svg(glue("Figures/Fig3/percent_plot.svg"), 
    width = 8, height = 9)
ggplot(data = sps_preds3, aes(y= sps_temp, x = prop)) +
  geom_vline(xintercept = 0,
             col = "gray43",
             size = 0.8) +
  geom_vline(xintercept = -0.3,
             col = "gray43",
             linetype = "dotted",
             size = 0.8) +
  geom_vline(xintercept = 0.3,
             col = "gray43",
             linetype = "dotted",
             size = 0.8) +
  geom_point(aes(shape = temp, color = temp), size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        #legend.title = element_blank(),
        legend.position = "none",
        #axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5) +
  scale_x_continuous(breaks = seq(-3, 1, 1),
                     limits = c(-3, 1, 1)) +
  scale_color_manual(values = c("t1" = "olivedrab4",
                                "t2" = "violetred",
                                "t3" = "darkorange3"),
                     labels = c("t1" = "0.2",
                                "t2" = "0.5",
                                "t3" = "0.8"),
                     name = "Temperature\nQuantiles") +
  xlab("Log(Not infested route \n      Infested route)") +
  scale_y_discrete(labels = c("\n", "WOTH", "\n", "\n", "REVI", "\n", "\n", "WBNU", "\n", "\n", "SCTA", "\n",
                              "\n", "EAPH", "\n", "\n", "CERW", "\n", "\n", "BLJA", "\n", "\n", "RBNU", "\n",
                              "\n", "MAWA", "\n", "\n", "HETH", "\n", "\n", "BTNW", "\n", "\n", "BLBW", "\n",
                              "\n", "BHVI", "\n", "\n", "ACFL", "\n")) 

dev.off()
