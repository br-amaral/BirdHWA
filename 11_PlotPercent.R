# Plot percent of population change after 20 years of infestation
# Input: preds_{species} for each species
# Output: plot of population rate of change for each temperature quantile

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

for(i in seq(from=1, to=nrow(sps_preds2), by=2)) {
  sps_preds2$prop[i] <- sps_preds2$prop[i+1] <- log(sps_preds2$prediction[i]/sps_preds2$prediction[i+1])
}

sps_preds3 <- sps_preds2 %>% 
  filter(HWA == "infest")   ## get only a copy from prop

order <- rev(c("ACFL", "BHVI", "BLBW", "BTNW", "HETH", "MAWA", "RBNU",
               "BLJA", "CERW", "EAPH", "REVI", "SCTA", "WBNU", "WOTH"))

sps_preds3$species <- factor(sps_preds3$species, levels = order)

svg(glue("Figures/percent_plot.svg"), 
    width = 8, height = 9)
ggplot(data = sps_preds3, aes(y= species, x = prop)) +
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
  geom_point(aes(shape = temp, color = temp), size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        #legend.title = element_blank(),
        legend.position = "right",
        legend.justification = "right",
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        #legend.margin=margin(0,0,0,0),
        #legend.box.margin=margin(-5,0,-5,-7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title.align = 0.5) +
  scale_x_continuous(breaks = seq(-3, 1, 1),
                     limits = c(-3, 1, 1)) +
  scale_color_manual(values = c("t1" = "blue4",
                                "t2" = "violetred",
                                "t3" = "darkorange3"),
                     labels = c("t1" = "0.2",
                                "t2" = "0.5",
                                "t3" = "0.8"),
                     name = "Temperature\nQuantiles")
dev.off()
