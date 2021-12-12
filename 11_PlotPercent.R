## Script to import the results of coeficients of best models for all species affected by hemlock and controls

library(tidyverse)
library(glue)
library(gridExtra)
library(egg)
#library(grid)
#library(RColorBrewer)

SPSLIST_PATH <- glue("data/src/sps_list.csv")
SPSPRED_PATH <- glue("data/preds/preds_{species}.rds")

spslist <- read_csv(SPSLIST_PATH)

preds_WEWA <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/preds/preds_WEWA.rds") %>% 
  mutate(species = "WEWA")
preds_REVI <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/preds/preds_REVI.rds") %>% 
  mutate(species = "REVI")
preds_BLJA <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/preds/preds_BLJA.rds") %>% 
  mutate(species = "BLJA")
preds_SCTA <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/preds/preds_SCTA.rds") %>% 
  mutate(species = "SCTA")
preds_WOTH <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/preds/preds_WOTH.rds") %>% 
  mutate(species = "WOTH")
preds_CERW <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/preds/preds_CERW.rds") %>% 
  mutate(species = "CERW")
preds_WBNU <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/preds/preds_WBNU.rds") %>% 
  mutate(species = "WBNU")
preds_EAPH <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/preds/preds_EAPH.rds") %>% 
  mutate(species = "EAPH")
preds_ACFL <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/preds/preds_ACFL.rds") %>% 
  mutate(species = "ACFL")
preds_RBNU <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/preds/preds_RBNU.rds") %>% 
  mutate(species = "RBNU")
preds_MAWA <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/preds/preds_MAWA.rds") %>% 
  mutate(species = "MAWA")
preds_HETH <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/preds/preds_HETH.rds") %>% 
  mutate(species = "HETH")
preds_BLBW <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/preds/preds_BLBW.rds") %>% 
  mutate(species = "BLBW")
preds_BHVI <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/preds/preds_BHVI.rds") %>% 
  mutate(species = "BHVI")
preds_BTNW <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/BirdHWA/data/preds/preds_BTNW.rds") %>% 
  mutate(species = "BTNW")

sps_preds <- rbind(preds_WEWA, preds_REVI, preds_BLJA, preds_SCTA, preds_WOTH, preds_CERW, preds_WBNU,
                   preds_EAPH, preds_ACFL, preds_RBNU, preds_MAWA, preds_HETH, preds_BLBW, preds_BHVI,
                   preds_BTNW)

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
