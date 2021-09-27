## Script to import the results of coeficients of best models for all species affected by hemlock and controls

library(tidyverse)
library(glue)
library(gridExtra)
library(egg)
#library(grid)
#library(RColorBrewer)

COEFMAT_PATH <- glue("data/coef_species.csv")
INTERCEPT_PATH <- glue("data/intercepts.csv")
TEMPQUANT_PATH <- glue("data/tempquant.csv")

pmat <- read_csv(COEFMAT_PATH, col_types = cols(
  year_offset_over = col_character(),
  infoff_over = col_character(),
  NewObserver_over = col_character(),
  temp_min_scale_over = col_character(),
  year_offset.infoff_over = col_character(),
  temp_min_scale.year_offset_over = col_character(),
  temp_min_scale.infoff_over = col_character(),
  temp_min_scale.infoff.year_offset_over = col_character()
))

# create a zero and one variable to indicate if it overlaps zero
colsindex <- c(0,4,8,12,16,20,24,28)

for(k in 1:length(colsindex)){
  
  j <- colsindex[k]
  
  for(i in 1:nrow(pmat)){
    
    if(is.na(between(0, pmat[i,7+j], pmat[i,8+j]))) {
      pmat[i,9+j] <- NA
      
    } else {
      
      if(between(0, pmat[i,7+j], pmat[i,8+j]) == FALSE) {
        pmat[i,9+j] <- "sig"
      }
      
      if(between(0, pmat[i,7+j], pmat[i,8+j]) == TRUE) {
        pmat[i,9+j] <- "zero"
      }
    }
    
  }
}

## plots

## models and years ---------------------------------
a <-
  ggplot(data = pmat, aes(x= model, y= reorder(species,order),
                          colour = factor(group),
                          shape = factor(treat_cont),
                          size =  0.8)) +
  geom_point(size = 2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1,16,1)) +
  geom_point(x= pmat$year, y= pmat$species,
             colour = 'black',
             shape = 3,
             size = 2) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        axis.title.y=element_blank()) +
  ggtitle("Model (symbol) and year (cross)")

## coeficients

## infoff ---------------------------
b <-
  ggplot(data = pmat, aes(x= infoff, y= reorder(species,order),
                          colour = factor(group),
                          shape = factor(treat_cont),
                          size = 0.8)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        axis.title.y=element_blank()) +
  geom_errorbarh(aes(xmax = infoff_up, xmin = infoff_low), height = .2, size = 0.1) +
  ggtitle("Change in intercept with infestation") +
  #  scale_x_continuous(breaks = seq(1,16,1)) +
  #  geom_point(x= pmat$year, y= pmat$species,
  #             colour = 'black',
  #             shape = 3,
  #             size = 4)
  geom_point(aes(x= infoff, y= reorder(species,order),
                 shape = factor(treat_cont),
                 size =  0.8),  colour = "gray79", size = 2,
             data = pmat[which(pmat$infoff_over == "zero"),]) +
  geom_errorbarh(aes(xmax = infoff_up, xmin = infoff_low), height = .2, size = 0.1,
                 colour = "gray79",
                 data = pmat[which(pmat$infoff_over == "zero"),])

## infoff:year_offset ---------------------------
c <- 
  ggplot(data = pmat, aes(x= year_offset.infoff, y= reorder(species,order),
                          colour = factor(group),
                          shape = factor(treat_cont),
                          size =  0.8)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        axis.title.y=element_blank()) +
  geom_errorbarh(aes(xmax = year_offset.infoff_up, xmin = year_offset.infoff_low), height = .2, size = 0.1) +
  ggtitle("Change in slope with infestation") +
  geom_point(aes(x= year_offset.infoff, y= reorder(species,order),
                 shape = factor(treat_cont),
                 size =  0.8),  colour = "gray79", size = 2,
             data = pmat[which(pmat$year_offset.infoff_over == "zero"),]) +
  geom_errorbarh(aes(xmax = year_offset.infoff_up, xmin = year_offset.infoff_low), height = .2, size = 0.1,
                 colour = "gray79",
                 data = pmat[which(pmat$year_offset.infoff_over == "zero"),])

## temperature ---------------------------
d <- 
  ggplot(data = pmat, aes(x= temp_min_scale, y= reorder(species,order),
                          colour = factor(group),
                          shape = factor(treat_cont),
                          size =  0.8)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        axis.title.y=element_blank()) +
  geom_errorbarh(aes(xmax = temp_min_scale_up, xmin = temp_min_scale_low), height = .2, size = 0.1) +
  ggtitle("Minimum temperature") +
  geom_point(aes(x= temp_min_scale, y= reorder(species,order),
                 shape = factor(treat_cont),
                 size =  0.8),  colour = "gray79", size = 2,
             data = pmat[which(pmat$temp_min_scale_over == "zero"),]) +
  geom_errorbarh(aes(xmax = temp_min_scale_up, xmin = temp_min_scale_low), height = .2, size = 0.1,
                 colour = "gray79",
                 data = pmat[which(pmat$temp_min_scale_over == "zero"),])

## temperature and infestation ---------------------------
e <- 
  ggplot(data = pmat, aes(x= temp_min_scale.infoff, y= reorder(species,order),
                          colour = factor(group),
                          shape = factor(treat_cont),
                          size =  0.8)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        axis.title.y=element_blank()) +
  geom_errorbarh(aes(xmax = temp_min_scale.infoff_up, xmin = temp_min_scale.infoff_low), height = .2, size = 0.1) +
  ggtitle("Minimum temperature and infestation") +
  geom_point(aes(x= temp_min_scale.infoff, y= reorder(species,order),
                 shape = factor(treat_cont),
                 size =  0.8),  colour = "gray79", size = 2,
             data = pmat[which(pmat$temp_min_scale.infoff_over == "zero"),]) +
  geom_errorbarh(aes(xmax = temp_min_scale.infoff_up, xmin = temp_min_scale.infoff_low), height = .2, size = 0.1,
                 colour = "gray79",
                 data = pmat[which(pmat$temp_min_scale.infoff_over == "zero"),])

## temperature and infestation and year_offset ---------------------------
f <- 
  ggplot(data = pmat, aes(x= temp_min_scale.infoff.year_offset, y= reorder(species,order),
                          colour = factor(group),
                          shape = factor(treat_cont),
                          size =  0.8)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        axis.title.y=element_blank()) +
  geom_errorbarh(aes(xmax = temp_min_scale.infoff.year_offset_up, xmin = temp_min_scale.infoff.year_offset_low), height = .2, size = 0.1) +
  ggtitle("Minimum temperature, time and infestation") 
#  geom_point(aes(x= temp_min_scale.infoff.year_offset, y= reorder(species,order),
#                 shape = factor(treat_cont),
#                 size =  0.8),  colour = "gray79",
#             data = pmat[which(pmat$temp_min_scale.infoff.year_offset_over == "zero"),]) +
#  geom_errorbarh(aes(xmax = temp_min_scale.infoff.year_offset_up, xmin = temp_min_scale.infoff.year_offset_low), height = .2, size = 0.1,
#                 colour = "gray79",
#                 data = pmat[which(pmat$temp_min_scale.infoff.year_offset_over == "zero"),])

## year_off ---------------------------
g <- 
  ggplot(data = pmat, aes(x= year_offset, y= reorder(species,order),
                          colour = factor(group),
                          shape = factor(treat_cont),
                          size =  0.8)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        axis.title.y=element_blank()) +
  geom_errorbarh(aes(xmax = year_offset_up, xmin = year_offset_low), height = .2, size = 0.1) +
  ggtitle("Year since infestation") +
  geom_point(aes(x= year_offset, y= reorder(species,order),
                 shape = factor(treat_cont),
                 size =  0.8),  colour = "gray79", size = 2,
             data = pmat[which(pmat$year_offset_over == "zero"),]) +
  geom_errorbarh(aes(xmax = year_offset_up, xmin = year_offset_low), height = .2, size = 0.1,
                 colour = "gray79",
                 data = pmat[which(pmat$year_offset_over == "zero"),])

## New observer ---------------------------
h <-
  ggplot(data = pmat, aes(x= NewObserver, y= reorder(species,order),
                          colour = factor(group),
                          shape = factor(treat_cont),
                          size =  0.8)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        axis.title.y=element_blank()) +
  geom_errorbarh(aes(xmax = NewObserver_up, xmin = NewObserver_low), height = .2, size = 0.1) +
  ggtitle("New observer") +
  geom_point(aes(x= NewObserver, y= reorder(species,order),
                 shape = factor(treat_cont),
                 size =  0.8),  colour = "gray79", size = 2,
             data = pmat[which(pmat$NewObserver_over == "zero"),]) +
  geom_errorbarh(aes(xmax = NewObserver_up, xmin = NewObserver_low), height = .2, size = 0.1,
                 colour = "gray79",
                 data = pmat[which(pmat$NewObserver_over == "zero"),])


## temp_min_scale.year_offset ---------------------------
i <- 
  ggplot(data = pmat, aes(x= temp_min_scale.year_offset, y= reorder(species,order),
                          colour = factor(group),
                          shape = factor(treat_cont))) +
  geom_vline(xintercept = 0) +
  geom_point(size = 2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        axis.title.y=element_blank()) +
  geom_errorbarh(aes(xmax = temp_min_scale.year_offset_up, xmin = temp_min_scale.year_offset_low), height = .2, size = 0.1) +
  ggtitle("Temperature and time") +
  geom_point(aes(x= temp_min_scale.year_offset, y= reorder(species,order),
                 shape = factor(treat_cont)),
             colour = "gray79", size = 2,
             data = pmat[which(pmat$temp_min_scale.year_offset_over == "zero"),]) +
  geom_errorbarh(aes(xmax = temp_min_scale.year_offset_up, xmin = temp_min_scale.year_offset_low), height = .2, size = 0.1,
                 colour = "gray79",
                 data = pmat[which(pmat$temp_min_scale.year_offset_over == "zero"),])

grid.arrange(a, b, c, d, e, f, g, h, i,
             nrow = 3)


## Proprtion of pop change: --------------
intercepts  <- read_csv(INTERCEPT_PATH)
temp_qunts  <- read_csv(TEMPQUANT_PATH)

pmat <- pmat %>% 
  mutate(intercept = pull(intercepts[ ,2]),
         temp1 = pull(temp_qunts[ ,2]),
         temp2 = pull(temp_qunts[ ,3]),
         temp3 = pull(temp_qunts[ ,4]),
         time = 20
  )

# estimates -----------
prop_tabX <- as_tibble(matrix(NA,nrow = nrow(pmat), ncol = 6)) %>% 
  mutate(species = pmat$species) %>% 
  relocate(species)
colnames(prop_tabX)[2:7] <- c("noinf1", "inf1", "noinf2", "inf2", "noinf3", "inf3")

for (i in 1:nrow(prop_tabX)){
  
  ifelse(!is.na(pmat$intercept[i]), b0 <- pmat$intercept[i], b0 <- 0)
  ifelse(!is.na(pmat$year_offset[i]), b1 <- pmat$year_offset[i], b1 <- 0)
  ifelse(!is.na(pmat$infoff[i]), b2 <- pmat$infoff[i], b2 <- 0)
  ifelse(!is.na(pmat$temp_min_scale[i]), b3 <- pmat$temp_min_scale[i], b3 <- 0)
  ifelse(!is.na(pmat$year_offset.infoff[i]), b4 <- pmat$year_offset.infoff[i], b4 <- 0)
  ifelse(!is.na(pmat$temp_min_scale.year_offset[i]), b5 <- pmat$temp_min_scale.year_offset[i], b5 <- 0)
  ifelse(!is.na(pmat$temp_min_scale.infoff[i]), b6 <- pmat$temp_min_scale.infoff[i], b6 <- 0)
  ifelse(!is.na(pmat$temp_min_scale.infoff.year_offset[i]), b7 <- pmat$temp_min_scale.infoff.year_offset[i], b7 <- 0)
  
  no_infes <- pmat[i,] %>% 
    mutate(prediction1 = exp(b0 + (b1 * time) + (b3 * temp1) + (b5 * year_offset * temp1)),
           prediction2 = exp(b0 + (b1 * time) + (b3 * temp2) + (b5 * year_offset * temp2)),
           prediction3 = exp(b0 + (b1 * time) + (b3 * temp3) + (b5 * year_offset * temp3)),
           HWA = 'infest'
    )
  
  infes <- pmat[i,] %>% 
    mutate(prediction1 = exp(b0 + (b1 * time) + (b2 * infoff) + (b3 * temp1) +
                               (b4 * time * infoff) + (b5 * time * temp1) +
                               (b6 * infoff * temp1) + (b7 * time * infoff * temp1)),
           prediction2 = exp(b0 + (b1 * time) + (b2 * infoff) + (b3 * temp2) +
                               (b4 * time * infoff) + (b5 * time * temp2) +
                               (b6 * infoff * temp2) + (b7 * time * infoff * temp2)),
           prediction3 = exp(b0 + (b1 * time) + (b2 * infoff) + (b3 * temp3) +
                               (b4 * time * infoff) + (b5 * time * temp3) +
                               (b6 * infoff * temp3) + (b7 * time * infoff * temp3)),
           HWA = 'no_infest'
    )
  prop_tabX$noinf1[i] <- no_infes$prediction1
  prop_tabX$inf1[i] <- infes$prediction1
  prop_tabX$noinf2[i] <- no_infes$prediction2
  prop_tabX$inf2[i] <- infes$prediction2
  prop_tabX$noinf3[i] <- no_infes$prediction3
  prop_tabX$inf3[i] <- infes$prediction3
  rm(no_infes, infes,
     b0, b1, b2, b3, b4, b5, b6, b7)
  
}

prop_tabX <- prop_tabX %>% 
  mutate(prop1 = inf1 / noinf1,
         prop2 = inf2 / noinf2,
         prop3 = inf3 / noinf3)

prop_tabX

## get percents attempt 2


import_pre <- function(xxx){
  read_csv(as.character(xxx)) %>% 
    filter(year == 20) %>% 
    dplyr::select(prediction, HWA) %>% 
    arrange(HWA) %>% 
    pull(prediction) %>% 
    t() %>% 
    as_tibble() %>%
    rename(not = V1, inf = V2) %>% 
    mutate(ratio = inf / not)
}

per1 <- rep(c("BHVI","BLBW","BTNW","HETH","MAWA","OVEN","RBNU","ACFL",
              "EAPH","WBNU","CERW","WOTH","SCTA","BLJA","BCCH","WEWA"), each = 3)
per15 <- c(rep("Hemlock species", 24),
           rep("Control species", 24))
per2 <- rep(c("t1", "t2", "t3"))
pers <- as_tibble(cbind(per1, per15, per2)) %>% 
  rename(sps = per1, temp = per2) %>% 
  mutate(name = as.character(glue("data/{sps}_{temp}preds.csv")),
         not = NA,
         infes = NA,
         ratio = NA)

for(i in 1:nrow(pers)){
  pers$not[i]   <- import_pre(pers$name[i])[1][[1]]
  pers$infes[i] <- import_pre(pers$name[i])[2][[1]]
  pers$ratio[i] <- import_pre(pers$name[i])[3][[1]]
}

pers <- pers %>% 
  mutate(piz = (infes*100)/not,
         piz_minus = 100 - (infes*100)/not,
         neg = piz_minus + 100)

pers2 <- pers %>% 
  dplyr::select(sps, per15, temp, not, infes, ratio) %>% 
  pivot_longer(`not`:`infes`, names_to = "infes_sta", values_to = "pop20")
pers2$infes_sta <- factor(pers2$infes_sta, levels = c("not","infes")) ## changes order in plot

pers2h <- pers2 %>% 
  filter(per15 == "Hemlock species") %>% 
  filter(!(sps == "OVEN")) 

pers2c <- pers2 %>% 
  filter(per15 == "Control species")

make_gradient <- function(deg = 45, n = 100, cols = blues9) {
  cols <- colorRampPalette(cols)(n + 1)
  rad <- deg / (180 / pi)
  mat <- matrix(data = rep(seq(0, 1, length.out = n) * cos(rad), n), 
                byrow = TRUE, ncol = n) +
    matrix(data = rep(seq(0, 1, length.out = n) * sin(rad), n),
           byrow = FALSE, ncol = n)
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  
  grid::rasterGrob(image = 
                     #scales::alpha(
                     mat, 
                   # 0.9), 
                   width = unit(1, "npc"),
                   height = unit(1,"npc"),
                   interpolate = TRUE)
}
#  grid::rasterGrob(
#     image = mat,
#     width = unit(1, "npc"),
#     height = unit(1, "npc"),
#     interpolate = TRUE
#  )
#}

g <- make_gradient(
  deg = 180, n = 500, cols = brewer.pal(9, "Spectral")
)

tempcols <- rep(c("blue", "blue","yellow", "yellow", "firebrick4", "firebrick4"),8)

p1 <- 
  ggplot(data = pers2c, aes(x = temp, y = sqrt(pop20), fill = infes_sta)) +
    #  annotation_custom(
    #    grob = g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
    geom_bar(position="dodge", stat='identity') +
    facet_wrap(~sps,ncol = 8) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          axis.title.x = element_text(margin = margin(t = 7))) +
    xlab("Temperature Quantile") + 
    ylab("(Abundance)^2") +
    scale_x_discrete(labels=c("t1" = "0.2",
                              "t2" = "0.5",
                              "t3" = "0.8")) +
    scale_fill_manual("legend",
                      values = c("not" = "darkolivegreen3", "infes" = "plum3"),
                      labels = c("Not infested", "Infested"))

p2 <- 
  ggplot(data = pers2h, aes(x = temp, y = sqrt(pop20), fill = infes_sta)) +
    geom_bar(position="dodge", stat='identity') +
    facet_wrap(~sps,ncol = 8) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.position="right",
          legend.justification="right",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-5,0,-5,-7),
          axis.title.x = element_blank()) +
    ylab("(Abundance)^2") +
    scale_x_discrete(labels=c("t1" = "0.2",
                              "t2" = "0.5",
                              "t3" = "0.8")) +
    scale_fill_manual("legend",
                      values = c("not" = "darkolivegreen3", "infes" = "plum3"),
                      labels = c("Not infested", "Infested"))
p1

p2

grid.arrange(p1, p2)

ggarrange(p1, p2, 
          ncol = 1, nrow = 2,
          heights = c(6,6), widths = c(10,10))

