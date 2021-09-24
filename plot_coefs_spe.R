## Script to import the results of coeficients of best models for all species affected by hemlock and controls

library(tidyverse)
library(glue)
library(gridExtra)

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
    select(prediction, HWA) %>% 
    arrange(HWA) %>% 
    pull(prediction) %>% 
    t() %>% 
    as_tibble() %>%
    rename(not = V1, inf = V2) %>% 
    mutate(ratio = inf / not)
}

per1 <- sort(rep(c("BHVI","BLBW","BTNW","HETH","MAWA","OVEN","RBNU","ACFL",
                   "EAPH","WBNU","CERW","WOTH","SCTA","BLJA","BCCH","WEWA"),3))
per15 <- c(rep("Hemlock Species", 24),
           rep("Control Species", 24))
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
  select(sps, per15, temp, not, infes, ratio) %>% 
  pivot_longer(`not`:`infes`, names_to = "Infes_sta", values_to = "pop20")

ggplot(data = pers2, aes(x = sps, y = ratio, fill = temp)) +
  geom_bar(position="dodge", stat='identity')
  

# library
library(likert) 

# Use a provided dataset
data(pisaitems) 
items28 <- pisaitems[, substr(names(pisaitems), 1, 5) == "ST24Q"] 

# Build plot
p <- likert(items28) 
plot(p)




















