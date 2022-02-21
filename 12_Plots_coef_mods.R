






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

yrmod <- pmat %>% 
  select(species, model, year) %>% 
  filter(!is.na(species)) %>% 
  saveRDS(file = "data/models_res/yrmod.rds")

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

order2 <- c(14, 13, 12, 11, 10, 9, 15,# "BHVI" "BLBW" "BTNW" "HETH" "MAWA" "OVEN" "RBNU" "ACFL"
            6, 3, 7, 1, 4, 8, 5, 2)   # "EAPH" "WBNU" "CERW" "WOTH" "SCTA" "BLJA" "REVI" "WEWA"

pmat2 <- pmat %>% 
  filter(species != "OVEN",
         species != "BCCH")
a1 <- ggplot(data = pmat2, aes(x= model, y= reorder(species, order2),
                               shape = factor(treat_cont),
                               size =  0.8),
             colour = "black",) +
  geom_point(size = 2) +
  theme_bw() +
  scale_y_discrete(limits = rev(levels(pmat2$species))) +
  scale_x_continuous(breaks = seq(1,11,1),
                     limits = c(1,11)) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        axis.title.y=element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Best model according to WAIC") +
  xlab("Model")

a2 <- ggplot(data = pmat2, aes(x= year, y= reorder(species, order2),
                               shape = factor(treat_cont),
                               size =  0.8),
             colour = "black",) +
  geom_point(size = 2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(2,16,1),
                     limits = c(2,16)) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        axis.title.y=element_blank(),
        axis.text.y = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("Delay in response to infestation") +
  xlab("Year")

a1
a2

grid.arrange(a1, a2, ncol = 2)

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

