library(tidyverse)



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

pmat <- pmat[1:15,]

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

pmat <- pmat %>% 
  select(species, treat_cont,
         infoff, infoff_low, infoff_up, infoff_over,
         year_offset.infoff, year_offset.infoff_low, year_offset.infoff_up, year_offset.infoff_over,
         temp_min_scale.infoff, temp_min_scale.infoff_low, temp_min_scale.infoff_up, temp_min_scale.infoff_over,
         temp_min_scale.infoff.year_offset, temp_min_scale.infoff.year_offset_low, temp_min_scale.infoff.year_offset_up, temp_min_scale.infoff.year_offset_over) %>%
  mutate(order = c(seq(2, 7, 1), 1, 
                   10, 14, 9, 16, 13, 8, 11, 15),
         species = fct_reorder(species, desc(order)))
pmat[pmat == 0] <- NA
pmat$treat_cont[1:7] <- 0
pmat$year_offset.infoff_up[8] <- 0

## short-term effect plot --------
ggplot(data = pmat, 
       aes(y= species, x = infoff)) +
  geom_vline(xintercept = 0,
             col = "gray43",
             linetype = "dashed",
             size = 1) +
  geom_errorbarh(aes(xmax = infoff_up, xmin = infoff_low, height = 0, color = infoff_over), size = 1) +
  geom_point(aes(shape = as.factor(treat_cont), color = infoff_over), size = 2.5) +
  theme_bw() +
  theme(#panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #legend.title = element_blank(),
        legend.position = "none",
        #legend.justification = "right",
        #legend.margin=margin(0,0,0,0),
        #legend.box.margin=margin(-5,0,-5,-7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(color = 'black'))+
        #legend.title.align = 0.5) +
  scale_color_manual(values = c("sig" = "black",
                                "zero" = "gray"))

## long-term effect plot --------
ggplot(data = pmat, 
       aes(y= species, x = year_offset.infoff)) +
  geom_vline(xintercept = 0,
             col = "gray43",
             linetype = "dashed",
             size = 1) +
  geom_errorbarh(aes(xmax = year_offset.infoff_up, xmin = year_offset.infoff_low, height = 0, color = year_offset.infoff_over), size = 1) +
  geom_point(aes(shape = as.factor(treat_cont), color = year_offset.infoff_over), size = 2.5) +
  theme_bw() +
  theme(#panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #legend.title = element_blank(),
    legend.position = "none",
    #legend.justification = "right",
    #legend.margin=margin(0,0,0,0),
    #legend.box.margin=margin(-5,0,-5,-7),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    axis.line.x.bottom = element_line(color = 'black'))+
  #legend.title.align = 0.5) +
  scale_color_manual(values = c("sig" = "black",
                                "zero" = "gray"))


## short-term effect and temperature plot --------
ggplot(data = pmat, 
       aes(y= species, x = temp_min_scale.infoff)) +
  geom_vline(xintercept = 0,
             col = "gray43",
             linetype = "dashed",
             size = 1) +
  geom_errorbarh(aes(xmax = temp_min_scale.infoff_up, xmin = temp_min_scale.infoff_low, height = 0, color = temp_min_scale.infoff_over), size = 1) +
  geom_point(aes(shape = as.factor(treat_cont), color = temp_min_scale.infoff_over), size = 2.5) +
  theme_bw() +
  theme(#panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #legend.title = element_blank(),
    legend.position = "none",
    #legend.justification = "right",
    #legend.margin=margin(0,0,0,0),
    #legend.box.margin=margin(-5,0,-5,-7),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    axis.line.x.bottom = element_line(color = 'black'))+
  #legend.title.align = 0.5) +
  scale_color_manual(values = c("sig" = "black",
                                "zero" = "gray"))

## long-term effect and temperature plot --------
ggplot(data = pmat, 
       aes(y= species, x = temp_min_scale.infoff.year_offset)) +
  geom_vline(xintercept = 0,
             col = "gray43",
             linetype = "dashed",
             size = 1) +
  geom_errorbarh(aes(xmax = temp_min_scale.infoff.year_offset_up, xmin = temp_min_scale.infoff.year_offset_low, 
                     height = 0, color = temp_min_scale.infoff.year_offset_over), size = 1) +
  geom_point(aes(shape = as.factor(treat_cont), color = temp_min_scale.infoff.year_offset_over), size = 2.5) +
  theme_bw() +
  theme(#panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #legend.title = element_blank(),
    legend.position = "none",
    #legend.justification = "right",
    #legend.margin=margin(0,0,0,0),
    #legend.box.margin=margin(-5,0,-5,-7),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    axis.line.x.bottom = element_line(color = 'black'))+
  #legend.title.align = 0.5) +
  scale_color_manual(values = c("sig" = "black",
                                "zero" = "gray"))





