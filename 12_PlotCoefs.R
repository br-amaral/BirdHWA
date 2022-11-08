# 12_PlotCoefs -----------------------------------------------------
# figure 2 and 4
# code to plot and creat table with info of coefficient estimates and best models
#
# INPUT
#   data/models_resnew/{spsr}/{spsr}_coefs.csv: individual species coefficients
#
# OUTPUT
#   data/coef_species.csv: species coefficient estimates table
#   data/modyear.csv: matrix with sps, control, treatment, offset and best model
#   data/coef_tab.csv: species coefficient estimates and CI, and best model and offset (table 4)
#   Figures/Fig4/coef_beta2.svg (figure 4)
#   Figures/Fig4/coef_beta3.svg (figure 4)
#   Figures/Fig4/coef_beta6.svg (figure 4)
#   Figures/Fig4/coef_beta7.svg (figure 4)
#   Figures/Fig2/mod_year.svg: plot with best model and year (figure 2)
#

# Load packages --------------------------------
library(glue)
library(tidyverse)
library(gridExtra)
library(fs)

SPSLIST_PATH <- path("data/src/sps_list.csv")
SPSPRED_PATH <- path("data/models_resnew/")

spslist <- read_csv(SPSLIST_PATH)

sps_coefs <- matrix(ncol= 10, nrow = 0)
colnames(sps_coefs) <- c('intercept','year_off_t','infoff','temp_min_scale',                   
                         'year_offset_infoff','year_offset_temp_min_scale','infoff_temp_min_scale',            
                         'year_offset_infoff_temp_min_scale','NewObserver','species')
sps_coefs <- as_tibble(sps_coefs, col_types = cols_only(
  species = col_character()))

modyear <- matrix(ncol= 3, nrow = 0)
colnames(modyear) <- c('species', 'mod', 'year')
modyear <- as_tibble(modyear)

for(i in 1:nrow(spslist)){
  spsr <- spslist[i,]
  modyr <- read_csv(glue("data/models_resnew/{spsr}/coefs_{spsr}.csv")) %>% 
    filter(betas == 'mod_year') %>% 
    rename(mod = mean,
           year = low,
           species = coef_name) %>% 
    select(species, mod, year)
  
  modyear <- rbind(modyear, modyr)
  
  coef <- read_csv(glue("data/models_resnew/{spsr}/coefs_{spsr}.csv")) %>% 
    mutate(mean = ifelse(is.na(low), NA, mean)) %>% 
    dplyr::select(mean,low,up) %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(species = pull(spsr)) 
  coef[,10] <- c("mean","low","up")
 
  colnames(coef)[1:10] <- c('intercept','year_off_t','infoff','temp_min_scale',                   
                   'year_offset_infoff','year_offset_temp_min_scale','infoff_temp_min_scale',            
                   'year_offset_infoff_temp_min_scale','NewObserver',"esti")
  
  sps_coefs <- rbind(sps_coefs,coef)
  rm(spsr)
}

sps_coefs <- sps_coefs %>%
  relocate(species) %>% 
  mutate(control = 0)

sps_coefs_overlap <- sps_coefs[,1:10]
sps_coefs_overlap[,2:10] <- NA

sps_coefs$control[((7*3)+1):(nrow(sps_coefs))] <- 1

modyear <- modyear %>% 
  mutate(control = 0)

modyear$control <- c(rep(0,7),rep(1,7))

# create a zero and one variable to indicate if it overlaps zero
rowsindex <- seq(1, 40, 3)

for(k in 1:length(rowsindex)){  ## loop in the rows - every three (mean, up and low)
  
  j <- rowsindex[k]
  
  for(i in 2:10){  ## loop in the columns (betas)
    
    if(is.na(between(0, sps_coefs[j+1,i], sps_coefs[j+2,i]))) {
      sps_coefs_overlap[c(j, j+1, j+2),i] <- NA
      
    } else {
      
      if(between(0, sps_coefs[j+1,i], sps_coefs[j+2,i]) == FALSE) {
        sps_coefs_overlap[c(j, j+1, j+2),i] <- "sig"
      }
      
      if(between(0, sps_coefs[j+1,i], sps_coefs[j+2,i]) == TRUE) {
        sps_coefs_overlap[c(j, j+1, j+2),i] <- "zero"
      }
    }
    
  }
}

sps_coefs_overlap2 <- sps_coefs_overlap %>% 
  distinct()
colnames(sps_coefs_overlap2)[2:10] <- paste0(colnames(sps_coefs_overlap2)[2:10],"_over")

sps_coefs_mean <- sps_coefs %>% 
  filter(esti == "mean")

sps_coefs_up <- sps_coefs %>% 
  filter(esti == "up") %>% 
  select(-c("esti","control"))
colnames(sps_coefs_up)[2:10] <- paste0(colnames(sps_coefs_up)[2:10],"_up")

sps_coefs_low <- sps_coefs %>% 
  filter(esti == "low") %>% 
  select(-c("esti","control"))
colnames(sps_coefs_low)[2:10] <- paste0(colnames(sps_coefs_low)[2:10],"_low")

sps_coefs2 <- left_join(sps_coefs_mean, sps_coefs_low, by = "species")
sps_coefs2 <- left_join(sps_coefs2, sps_coefs_up, by = "species")
sps_coefs2 <- left_join(sps_coefs2, sps_coefs_overlap2, by = "species")

write_csv(sps_coefs2, file = "data/coef_species.csv")

order <- rev(c("ACFL", "BHVI", "BLBW", "BTNW", "HETH", "MAWA", "RBNU",
                   "BLJA", "CERW", "EAPH", "REVI", "SCTA", "WBNU", "WOTH"))

sps_coefs2$species <- factor(sps_coefs2$species, levels = order)

## short-term effect plot --------
svg(glue("Figures/Fig4/coef_beta2.svg"), 
    width = 3.5, height = 3)
ggplot(data = sps_coefs2, 
       aes(y= species, x = infoff)) +
  geom_vline(xintercept = 0,
             col = "gray43",
             linetype = "dashed",
             size = 1) +
  geom_errorbarh(aes(xmax = infoff_up, xmin = infoff_low, height = 0, color = infoff_over), size = 1) +
  geom_point(aes(shape = as.factor(control), color = infoff_over), size = 2.5) +
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
dev.off()

## long-term effect plot --------
svg(glue("Figures/Fig4/coef_beta3.svg"), 
    width = 3.5, height = 3)
ggplot(data = sps_coefs2, 
       aes(y= species, x = year_offset_infoff)) +
  geom_vline(xintercept = 0,
             col = "gray43",
             linetype = "dashed",
             size = 1) +
  geom_errorbarh(aes(xmax = year_offset_infoff_up, xmin = year_offset_infoff_low, height = 0, 
                     color = year_offset_infoff_over), size = 1) +
  geom_point(aes(shape = as.factor(control), color = year_offset_infoff_over), size = 2.5) +
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
dev.off()

## short-term effect and temperature plot --------
svg(glue("Figures/Fig4/coef_beta6.svg"), 
    width = 3.5, height = 3)
ggplot(data = sps_coefs2, 
       aes(y= species, x = infoff_temp_min_scale)) +
  geom_vline(xintercept = 0,
             col = "gray43",
             linetype = "dashed",
             size = 1) +
  geom_errorbarh(aes(xmax = infoff_temp_min_scale_up, xmin = infoff_temp_min_scale_low, height = 0, 
                     color = infoff_temp_min_scale_over), size = 1) +
  geom_point(aes(shape = as.factor(control), color = infoff_temp_min_scale_over), size = 2.5) +
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
dev.off()

## long-term effect and temperature plot --------
svg(glue("Figures/Fig4/coef_beta7.svg"), 
    width = 3.5, height = 3)
ggplot(data = sps_coefs2, 
       aes(y= species, x = year_offset_infoff_temp_min_scale)) +
  geom_vline(xintercept = 0,
             col = "gray43",
             linetype = "dashed",
             size = 1) +
  geom_errorbarh(aes(xmax = year_offset_infoff_temp_min_scale_up, xmin = year_offset_infoff_temp_min_scale_low, 
                     height = 0, color = year_offset_infoff_temp_min_scale_over), size = 1) +
  geom_point(aes(shape = as.factor(control), color = year_offset_infoff_temp_min_scale_over), size = 2.5) +
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
dev.off()

## best model and year plots
modyear$species <- factor(modyear$species, levels = order)

a <- ggplot(data = modyear, aes(x= mod, y= species,
                        shape = factor(control),
                        size =  0.8)) +
  geom_point(size = 2) +
  theme_bw() +
  scale_x_continuous(limits = c(1,11),
                   breaks = seq(1,11,1)) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.grid.minor = element_blank()) 

b <- ggplot(data = modyear, aes(x= year, y= species,
                                shape = factor(control),
                                size =  0.8)) +
  geom_point(size = 2) +
  theme_bw() +
  scale_x_continuous(limits = c(1,16),
                     breaks = seq(1,16,1)) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        #axis.text.y = element_blank(),
        panel.grid.minor = element_blank()) 

svg(glue("Figures/Fig2/mod_year.svg"), 
    width = 8, height = 3)
grid.arrange(a,b, ncol = 2)
dev.off()

#write_csv(modyear, file = "data/modyear.csv")
 
## table
tableB <- left_join(modyear, sps_coefs2, by = c("species","control"))
tableB <- tableB[,1:32]
tableB <- tableB %>% 
  dplyr::select(-esti) %>% 
  dplyr::select(-control) %>% 
  relocate(species, mod, year,                       
           intercept,intercept_low, intercept_up,
           year_off_t, year_off_t_low, year_off_t_up,
           infoff, infoff_low, infoff_up,
           temp_min_scale, temp_min_scale_low, temp_min_scale_up,
           year_offset_infoff, year_offset_infoff_low,year_offset_infoff_up,
           year_offset_temp_min_scale, year_offset_temp_min_scale_low, year_offset_temp_min_scale_up,
           infoff_temp_min_scale, infoff_temp_min_scale_low, infoff_temp_min_scale_up,
           year_offset_infoff_temp_min_scale, year_offset_infoff_temp_min_scale_low, year_offset_infoff_temp_min_scale_up,
           NewObserver, NewObserver_low, NewObserver_up)

tableB <- tableB[match(rev(order), tableB$species),]

write_csv(tableB, file='Figures/Tables/Table4/coef_tab.csv')
