library(ggplot2)
library(tidyverse)
library(glue)

SPECIES_DATA_PATH <- "data/src/sps_list.csv"
sps_list <- read_csv(SPECIES_DATA_PATH)

# estimates of the full model -----------
# source(13.5_plot_full_mod_dat)
a1 <- read_rds(glue("data/models_res/{sps_list[1,]}/{sps_list[1,]}_fullmodel.rds"))
s1 <- read_rds(glue("data/models_res/{sps_list[2,]}/{sps_list[2,]}_fullmodel.rds"))
c1 <- read_rds(glue("data/models_res/{sps_list[3,]}/{sps_list[3,]}_fullmodel.rds"))
d1 <- read_rds(glue("data/models_res/{sps_list[4,]}/{sps_list[4,]}_fullmodel.rds"))
e1 <- read_rds(glue("data/models_res/{sps_list[5,]}/{sps_list[5,]}_fullmodel.rds"))
f1 <- read_rds(glue("data/models_res/{sps_list[7,]}/{sps_list[7,]}_fullmodel.rds"))
g1 <- read_rds(glue("data/models_res/{sps_list[8,]}/{sps_list[8,]}_fullmodel.rds"))
h1 <- read_rds(glue("data/models_res/{sps_list[9,]}/{sps_list[9,]}_fullmodel.rds"))
k1 <- read_rds(glue("data/models_res/{sps_list[10,]}/{sps_list[10,]}_fullmodel.rds"))
l1 <- read_rds(glue("data/models_res/{sps_list[11,]}/{sps_list[11,]}_fullmodel.rds"))
m1 <- read_rds(glue("data/models_res/{sps_list[12,]}/{sps_list[12,]}_fullmodel.rds"))
n1 <- read_rds(glue("data/models_res/{sps_list[13,]}/{sps_list[13,]}_fullmodel.rds"))
o1 <- read_rds(glue("data/models_res/{sps_list[14,]}/{sps_list[14,]}_fullmodel.rds"))
#p1 <- read_rds(glue("data/models_res/{sps_list[15,]}/{sps_list[15,]}_fullmodel.rds"))
q1 <- read_rds(glue("data/models_res/{sps_list[16,]}/{sps_list[16,]}_fullmodel.rds"))
r1 <- read_rds(glue("data/models_res/{sps_list[17,]}/{sps_list[17,]}_fullmodel.rds"))

parnames <- rownames(a1$summary.fixed)

a2 <- a1$summary.fixed %>% 
  select(mean) %>% 
  as_tibble() %>% 
  mutate(par = parnames,
         species = sps_list[1,]) 

s2 <- s1$summary.fixed %>% 
  select(mean) %>% 
  as_tibble() %>% 
  mutate(par = parnames,
         species = sps_list[2,]) 

c2 <- c1$summary.fixed %>% 
  select(mean) %>% 
  as_tibble() %>% 
  mutate(par = parnames,
         species = sps_list[3,])

d2 <- d1$summary.fixed %>% 
  select(mean) %>% 
  as_tibble() %>% 
  mutate(par = parnames,
         species = sps_list[4,]) 

e2 <- e1$summary.fixed %>% 
  select(mean) %>% 
  as_tibble() %>% 
  mutate(par = parnames,
         species = sps_list[5,]) 

f2 <- f1$summary.fixed %>% 
  select(mean) %>% 
  as_tibble() %>% 
  mutate(par = parnames,          
         species = sps_list[7,]) 

g2 <- g1$summary.fixed %>% 
  select(mean) %>% 
  as_tibble() %>% 
  mutate(par = parnames,          
         species = sps_list[8,])

h2 <- h1$summary.fixed %>% 
  select(mean) %>% 
  as_tibble() %>% 
  mutate(par = parnames,          
         species = sps_list[9,]) 

k2 <- k1$summary.fixed %>% 
  select(mean) %>% 
  as_tibble() %>% 
  mutate(par = parnames,          
         species = sps_list[10,]) 

l2 <- l1$summary.fixed %>% 
  select(mean) %>% 
  as_tibble() %>% 
  mutate(par = parnames,          
         species = sps_list[11,]) 

m2 <- m1$summary.fixed %>% 
  select(mean) %>% 
  as_tibble() %>% 
  mutate(par = parnames,          
         species = sps_list[12,])

n2 <- n1$summary.fixed %>% 
  select(mean) %>% 
  as_tibble() %>% 
  mutate(par = parnames,          
         species = sps_list[13,]) 

o2 <- o1$summary.fixed %>% 
  select(mean) %>% 
  as_tibble() %>% 
  mutate(par = parnames,          
         species = sps_list[14,]) 

#p2 <- p1$summary.fixed %>% 
#  select(mean) %>% 
#  as_tibble() %>% 
#  mutate(par = parnames,          
#         species = sps_list[15,])

q2 <- q1$summary.fixed %>% 
  select(mean) %>% 
  as_tibble() %>% 
  mutate(par = parnames,          
         species = sps_list[16,]) 

r2 <- r1$summary.fixed %>% 
  select(mean) %>% 
  as_tibble() %>% 
  mutate(par = parnames,          
         species = sps_list[17,]) 

full_pred <- function(tab){
  
  species <- as.character(tab$species[1,])
  
  tab2 <- tab %>% pivot_wider(names_from = par, values_from = mean)
  colnames(tab2) <- c("species","intercept","year_offset","infoff","NewObserver",
                      "temp_min_scale","year_offset.infoff","year_offset.temp_min_scale",
                      "infoff.temp_min_scale","year_offset.infoff.temp_min_scale")
  
  ifelse(!is.na(tab2$intercept), b0 <- tab2$intercept, b0 <- 0)
  ifelse(!is.na(tab2$year_offset), b1 <- tab2$year_offset, b1 <- 0)
  ifelse(!is.na(tab2$infoff), b2 <- tab2$infoff, b2 <- 0)
  ifelse(!is.na(tab2$temp_min_scale), b3 <- tab2$temp_min_scale, b3 <- 0)
  ifelse(!is.na(tab2$year_offset.infoff), b4 <- tab2$year_offset.infoff, b4 <- 0)
  ifelse(!is.na(tab2$year_offset.temp_min_scale), b5 <- tab2$year_offset.temp_min_scale, b5 <- 0)
  ifelse(!is.na(tab2$infoff.temp_min_scale), b6 <- tab2$infoff.temp_min_scale, b6 <- 0)
  ifelse(!is.na(tab2$year_offset.infoff.temp_min_scale), b7 <- tab2$year_offset.infoff.temp_min_scale, b7 <- 0)
  
  preds <- readRDS(glue("data/preds/preds_{species}.rds")) %>% 
    mutate(species = species) %>% 
    filter(year == 20)
  preds$prop <- preds$prediction <- NA
  
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
    arrange(temp)
  
  for(j in seq(from=1, to=nrow(sps_preds2), by=2)) {  # infested divided by not_infested
    sps_preds2$prop[j] <- sps_preds2$prop[j+1] <- log(sps_preds2$prediction[j]/sps_preds2$prediction[j+1])
  }
  
  sps_preds3 <- sps_preds2 %>% 
    filter(HWA == "infest")   ## get only a copy from prop
  return(sps_preds3)
}

a3 <- full_pred(a2)
s3 <- full_pred(s2)
c3 <- full_pred(c2)
d3 <- full_pred(d2)
e3 <- full_pred(e2)
f3 <- full_pred(f2)
g3 <- full_pred(g2)
h3 <- full_pred(h2)
k3 <- full_pred(k2)
l3 <- full_pred(l2)
m3 <- full_pred(m2)
n3 <- full_pred(n2)
o3 <- full_pred(o2)
#p3 <- full_pred(p2)
q3 <- full_pred(q2)
r3 <- full_pred(r2)

master_full <- rbind(a3,s3,c3,d3,e3,f3,g3,h3,k3,l3,m3,n3,o3,q3,r3)


# sensitivity    -----------
sensi_pro <- function(species) {
  COEF_PATH_SENSI <- glue("data/models_res/{species}/sensi/coefs_{species}.rds")
  COEF_PATH <- glue("data/models_res/{species}/{species}_fullmodel.rds")
  
  plot_tib <- readRDS(COEF_PATH_SENSI)
  
  plot_tib2 <- readRDS(COEF_PATH)
  plot_tib2 <- plot_tib2$summary.fixed
  plot_tib2$par2 <- c("B0", "B1", "B2", "B8", "B4", "B3", "B5", "B6", "B7")
  
  colnames(plot_tib2)[1] <- "value"
  colnames(plot_tib2)[3] <- "low1"
  colnames(plot_tib2)[5] <- "up1"
  
  ggplot(data = plot_tib, aes(x = par2, y = mean)) +
    #geom_jitter(col = "gray") +
    geom_boxplot() +
    #coord_flip()
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="none") +
    ylab("Estimates") +
    xlab("Coefficients") +
    geom_point(data = plot_tib2, aes(x = par2, y = value), col = "red") +
    geom_segment(data = plot_tib2, aes(x = par2, y = low1, xend = par2, yend = up1), col = "red") +
    ggtitle("Sensitivity Analysis")
  
  # pop change
  plot_tib_per <- plot_tib %>% 
    arrange(RouteId) %>% 
    pivot_wider(names_from = par2, values_from = mean)
  for(i in 1:nrow(plot_tib_per)){
    for(j in 5:13){
      if(is.na(plot_tib_per[i,j])) {plot_tib_per[i,j] <- 0}
    }
  }
  
  # remove NA columns
  plot_tib_per$value <- NA
  
  for(i in 1:nrow(plot_tib_per)){
    plot_tib_per$value[i] <- sum(plot_tib_per[i,5:13])
  }
  
  plot_tib_per <- plot_tib_per %>% 
    select(RouteId, low, up, par, value)
  
  routes <- sort(unique(plot_tib_per$RouteId))
  
  # template for predictions
  preds <- readRDS(glue("data/preds/preds_{species}.rds")) %>% 
    filter(year == 20) %>% 
    select(!( year_off_t))
  preds$prop <- preds$prediction <- NA
  preds$species <- as.character(species)
  
  routes_change <- as.data.frame(matrix(NA, ncol = 4, nrow = length(routes)))
  colnames(routes_change) <- c("RouteId", "t1", "t2", "t3")
  routes_change$RouteId <- routes
  
  for(i in 1:length(routes)){
    
    route <- routes[i]
    
    pars_tib <- plot_tib_per %>% 
      filter(RouteId == route) %>% 
      pivot_wider(names_from = par, values_from = value) %>% 
      select(-c(RouteId, low, up))
    
    pars_tib <- colSums(pars_tib, na.rm= T)
    
    pars_tib <- as.data.frame(t(pars_tib))
    
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
      arrange(temp)
    
    for(j in seq(from=1, to=nrow(sps_preds2), by=2)) {  # infested divided by not_infested
      sps_preds2$prop[j] <- sps_preds2$prop[j+1] <- log(sps_preds2$prediction[j]/sps_preds2$prediction[j+1])
    }
    
    sps_preds3 <- sps_preds2 %>% 
      filter(HWA == "infest")   ## get only a copy from prop
    
    routes_change[i,2:4] <- t(sps_preds3$prop)
    print(i) 
  }
  routes_change2 <- routes_change %>%
    pivot_longer(`t1`:`t3`, names_to = "temp", values_to = "prop",
                 names_transform = list(Year = as.numeric),
                 values_ptypes = list(Infested = logical())) 
  routes_change2$species <- as.character(species)
  
  assign(glue("sen_prop_{species}"), routes_change2)
  
  return(get(glue("sen_prop_{species}")))
}

a <- sensi_pro(sps_list[1,])
b <- sensi_pro(sps_list[2,])
c <- sensi_pro(sps_list[3,])
d <- sensi_pro(sps_list[4,])
e <- sensi_pro(sps_list[5,])
f <- sensi_pro(sps_list[7,])
g <- sensi_pro(sps_list[8,])
h <- sensi_pro(sps_list[9,]) 
k <- sensi_pro(sps_list[10,])
l <- sensi_pro(sps_list[11,])
m <- sensi_pro(sps_list[12,])
n <- sensi_pro(sps_list[13,])
o <- sensi_pro(sps_list[14,]) 
#p <- sensi_pro(sps_list[15,])
q <- sensi_pro(sps_list[16,])
r <- sensi_pro(sps_list[17,]) 

master_pro <- rbind(a,b,c,d,e,f,g,h,k,l,m,n,o,q,r)

master_full <- master_pro %>% 
  filter(RouteId == "full")

master_pro <- master_pro %>% 
  filter(RouteId != "full")

master_full$temp2 <- NA
for(i in 1:nrow(master_full)){
  if(master_full$temp[i] == "t1") {master_full$temp2[i] <- "tt1" }
  if(master_full$temp[i] == "t2") {master_full$temp2[i] <- "tt2" }
  if(master_full$temp[i] == "t3") {master_full$temp2[i] <- "tt3" }
}

master_pro2 <- master_pro %>% 
  select(RouteId, prop, species, temp) %>% 
  unite(sps_temp, species:temp, remove = FALSE)

temp_order <- as_tibble(matrix(c("t1","t2","t3",1,2,3), nrow = 3)) %>% 
  rename(temp = V1,
         orde = V2)

master_pro2 <- left_join(master_pro2, temp_order, by = "temp") %>% 
  arrange(orde)

master_full2 <- left_join(master_full, temp_order, by = "temp") %>% 
  unite(sps_temp, c(species,temp), remove = FALSE)

ggplot(data = master_pro2, aes(y= sps_temp, x = prop)) +
  geom_vline(xintercept = 0,
             col = "gray43",
             linetype = "dotted",
             size = 1) +
  geom_point(aes(shape = temp, color = temp), size = 2) +
  geom_boxplot() + 
  #facet_wrap(~temp) +
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
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5)) +
  #scale_x_continuous(breaks = seq(-3, 1, 0.5),
  #                   limits = c(-3, 1, 0.5)) +
  geom_point(data = master_full2, aes(y= sps_temp, x = prop,
                                     shape = temp2, color = temp2), size = 3) +
  scale_shape_manual(values=c("tt1" = 21,
                              "tt2" = 24,
                              "tt3" = 22,
                              "t1" = 16,
                              "t2" = 17,
                              "t3" = 15)) +
  scale_fill_manual(values=c("olivedrab4",
                             "violetred",
                             "darkorange3")) +
  scale_color_manual(values = c("t1" = "olivedrab3",
                                "t2" = "palevioletred2",
                                "t3" = "tan1",
                                "tt1" = "black",
                                "tt2" = "black",
                                "tt3" = "black"),
                     labels = c("t1" = "0.2",
                                "t2" = "0.5",
                                "t3" = "0.8"),
                     name = "Temperature\nQuantiles") +
  labs(title="Sensitivity analysis")

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

ggplot(data = master_pro3, aes(y= sps_temp, x = prop)) +
  geom_vline(xintercept = 0,
             col = "gray43",
             linetype = "dotted",
             size = 1) +
  #geom_point(aes(shape = temp, color = temp), size = 2) +
  geom_boxplot() + 
  # facet_wrap(~temp) +
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
  labs(title="Sensitivity analysis") +
  coord_flip() 

# permutation ----------
pred_per_sps <- function(species){
  TEMPQUANT_PATH <- glue("data/tempquant.csv")
  COEF_PERM_PATH <- glue("data/models_res/{species}/perm/coefs_{species}.rds", sep= "")
  
  coefs <- read_rds(COEF_PERM_PATH)  
  temp_qunts <- read_csv(TEMPQUANT_PATH)
  
  temp_qunts2 <- temp_qunts %>% 
    as_tibble() %>% 
    rename(Species = species, t1 = ...2, t2 = ...3, t3 = ...4) %>% 
    dplyr::filter(trimws(Species) == trimws(species))
    
  coefs2 <- coefs %>%
    select(par, mean) %>% 
    group_by(par) %>%
    mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = par, values_from = mean) %>%
    select(-row)
  
  pmat <- coefs2 %>% 
    mutate(temp1 = pull(temp_qunts2[ ,2]),
           temp2 = pull(temp_qunts2[ ,3]),
           temp3 = pull(temp_qunts2[ ,4]),
           time = 20
    )
  
  # estimates -----------
  prop_tabX <- as_tibble(matrix(NA,nrow = nrow(pmat), ncol = 6))
  colnames(prop_tabX) <- c("noinf1", "inf1", "noinf2", "inf2", "noinf3", "inf3")
  
  for (i in 1:nrow(prop_tabX)){
    
    ifelse(!is.na(pmat$intercept[i]), b0 <- pmat$intercept[i], b0 <- 0)
    ifelse(!is.na(pmat$year_offset[i]), b1 <- pmat$year_offset[i], b1 <- 0)
    ifelse(!is.na(pmat$infoff[i]), b2 <- pmat$infoff[i], b2 <- 0)
    ifelse(!is.na(pmat$temp_min_scale[i]), b3 <- pmat$temp_min_scale[i], b3 <- 0)
    ifelse(!is.na(pmat$year_offset.infoff[i]), b4 <- pmat$year_offset.infoff[i], b4 <- 0)
    ifelse(!is.na(pmat$year_offset.temp_min_scale[i]), b5 <- pmat$year_offset.temp_min_scale[i], b5 <- 0)
    ifelse(!is.na(pmat$infoff.temp_min_scale[i]), b6 <- pmat$infoff.temp_min_scale[i], b6 <- 0)
    ifelse(!is.na(pmat$year_offset.infoff.temp_min_scale[i]), b7 <- pmat$year_offset.infoff.temp_min_scale[i], b7 <- 0)
    
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
    mutate(t1 = log(noinf1/inf1),
           t2 = log(noinf2/inf2),
           t3 = log(noinf3/inf3),
           Species = species)
  
  prop_tabX2 <- prop_tabX %>% 
    pivot_longer(`t1`:`t3`, names_to = "temp", values_to = "pop202") %>% 
    select(Species, temp, pop202) %>% 
    mutate(sps_temp = glue("{Species}_{temp}", remove = FALSE))
  
  temp_order <- as_tibble(matrix(c("t1","t2","t3",1,2,3), nrow = 3)) %>% 
    rename(temp = V1,
           orde = V2)
  
  prop_tabX3 <- left_join(prop_tabX2, temp_order, by = "temp") %>% 
    arrange(orde)
  
  return(prop_tabX3)
  
}

# add real estimate on top
master_full_per <- master_full %>% 
  mutate(sps_temp = glue("{species}_{temp}"),
         temp2 = glue("t{temp}"))

per1 <- pred_per_sps(trimws(sps_list[1,]))
per2 <- pred_per_sps(trimws(sps_list[2,]))
per3 <- pred_per_sps(trimws(sps_list[3,]))
per4 <- pred_per_sps(trimws(sps_list[4,]))
per5 <- pred_per_sps(trimws(sps_list[5,]))
#6
per7 <- pred_per_sps(trimws(sps_list[7,]))
per8 <- pred_per_sps(trimws(sps_list[8,]))
per9 <- pred_per_sps(trimws(sps_list[9,]))
per10 <- pred_per_sps(trimws(sps_list[10,]))
per11 <- pred_per_sps(trimws(sps_list[11,]))
per12 <- pred_per_sps(trimws(sps_list[12,]))
per13 <- pred_per_sps(trimws(sps_list[13,]))
per14 <- pred_per_sps(trimws(sps_list[14,]))
#per15 <- pred_per_sps(trimws(sps_list[15,]))
per16 <- pred_per_sps(trimws(sps_list[16,]))
per17 <- pred_per_sps(trimws(sps_list[17,]))

prop_tabX3 <- rbind(per1, per2, per3, per4, per5, per7, per8, per9, 
                    per10, per11, per12, per13, per14, per16, per17) %>% 
  arrange(match(Species, master_full2$sps_temp)) %>% 
  mutate(orde = seq(1:nrow(prop_tabX3)))

(pl1 <- ggplot(data = prop_tabX3, aes(y = reorder(sps_temp,desc(orde)), x = pop202)) +
    geom_point(aes(shape = temp), colour = "grey", size = 2) +
    geom_vline(xintercept = 0,
               col = "gray43",
               linetype = "dotted",
               size = 1) +
    geom_point(data = master_full_per, 
               aes(y = sps_temp, x = prop,
                   shape = temp), colour = "black", size = 2) +
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
          plot.title = element_text(hjust = 0.5),
          legend.title.align = 0.5) +
    scale_x_continuous(breaks = seq(-3, 1, 0.5),
                       limits = c(-3, 1, 0.5)) +
    scale_color_manual(values = c("t1" = "blue4",
                                  "t2" = "violetred",
                                  "t3" = "darkorange3"),
                       labels = c("t1" = "0.2",
                                  "t2" = "0.5",
                                  "t3" = "0.8"),
                       name = "Temperature\nQuantiles")) +
  ggtitle("Permutation")


ggplot(data = prop_tabX3, aes(y = reorder(sps_temp,desc(orde)), x = pop202)) +
  geom_vline(xintercept = 0,
             col = "gray43",
             linetype = "dotted",
             size = 1) +
  #geom_point(aes(shape = temp, color = temp), size = 2) +
  geom_boxplot() + 
  # facet_wrap(~temp) +
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
        legend.title.align = 0.5,
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #scale_x_continuous(breaks = seq(-3, 1, 0.5),
  #                   limits = c(-3, 1, 0.5)) +
  geom_point(data = master_full_per, aes(y = sps_temp, x = prop,
                                      shape = temp2, color = temp2
                                      ), size = 3) +
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
  labs(title="Permutation analysis") +
  coord_flip() +
  scale_y_discrete(limits=rev)







