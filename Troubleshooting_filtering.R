## primeiro erro - para nao infestadas, ultimo ano nao eh zero, mas sim zero menos o offset!
#          BIRDx$year_offset[i] <- BIRDx$Year[i] - as.numeric(off_noin)
#          BIRDx$year_offset[i] <- BIRDx$Year[i] - as.numeric(off_noin) + off - 1

## MAIS OFFSERT JUMENTA
## infested se for maior que OFFSET, nao maior que zero
#          infoff = ifelse(year_offset <= 0, 0, ifelse(year_offset > 0, 1, NA)))
#          infoff = ifelse(year_offset < off, 0, ifelse(year_offset >= off, 1, NA)))
# aqui tbm:
#          mutate(year_offset = ifelse(YearInfested != 0, Year - YearInfested + off , 0),


library(INLA)
library(tidyverse)
library(glue)
library(gridExtra)

set.seed(10)

species <- "ACFL"

SPECIES_DATA_PATH <- "data/src/sps_list.csv"
source("5_formulasModels.R")
sps_list <- read_csv(SPECIES_DATA_PATH)
hex.adj <- paste0(getwd(),"/data/hexmap.graph")
DATA_PATH <- glue("data")
RESULT_PATH <- glue(DATA_PATH, "models_res")

offsets <- seq(2,16,1)

i <- j <- 1
formula <- formulas[[i]]
off <- offsets[j]
SPECIES_MOD_DAT <- glue("data/species/{species}.rds")
BIRDtab <- readRDS(SPECIES_MOD_DAT)

BIRDx <- BIRDtab #%>% 
BIRDx <- BIRDx %>% 
  # year_offset is standardizing yrhwa to the offset (years after infestation to the impact)
  mutate(year_offset = ifelse(YearInfested != 0, Year - YearInfested + off , 0),
         # infoff: 'infested' route according to the delay in the effect (offset)
         infoff = ifelse(year_offset < off, 0, ifelse(year_offset >= off, 1, NA)))

rout_notinf <- BIRDx %>% 
  select(RouteId, Year, YearInfested, Infested) %>% 
  filter(YearInfested == 0) %>% 
  distinct() %>% 
  group_by(RouteId) %>% 
  mutate(maxYear = max(Year)) %>% 
  select(RouteId, maxYear) %>% 
  distinct()

## if a route was never infested, year_offset is 'equal' to the last year it was sampled
for(i in 1:nrow(BIRDx)){
  if(BIRDx$YearInfested[i] == 0){
    off_noin <- rout_notinf[which(rout_notinf$RouteId == BIRDx$RouteId[i]), 2]
    BIRDx$year_offset[i] <- BIRDx$Year[i] - as.numeric(off_noin) + off - 1
  }
}

table(teste$RouteId, teste$Infested)

teste <- BIRDx # %>% filter(RouteId %in% c(82034, 88007, 88005, 90041, 82029))
unique(teste$RouteId)

b <- 82034  ## infested - -40 ate 10
b <- 88007  ## infested - -23 ate 25
b <- 88005  ## infested - -23 ate 25
b <- 90041  ## not infested - -41 ate 0
b <- 82029  ## infested - -45 ate 5

teste0 <- BIRDx %>% filter(RouteId == b)  # not infested
# teste <- teste0
unique(teste0$RouteId)
#    ggplot(teste0, aes(x = year_offset, y = SpeciesTotal, colour = Infested)) + geom_point() + geom_smooth(aes(fill = Infested))

## no filters ----------------------------
ggplot(teste, aes(x = year_offset, y = SpeciesTotal, colour = Infested)) +
  geom_point() +
  geom_smooth(aes(fill = Infested)) +
  ggtitle("no filter") +
  theme_bw()

table(teste$year_offset, teste$infoff)

## todos os filtros ----------------------------
BIRDx1 <- teste %>% 
  filter(YearInfested != 0,
         year_offset > -20 & year_offset < 20) %>% 
  group_by(RouteId) %>% 
  group_split()

for(i in 1:length(BIRDx1)){
  a <- BIRDx1[[i]]
  maxi <- max(a$year_offset)
  if(maxi < 10) {BIRDx1[[i]] <- NULL }
}

BIRDx2_1 <- data.table::rbindlist(BIRDx1) %>% 
  as_tibble()

ggplot(BIRDx2_1, aes(x = year_offset, y = SpeciesTotal, colour = Infested)) +
  geom_point() +
  geom_smooth(aes(fill = Infested)) +
  ggtitle("all filters") +
  theme_bw()

table(BIRDx2_1$year_offset, BIRDx2_1$infoff)

## only -20 and +20 filter   ----------------------------
BIRDx2_2 <- teste %>% 
  filter(year_offset > -20 & year_offset < 20)

ggplot(BIRDx2_2, aes(x = year_offset, y = SpeciesTotal, colour = Infested)) +
  geom_point() +
  geom_smooth(aes(fill = Infested)) +
  ggtitle("-20 e +20 filtro") +
  theme_bw()

table(BIRDx2_2$year_offset, BIRDx2_2$infoff)

## only infested  ----------------------------
BIRDx2_3 <- teste %>% 
  filter(YearInfested != 0)

ggplot(BIRDx2_3, aes(x = year_offset, y = SpeciesTotal, colour = Infested)) +
  geom_point() +
  geom_smooth(aes(fill = Infested)) +
  ggtitle("only infested") +
  theme_bw()

table(BIRDx2_3$year_offset, BIRDx2_3$infoff)   

## only infested for 9 years   ----------------------------
BIRDx1 <- teste %>% 
  group_by(RouteId) %>% 
  group_split()

for(i in 1:length(BIRDx1)){
  a <- BIRDx1[[i]]
  maxi <- max(a$year_offset)
  if(maxi < 10) {BIRDx1[[i]] <- NULL }
}

BIRDx2_4 <- data.table::rbindlist(BIRDx1) %>% 
  as_tibble()

ggplot(BIRDx2_4, aes(x = year_offset, y = SpeciesTotal, colour = Infested)) +
  geom_point() +
  geom_smooth(aes(fill = Infested)) +
  ggtitle("only infested 9 for 9 years") +
  theme_bw()

table(BIRDx2_4$year_offset, BIRDx2_4$infoff)   

## only 20+ and 20- years and infested for 9 years (still has uninfested)   ----------------------------
BIRDx1 <- teste %>% 
  filter(year_offset > -20 & year_offset < 20) %>% 
  group_by(RouteId) %>% 
  group_split()

for(i in 1:length(BIRDx1)){
  a <- BIRDx1[[i]]
  maxi <- max(a$year_offset)
  if(maxi < 10) {BIRDx1[[i]] <- NULL }
}

BIRDx2_5 <- data.table::rbindlist(BIRDx1) %>% 
  as_tibble()

ggplot(BIRDx2_5, aes(x = year_offset, y = SpeciesTotal, colour = Infested)) +
  geom_point() +
  geom_smooth(aes(fill = Infested)) +
  ggtitle("20s and 9 years") +
  theme_bw()

table(BIRDx2_5$year_offset, BIRDx2_5$infoff)   

# plot temperatures    
teste
BIRDx2_1
BIRDx2_2
BIRDx2_3
BIRDx2_4
BIRDx2_5

ggplot(teste, aes(x = year_offset, y = SpeciesTotal)) +
  #   geom_point() +
  geom_smooth() +
  ggtitle(species) + xlim(-20,20) 

ggplot(BIRDx2_1, aes(x = year_offset, y = SpeciesTotal)) +
  #   geom_point() +
  geom_smooth() +
  ggtitle(species) + xlim(-20,20) 

ggplot(BIRDx2_2, aes(x = year_offset, y = SpeciesTotal)) +
  #   geom_point() +
  geom_smooth() +
  ggtitle(species) + xlim(-20,20) 

ggplot(BIRDx2_3, aes(x = year_offset, y = SpeciesTotal)) +
  #   geom_point() +
  geom_smooth() +
  ggtitle(species) + xlim(-20,20) 

ggplot(BIRDx2_4, aes(x = year_offset, y = SpeciesTotal)) +
  #   geom_point() +
  geom_smooth() +
  ggtitle(species) + xlim(-20,20) 

ggplot(BIRDx2_5, aes(x = year_offset, y = SpeciesTotal)) +
  #   geom_point() +
  geom_smooth() +
  ggtitle(species) + xlim(-20,20) 

plot_temp <- function(matt){
  
  mattINF <- matt[which(matt$Infested == T),]
  mattNO <- matt[which(BIRDx$Infested == F),]
  
  temps <- rbind(
    mattINF %>% select(temp_min_scale, Infested),
    mattNO %>% select(temp_min_scale, Infested)
  )
  
  t1 <- quantile(mattINF$temp_min_scale, c(0.2, 0.5, 0.8))[1]
  t2 <- quantile(mattINF$temp_min_scale, c(0.2, 0.5, 0.8))[2]
  t3 <- quantile(mattINF$temp_min_scale, c(0.2, 0.5, 0.8))[3]
  
  p1 <- ggplot(matt, aes(x=minTemp, y=temp_min_scale)) +
    geom_point() +
    theme_bw()
  
  p2 <- ggplot(temps, aes(x = temp_min_scale, fill = Infested)) +
    geom_histogram(aes(y = stat(count)/length(temps$temp_min_scale)),
                   position = "identity", alpha = .7,
                   bins = 15) + 
    scale_fill_manual(values=c("grey10", "grey90")) +
    geom_vline(xintercept = t1, size=0.5, color = "black") +
    geom_vline(xintercept = t2, size=0.5, color = "black") +
    geom_vline(xintercept = t3, size=0.5, color = "black") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size= 12),
          axis.title = element_text(size = 12),
          legend.position = "none",
          axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0))) + 
    xlab("Mean minimum temperature") +
    ylab("Frequency") +
    scale_x_continuous(limits=c(-2,2))
  
  grid.arrange(p1, p2, nrow = 1)
}

plot_temp(teste)  
plot_temp(BIRDx2_1)
plot_temp(BIRDx2_2)
plot_temp(BIRDx2_3)
plot_temp(BIRDx2_4)
plot_temp(BIRDx2_5)

par(mfrow=c(2,1))
plot(teste$temp_min_scale, teste$minTemp); hist(teste$temp_min_scale)
plot(BIRDx2_1$temp_min_scale, BIRDx2_1$minTemp); hist(BIRDx2_1$temp_min_scale)
plot(BIRDx2_2$temp_min_scale, BIRDx2_2$minTemp); hist(BIRDx2_2$temp_min_scale)
plot(BIRDx2_3$temp_min_scale, BIRDx2_3$minTemp); hist(BIRDx2_3$temp_min_scale)
plot(BIRDx2_4$temp_min_scale, BIRDx2_4$minTemp); hist(BIRDx2_4$temp_min_scale)
plot(BIRDx2_5$temp_min_scale, BIRDx2_5$minTemp); hist(BIRDx2_5$temp_min_scale)







## rum models

formula <- formulas[[1]]
SPECIES_MOD_DAT <- glue("data/species/{species}.rds")
resu0 <- inla(formula, family="poisson", data=teste, 
              control.predictor=list(compute=TRUE), 
              control.compute=list(waic=TRUE, dic=TRUE, cpo=TRUE))
saveRDS(object = resu0, file = "teste_res.rds")
#rm(resu0)

resu1 <- inla(formula, family="poisson", data=BIRDx2_1, 
              control.predictor=list(compute=TRUE), 
              control.compute=list(waic=TRUE, dic=TRUE, cpo=TRUE))
saveRDS(object = resu1, file = "BIRDx2_1_res.rds")
#rm(resu1)

resu2 <- inla(formula, family="poisson", data=BIRDx2_2, 
              control.predictor=list(compute=TRUE), 
              control.compute=list(waic=TRUE, dic=TRUE, cpo=TRUE))
saveRDS(object = resu2, file = "BIRDx2_2_res.rds")
#rm(resu2)

resu3 <- inla(formula, family="poisson", data=BIRDx2_3, 
              control.predictor=list(compute=TRUE), 
              control.compute=list(waic=TRUE, dic=TRUE, cpo=TRUE))
saveRDS(object = resu3, file = "BIRDx2_3_res.rds")
#rm(resu3)

resu4 <- inla(formula, family="poisson", data=BIRDx2_4, 
              control.predictor=list(compute=TRUE), 
              control.compute=list(waic=TRUE, dic=TRUE, cpo=TRUE))
saveRDS(object = resu4, file = "BIRDx2_4_res.rds")
#rm(resu4)

resu5 <- inla(formula, family="poisson", data=BIRDx2_5, 
              control.predictor=list(compute=TRUE), 
              control.compute=list(waic=TRUE, dic=TRUE, cpo=TRUE))
saveRDS(object = resu5, file = "BIRDx2_5_res.rds")
#rm(resu5)

lista <- list(summary(resu0), summary(resu1), summary(resu2), 
              summary(resu3), summary(resu4), summary(resu5))

all_combinations <- tibble(
  model = 1,
  #formula = as.vector(formulas),
  year = 2,
  filter = seq(1,6,1),
  species = species,  # change species here
  result = lista
) %>%
  unnest(year) %>%
  unnest(species) 

summary_results <- all_combinations

library(tidyverse)
library(glue)
library(fs)
library(INLA)
library(tidyselect)

### choose species here! species list is: 
###  BHVI BLBW BTNW HETH MAWA RBNU ACFL  
spsr <- species

source("7_extract_fixed_pars.R")

#summary_results <- readRDS("data/models_res/summary_results.rds") %>% 
#  filter(species == spsr)
# summary_results <- readRDS(glue('{spsr}_summares_2.RDS'))
# summary_results <- summary_results[1:15,]

summary_results2 <- summary_results %>%
  mutate(waic_list = map(result, "waic")) %>%
  #filter(!map_lgl(waic_list, is.null)) %>%
  mutate(waicNull = !map_lgl(waic_list, is.null),
         waic = NA)

for(i in 1:nrow(summary_results2)){
  if(as.logical(summary_results2$waicNull[i]) == TRUE) {
    summary_results2$waic[i] <- unlist(pluck(summary_results2$waic_list[i], 1))} else {
      summary_results2$waic[i] <- NA
    }
}

summary_results2 <- summary_results2 %>%
  mutate(fixed = map(result, "fixed"),
         intercept = map(fixed, f_intercept),
         year_offset = map(fixed, f_year_offset),
         infoff = map(fixed, f_infoff),
         NewObserver = map(fixed, f_NewObserver),
         temp_min_scale = map(fixed, f_temp_min_scale),
         year_offset_infoff = map(fixed, f_year_offset_infoff),
         year_offset_temp_min_scale = map(fixed, f_year_offset_temp_min_scale),
         infoff_temp_min_scale = map(fixed, f_infoff_temp_min_scale),
         year_offset_infoff_temp_min_scale = map(fixed, f_year_offset_infoff_temp_min_scale)) 

pars_models_FUNC <- function(i) {
  summary_results2 %>%
    rowwise() %>%
    mutate(intercept = as.numeric(intercept[[i]]),
           year_offset = year_offset[[i]],
           infoff = infoff[[i]],
           NewObserver = NewObserver[[i]],
           temp_min_scale = temp_min_scale[[i]],
           year_offset_infoff = year_offset_infoff[[i]],
           year_offset_temp_min_scale = year_offset_temp_min_scale[[i]],
           infoff_temp_min_scale = infoff_temp_min_scale[[i]],
           year_offset_infoff_temp_min_scale = year_offset_infoff_temp_min_scale[[i]],
           esti_type = ifelse(i == 1, "mean", ifelse(i == 2, "low", "up"))) %>% 
    select(-c(result, waic_list, waicNull, fixed))
}

pars_models <- as_tibble(rbind(pars_models_FUNC(1),
                               pars_models_FUNC(2),
                               pars_models_FUNC(3)))

(waic_best <- summary_results2[which(
  summary_results2$waic == min(summary_results2$waic)),1:4])

if(nrow(waic_best) != 1) {stop("\n\n\n\n two models tied as the best according to WAIC - pick one \n\n\n\n")}

year_ <- waic_best$year[1]
mod_ <- waic_best$model[1]
fil_ <- waic_best$filter[1]

nacol <- c("mean", "low", "up","par")
par_tib <- pars_models[which(pars_models$filter == fil_),]
par_tib <- par_tib[which(par_tib$model == mod_),]
par_tib <- par_tib[which(par_tib$year == year_),]
par_tib <- par_tib[which(par_tib$species == species),]

par_tib <- par_tib %>%  
  mutate(WAIC = waic) %>% 
  select(-c(model, year, species, waic)) %>% 
  relocate(esti_type) %>% 
  t() %>% 
  cbind(rownames(.)) %>% 
  as_tibble() %>% 
  rename_with(~nacol) %>% 
  mutate(mean = as.double(mean),
         low = as.double(low),
         up = as.double(up)) %>% 
  relocate(par)
(par_tib <- par_tib[-1,])

# waic plot   ---------------------
ggplot(aes(year, 
           jitter(waic, amount = 0.5),
           waic,
           group=model, color=factor(model)), data = summary_results2) +
  geom_line(alpha=0.5) +
  geom_point(alpha=0.5)


# ploting parameters  -----------------
model_names <- paste(rep("Model",10), seq(1,10), sep= " ")

plot_var <- function(my_tibble, variable1) {
  my_tibble %>% 
    select(all_of(variable1)) %>% 
    unlist() %>% 
    matrix(., ncol = 3, byrow = T) %>% 
    as_tibble() %>% 
    rename(mean = V1, low = V2, up = V3) %>% 
    bind_cols(summary_results2[,c(4,1,3)], .) %>%
    mutate(model_name = as.character(glue("Model {model}"))) %>% 
    group_by(model) %>% 
    transform(model_name = factor(model_name, levels= model_names)) %>% 
    ggplot(aes(year, mean)) +
    geom_hline(aes(yintercept = 0, colour="red"),
               show.legend = FALSE) +
    geom_errorbar(aes(ymin= low, ymax= up),
                  colour = "grey65") +
    geom_point() +
    facet_wrap(~ model_name, ncol = 5) + 
    theme_bw() +
    labs(title = variable1) +
    theme(plot.title = element_text(hjust = 0.5))
}

## mudar aqui tibble --------
## teste    BIRDx2_1    BIRDx2_2   BIRDx2_3   BIRDx2_4  BIRDx2_5
a <- BIRDx2 <- BIRDx2_1
a$Infested <- as.logical(ifelse(a$Infested==1,T,F))

ggplot(a, aes(x = jitter(year_offset), y = jitter(SpeciesTotal), colour = Infested)) +
  geom_point() +
  geom_smooth(aes(fill = Infested))

table(a$YearInfested)
hist(a$YearInfested[which(a$YearInfested != 0)])

ggplot(a, aes(x = year_offset, y = SpeciesTotal)) +
  #   geom_point() +
  geom_smooth() +
  ggtitle(species) + xlim(-20,20)  

# Make predictions with the fixed values and temperature quantiles
my_tibble <- summary_results2
pred_tab <- as_tibble(seq(-10,20,1)) %>% 
  rename(year = value)

create_pred_off <- function(offset_v){
  pred_tabX <- pred_tab %>% 
    mutate(infoff_t =  ifelse(year <= offset_v, 0, 1),
           year_off_t = year - offset_v)
  return(pred_tabX)
}

species <- spsr
offset <- year_

BIRDx2INF <- BIRDx2[which(BIRDx2$Infested == T),]
BIRDx2NO <- BIRDx2[which(BIRDx$Infested == F),]

temps <- rbind(
  BIRDx2INF %>% select(temp_min_scale, Infested),
  BIRDx2NO %>% select(temp_min_scale, Infested)
)

t1 <- quantile(BIRDx2INF$temp_min_scale, c(0.2, 0.5, 0.8))[1]
t2 <- quantile(BIRDx2INF$temp_min_scale, c(0.2, 0.5, 0.8))[2]
t3 <- quantile(BIRDx2INF$temp_min_scale, c(0.2, 0.5, 0.8))[3]

ggplot(temps, aes(x = temp_min_scale, fill = Infested)) +
  geom_histogram(aes(y = stat(count)/length(temps$temp_min_scale)),
                 position = "identity", alpha = .7,
                 bins = 15) + 
  scale_fill_manual(values=c("grey10", "grey90")) +
  geom_vline(xintercept = t1, size=0.5, color = "black") +
  geom_vline(xintercept = t2, size=0.5, color = "black") +
  geom_vline(xintercept = t3, size=0.5, color = "black") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size= 12),
        axis.title = element_text(size = 12),
        legend.position = "none",
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0))) + 
  xlab("Mean minimum temperature") +
  ylab("Frequency") 

predict.inla2 <- function(species, modelN, temp, max) {
  my_tibble2 <- my_tibble %>% 
    filter(species == species,
           model == modelN,
           filter_ == fil_) %>% 
    select(species,
           model,
           offset = year,
           intercept,
           year_off_t = year_offset,
           infoff,
           temp_min_scale,
           year_offset_infoff,
           year_offset_temp_min_scale,
           infoff_temp_min_scale,
           year_offset_infoff_temp_min_scale) 
  
  pred_tab2 <- create_pred_off(2)
  pred_tab3 <- create_pred_off(3)
  pred_tab4 <- create_pred_off(4)
  pred_tab5 <- create_pred_off(5)
  pred_tab6 <- create_pred_off(6)
  pred_tab7 <- create_pred_off(7)
  pred_tab8 <- create_pred_off(8)
  pred_tab9 <- create_pred_off(9)
  pred_tab10 <- create_pred_off(10)
  pred_tab11 <- create_pred_off(11)
  pred_tab12 <- create_pred_off(12)
  pred_tab13 <- create_pred_off(13)
  pred_tab14 <- create_pred_off(14)
  pred_tab15 <- create_pred_off(15)
  pred_tab16 <- create_pred_off(16)
  
  pred_tab_name <- glue("pred_tab{year_}")
  
  plot.pred(off = year_, pars_tib = my_tibble2, pred_tabX = get(pred_tab_name), temp = temp, max = max)
  
}

plot.pred <- function(off, pars_tib, pred_tabX, temp, max){
  pars_tib <- pars_tib %>% 
    filter(offset == off)
  if(nrow(pars_tib) != 1) {stop("oooppsssss error row 19 extracting model pars")}
  
  # mean -----------
  ifelse(!is.na(pars_tib$intercept[[1]][1]), b0 <- pars_tib$intercept[[1]][1], b0 <- 0)
  ifelse(!is.na(pars_tib$year_off_t[[1]][1]), b1 <- pars_tib$year_off_t[[1]][1], b1 <- 0)
  ifelse(!is.na(pars_tib$infoff[[1]][1]), b2 <- pars_tib$infoff[[1]][1], b2 <- 0)
  ifelse(!is.na(pars_tib$temp_min_scale[[1]][1]), b3 <- pars_tib$temp_min_scale[[1]][1], b3 <- 0)
  ifelse(!is.na(pars_tib$year_offset_infoff[[1]][1]), b4 <- pars_tib$year_offset_infoff[[1]][1], b4 <- 0)
  ifelse(!is.na(pars_tib$year_offset_temp_min_scale[[1]][1]), b5 <- pars_tib$year_offset_temp_min_scale[[1]][1], b5 <- 0)
  ifelse(!is.na(pars_tib$infoff_temp_min_scale[[1]][1]), b6 <- pars_tib$infoff_temp_min_scale[[1]][1], b6 <- 0)
  ifelse(!is.na(pars_tib$year_offset_infoff_temp_min_scale[[1]][1]), b7 <- pars_tib$year_offset_infoff_temp_min_scale[[1]][1], b7 <- 0)
  
  pred_tabX <- pred_tabX %>% 
    mutate(temp_t = temp)
  
  no_infes <- pred_tabX %>% 
    mutate(prediction = exp(
      b0 + (b1 * year_off_t) + (b3 * temp_t) +
        (b5 * year_off_t * temp_t)),
      HWA = 'infest'
    )
  
  infes <- pred_tabX %>% 
    mutate(prediction = exp(
      b0 + (b1 * year_off_t) + (b2 * infoff_t) + (b3 * temp_t) +
        (b4 * year_off_t * infoff_t) + (b5 * year_off_t * temp_t) +
        (b6 * infoff_t * temp_t) + (b7 * year_off_t * infoff_t * temp_t)),
      HWA = 'no_infest'
    )
  
  # up 
  b0u <- pars_tib$intercept[[1]][3]
  b1u <- pars_tib$year_off_t[[1]][3]
  b2u <- pars_tib$infoff[[1]][3]
  b3u <- pars_tib$temp_min_scale[[1]][3]
  b4u <- pars_tib$year_offset_infoff[[1]][3]
  b5u <- pars_tib$year_offset_temp_min_scale[[1]][3]
  b6u <- pars_tib$infoff_temp_min_scale[[1]][3]
  b7u <- pars_tib$year_offset_infoff_temp_min_scale[[1]][3]
  
  no_infes <- no_infes %>% 
    mutate(predictionU = exp(
      b0u + (b1u * year_off_t) + (b3u * temp_t) +
        (b5u * year_off_t * temp_t)),
      HWA = 'infest'
    )
  
  infes <- infes %>% 
    mutate(predictionU = exp(
      b0u + (b1u * year_off_t) + (b2u * infoff_t) + (b3u * temp_t) +
        (b4u * year_off_t * infoff_t) + (b5u * year_off_t * temp_t) +
        (b6u * infoff_t * temp_t) + (b7u * year_off_t * infoff_t * temp_t)),
      HWA = 'no_infest'
    )
  # low -----------------
  b0l <- pars_tib$intercept[[1]][2]
  b1l <- pars_tib$year_off_t[[1]][2]
  b2l <- pars_tib$infoff[[1]][2]
  b3l <- pars_tib$temp_min_scale[[1]][2]
  b4l <- pars_tib$year_offset_infoff[[1]][2]
  b5l <- pars_tib$year_offset_temp_min_scale[[1]][2]
  b6l <- pars_tib$infoff_temp_min_scale[[1]][2]
  b7l <- pars_tib$year_offset_infoff_temp_min_scale[[1]][2]
  
  no_infes <- no_infes %>% 
    mutate(predictionL = exp(
      b0l + (b1l * year_off_t) + (b3l * temp_t) +
        (b5l * year_off_t * temp_t)),
      HWA = 'infest'
    )
  
  infes <- infes %>% 
    mutate(predictionL = exp(
      b0l + (b1l * year_off_t) + (b2l * infoff_t) + (b3l * temp_t) +
        (b4l * year_off_t * infoff_t) + (b5l * year_off_t * temp_t) +
        (b6l * infoff_t * temp_t) + (b7l * year_off_t * infoff_t * temp_t)),
      HWA = 'no_infest'
    )
  # ---------
  plot_preds <- rbind(no_infes, infes)
  
  off_gap <- infes %>% 
    filter(year %in% c(off, off+1))
  
  plot_preds <- plot_preds %>% 
    filter(!(HWA == 'no_infest' & year_off_t < 0)) %>% 
    arrange(desc(HWA)) 
  
  ggplot(aes(x = year, y = prediction, col = HWA), data = plot_preds) +
    geom_vline(xintercept = 0, size=1, color = "darkgray") +
    geom_vline(xintercept = off, linetype="dotted", color = "black", size=1) +
    geom_line() +
    #geom_line(aes(x = year, y = prediction), data = off_gap,
    #          col = 'white', size=2, alpha=1) + 
    geom_point(size = 2) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size= 16),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16),
          legend.position = "none",
          axis.title = element_text(size = 16)) + 
    xlab("Years since HWA infestation") +
    ylab("Bird Abundance") +
    scale_x_continuous(breaks = c(-10,-5,0,5,10,15,20)) +
    ylim(0, max)
  # confidence interval
  #geom_line(aes(x = year, y = predictionL, col = HWA), data = plot_preds) +
  #geom_line(aes(x = year, y = predictionU, col = HWA), data = plot_preds)
  
}

maxi <- 0.6

predict.inla2(spsr, mod_, t2, maxi)

unique(BIRDtab$min_tempMe)/100
unique(BIRDtab$sd_tempMi)/100

quantile(BIRDx2INF$temp_min_scale, c(0.2, 0.5, 0.8))







