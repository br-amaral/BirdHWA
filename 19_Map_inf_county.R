# 19_Map_inf_county
# get which counties were used in the analysis, and for how long each has been infested.
#
# Input:   data/BirdHWA_2.rds: bird dataset with info from BBS and HWA created by 3_combineData 
#          data/Fips_route.csv
#          data/fips_order.csv
#
# Output:  data/fips_route2.csv
#          data/routes_study.csv
#          data/map_routes_order.csv
#

BIRD <- readRDS("data/BirdHWA_2.rds")

routes_used_infestime <- 
  BIRD %>% select(RouteId, Year, YearInfested, yrhwa) %>% distinct() %>% mutate(lenInf = 2018 - YearInfested) %>% 
  select(RouteId, lenInf, YearInfested) %>% 
  distinct() #%>% #filter(RouteId %in% c("72013", "72060")) %>% 
  #View()

for(i in 1:nrow(routes_used_infestime)){
  if(routes_used_infestime$lenInf[i] == 2018) {
    routes_used_infestime$lenInf[i] <- 0
  }
}

table(routes_used_infestime$lenInf)  # no zeros, perfect!


all_routes_data <- readRDS(file = 'data/infestations_2.rds') %>% 
  select(RouteId, YearInfested) %>% 
  distinct() %>% 
  rename(YearInfComplete = YearInfested) %>% 
  mutate(YearInfComplete = ifelse(YearInfComplete == "Inf", 0, YearInfComplete))

all_routes_data <- left_join(all_routes_data, routes_used_infestime, by = "RouteId")

all_routes_data <- all_routes_data %>% 
  mutate(YearInfested = ifelse(is.na(YearInfested), 2030, YearInfested)) ## 2030 infested but not used in analysis
  
c(39013, 88009, 88039, 88919)

42133 - fips
72195
72196

fips_route <- read_csv("data/Fips_route.csv") %>% 
  select(FIPS, RouteId)

fips_infes <- left_join(fips_route, all_routes_data, by = "RouteId") %>% 
  select(FIPS,YearInfested) %>% 
  distinct(FIPS, .keep_all = TRUE)

fips_order <- read_csv("data/fips_order.csv") %>% 
  select(FIPS)

fips_route2 <- left_join(fips_order, fips_infes, by = "FIPS") 
fips_route2$YearInfested2 <- ifelse(is.na(fips_route2$YearInfested), 2030, fips_route2$YearInfested)

routes_order <- read_csv("data/map_routes_order.csv") %>% 
  select(OBJID,RTENO) %>% 
  rename(RouteId = RTENO) %>% 
  mutate(RouteId = as.character(RouteId))

routes_study <- left_join(routes_order, all_routes_data %>% select(RouteId, YearInfested), by ='RouteId')
routes_study$YearInfested <- ifelse(is.na(routes_study$YearInfested), 2030, routes_study$YearInfested)
routes_study$YearInfested <- ifelse(routes_study$YearInfested != 2030, 10, routes_study$YearInfested)

 
## add counties with no bbs routes info
#no_route <- read_csv("data/no_route_county.csv") %>% 
#  select(FIPS,YEARFIRST)

#for(i in 1:nrow(fips_route2)){

#    if(is.na(fips_route2$YearInfested[i])){
#      a <- fips_route2$FIPS[i]
#      b <- no_route[which(no_route$FIPS == a),2] %>% pull()
#      fips_route2$YearInfested[i] <- b
#  } else {fips_route2$YearInfested[i] <-fips_route2$YearInfested[i]}
#}

write_csv(fips_route2, file = "data/fips_route2.csv")
write_csv(routes_study, file = "data/routes_study.csv")

