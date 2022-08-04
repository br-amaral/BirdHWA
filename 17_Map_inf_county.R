routes_used_infestime <- 
  BIRD3 %>% select(RouteId, Year, YearInfested, yrhwa) %>% distinct() %>% mutate(lenInf = 2018 - YearInfested) %>% 
  select(RouteId, lenInf, YearInfested) %>% 
  distinct() #%>% #filter(RouteId %in% c("72013", "72060")) %>% 
  #View()

for(i in 1:nrow(routes_used_infestime)){
  if(routes_used_infestime$lenInf[i] == 2018) {
    routes_used_infestime$lenInf[i] <- 0
  }
}

all_routes_data <- readRDS(file = 'data/infestations_2.rds') %>% 
  select(RouteId, YearInfested) %>% 
  distinct() %>% 
  rename(YearInfComplete = YearInfested) %>% 
  mutate(YearInfComplete = ifelse(YearInfComplete == "Inf", 0, YearInfComplete))

all_routes_data <- left_join(all_routes_data, routes_used_infestime, by = "RouteId")

all_routes_data <- all_routes_data %>% 
  mutate(YearInfested = ifelse(is.na(YearInfested), 2030, YearInfested))
  
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

## add counties with no bbs routes info
no_route <- read.csv("data/no_route_county.csv") %>% 
  select(FIPS,YEARFIRST)

for(i in 1:nrow(fips_route2)){

    if(is.na(fips_route2$YearInfested[i])){
      a <- fips_route2$FIPS[i]
      b <- no_route[which(no_route$FIPS == a),2]
      fips_route2$YearInfested[i] <- b
  } else {fips_route2$YearInfested[i] <-fips_route2$YearInfested[i]}
}

write_csv(fips_route2, file = "data/fips_route2.csv")
