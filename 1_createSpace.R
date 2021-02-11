library(sp)
library(raster)
library(spdep)
library(dplyr)
library(stringr)

# create hexagon map
HEX_DATA_PATH <- "data/src/HexMap/hexagons"
ROUTE_DATA_PATH <- "data/src/route_hex.csv"

hexmap <- shapefile(HEX_DATA_PATH)
  
#plot(hexmap)
# transform shapefile into adjacency matrix (for CAR model)
# who is whose neighbour?
temp <- poly2nb(hexmap, row.names= hexmap$Input_FID)
nb2INLA("hexmap.graph", temp)
hex.adj <- paste(getwd(),"/data/hexmap.graph", sep="")
hexord <- as.data.frame(cbind(seq(1,length(hexmap$Input_FID),by =1),hexmap$Input_FID))
colnames(hexord) <- c("hexID","FID")

route_hex <- read_csv(ROUTE_DATA_PATH, col_types = cols_only(
  RTENO = col_character(),
  Input_FID = col_number())) %>% 
  select(RouteId = `RTENO`,
         FID = `Input_FID`)

for(i in 1:nrow(route_hex)){
  if(nchar(route_hex$RouteId[i]) != 5) {
    route_hex$RouteId[i] <- str_c(rep(0,(5-nchar(route_hex$RouteId[i]))), route_hex$RouteId[i], collapse= "")
    } else {
      route_hex$RouteId[i] <- route_hex$RouteId[i]
    }
}

route_hex <- left_join(route_hex, hexord, by = "FID") %>% 
  select(`RouteId`,
         `hexID`)

saveRDS(route_hex, "data/route_hex.rds")
