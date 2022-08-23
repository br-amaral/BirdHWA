# 1_createSpace

# Input;
#   data/src/HexMap/hexagons: hexagon shape file, spatial position and FID of each cell
#   data/src/route_hex.csv: matrix with each route and its FID
# Output: 
#   route_hex.csv: matrix with each route and the hexagon number where it is

# Load packages ---------------------
library(sp)
library(spdep)
library(tidyverse)
requireNamespace("raster")

# Load data ---------------------
#3 create hexagon map
HEX_DATA_PATH <- "data/src/HexMap/hexagons"
ROUTE_DATA_PATH <- "data/src/route_hex.csv"

hexmap <- raster::shapefile(HEX_DATA_PATH)
  
# plot(hexmap)
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
         FID = `Input_FID`) %>%
  mutate(RouteId = ifelse(nchar(RouteId) != 5,
                          paste0(strrep(0, times = 5 - nchar(RouteId)), RouteId),
                          paste0(RouteId)
                          ))

route_hex <- left_join(route_hex, hexord, by = "FID") %>% 
  select(`RouteId`,
         `hexID`)

saveRDS(route_hex, "data/route_hex.rds")
