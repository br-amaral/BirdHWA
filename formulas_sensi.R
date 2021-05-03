# MODELS WITH TEMPERATURE:  --------------------
# full model ----------
formula1 <- SpeciesTotal ~ 1 + 
  year_offset + 
  infoff +
  year_offset : infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  temp_min_scale : infoff +
  temp_min_scale : infoff : year_offset +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  

# minus infoff: no initial change, broken stick with 2 connected lines ----------
formula2 <- SpeciesTotal ~ 1 + 
  year_offset + 
  year_offset : infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  temp_min_scale : infoff : year_offset +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  

# minus year_offset : infoff: no change in trend - one time effect ----------
formula3 <- SpeciesTotal ~ 1 + 
  year_offset + 
  infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  temp_min_scale : infoff +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  
####

###### obs_route as fixed

# MODELS WITH TEMPERATURE:  --------------------
# full model ----------
formula4 <- SpeciesTotal ~ 1 + 
  year_offset + 
  infoff +
  year_offset : infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  temp_min_scale : infoff +
  temp_min_scale : infoff : year_offset +
  ObserverRoute + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  

# minus infoff: no initial change, broken stick with 2 connected lines ----------
formula5 <- SpeciesTotal ~ 1 + 
  year_offset + 
  year_offset : infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  temp_min_scale : infoff : year_offset +
  ObserverRoute + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  

# minus year_offset : infoff: no change in trend - one time effect ----------
formula6 <- SpeciesTotal ~ 1 + 
  year_offset + 
  infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  temp_min_scale : infoff +
  ObserverRoute + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  

######### no hedId

# MODELS WITH TEMPERATURE:  --------------------
# full model ----------
formula7 <- SpeciesTotal ~ 1 + 
  year_offset + 
  infoff +
  year_offset : infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  temp_min_scale : infoff +
  temp_min_scale : infoff : year_offset +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +

# minus infoff: no initial change, broken stick with 2 connected lines ----------
formula8 <- SpeciesTotal ~ 1 + 
  year_offset + 
  year_offset : infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  temp_min_scale : infoff : year_offset +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +

# minus year_offset : infoff: no change in trend - one time effect ----------
formula9 <- SpeciesTotal ~ 1 + 
  year_offset + 
  infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  temp_min_scale : infoff +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +






