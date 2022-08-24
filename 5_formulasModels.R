# 5_formulasModels.R
#   script with all 11 models created to look at relationship of birds and
#     temperature, infestation, etc

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
formula3 <- SpeciesTotal ~ 1 + 
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
formula2 <- SpeciesTotal ~ 1 + 
  year_offset + 
  infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  temp_min_scale : infoff +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  

# modeel in between shgould have temperature times year!
# only temp_min_scale but no temp interaction - additive effect ----------
formula4 <- SpeciesTotal ~ 1 + 
  year_offset + 
  infoff +
  year_offset : infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  

# only temp_min_scale but no interaction ----------
# additive effect + only intercept change
formula5 <- SpeciesTotal ~ 1 + 
  year_offset + 
  infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)

## only temp_min_scale but no interaction ----------
# additive effect + only slope change
formula6 <- SpeciesTotal ~ 1 + 
  year_offset + 
  year_offset : infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  

# MODELS WITH NO TEMPERATURE:  --------------------
## null model ----------
formula10 <- SpeciesTotal ~ 1 + 
  year_offset + 
  NewObserver +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE) 

## intercept, year_offset, NewObservers + plus intercept change ----------
formula8 <- SpeciesTotal ~ 1 + 
  year_offset + 
  infoff +
  NewObserver +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE) 

## intercept, year_offset, NewObservers + plus trend change ----------
formula9 <- SpeciesTotal ~ 1 + 
  year_offset + 
  year_offset : infoff +
  NewObserver +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE) 

## intercept, year_offset, NewObservers + intercept and trend change ----------
formula7 <- SpeciesTotal ~ 1 + 
  year_offset + 
  infoff +
  year_offset : infoff +
  NewObserver +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE) 

# Model x
formula11 <- SpeciesTotal ~ 1 + 
  year_offset + 
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  


formulas <- list(formula1,formula2,formula3,formula4,
                 formula6,formula6,formula7,formula8,
                 formula9,formula10, formula11)
