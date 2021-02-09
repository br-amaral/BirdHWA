### with temperature: ##
## full model
formula1 <- SpeciesTotal ~ 1 + 
  yrhwa + 
  infoff +
  yrhwa * infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale * yrhwa +
  temp_min_scale * infoff +
  temp_min_scale * infoff * yrhwa +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  

## minus infoff: no initial change, broken stick with 2 connected lines
formula2 <- SpeciesTotal ~ 1 + 
  yrhwa + 
  yrhwa * infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale * yrhwa +
  temp_min_scale * infoff * yrhwa +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  

## minus yrhwa * infoff: no change in trend - one time effect
formula3 <- SpeciesTotal ~ 1 + 
  yrhwa + 
  infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale * yrhwa +
  temp_min_scale * infoff +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  

## modeel in between shgould have temperature times year!
## only temp_min_scale but no temp interaction - additive effect
formula4 <- SpeciesTotal ~ 1 + 
  yrhwa + 
  infoff +
  yrhwa * infoff +
  NewObserver +
  temp_min_scale * yrhwa +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  

## only temp_min_scale but no interaction - additive effect + only intercept change
formula5 <- SpeciesTotal ~ 1 + 
  yrhwa + 
  infoff +
  NewObserver +
  temp_min_scale * yrhwa +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)

## only temp_min_scale but no interaction - additive effect + only slope change
formula6 <- SpeciesTotal ~ 1 + 
  yrhwa + 
  yrhwa * infoff +
  NewObserver +
  temp_min_scale * yrhwa +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  

### No temperature ###
## only intercept, yrhwa, NewObservers - null model
formula7 <- SpeciesTotal ~ 1 + 
  yrhwa + 
  NewObserver +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE) 

## intercept, yrhwa, NewObservers + plus intercept change
formula8 <- SpeciesTotal ~ 1 + 
  yrhwa + 
  infoff +
  NewObserver +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE) 

## intercept, yrhwa, NewObservers + plus trend change
formula9 <- SpeciesTotal ~ 1 + 
  yrhwa + 
  yrhwa * infoff +
  NewObserver +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE) 

## intercept, yrhwa, NewObservers + intercept and trend change
formula10 <- SpeciesTotal ~ 1 + 
  yrhwa + 
  infoff +
  yrhwa * infoff +
  NewObserver +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE) 


formulas <- list(formula1,formula2,formula3,formula4,
                 formula6,formula6,formula7,formula8,
                 formula9,formula10)
