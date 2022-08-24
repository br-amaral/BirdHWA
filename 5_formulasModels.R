# 5_formulasModels.R
#   script with all 11 models created to look at relationship of birds and
#     temperature, infestation, etc

# MODELS WITH TEMPERATURE:  --------------------
## model 1 (full model) --------------------
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

## model 2: immediate effect --------------------
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

## model 3: long-term effect --------------------
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

# NO EFFECT OF TEMPERATURE ON INFESTATION --------------------
## model 4 --------------------
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

## model 5 ------------------------------
formula5 <- SpeciesTotal ~ 1 + 
  year_offset + 
  infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)

## model 6 ------------------------------
formula6 <- SpeciesTotal ~ 1 + 
  year_offset + 
  year_offset : infoff +
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  

# MODELS WITH NO TEMPERATURE:  ------------------------------
## model 7------------------------------
formula7 <- SpeciesTotal ~ 1 + 
  year_offset + 
  infoff +
  year_offset : infoff +
  NewObserver +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE) 

## model 8 ----------------------------------------
formula8 <- SpeciesTotal ~ 1 + 
  year_offset + 
  infoff +
  NewObserver +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE) 

## model 9 ------------------------------
formula9 <- SpeciesTotal ~ 1 + 
  year_offset + 
  year_offset : infoff +
  NewObserver +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE) 

# NO INFESTATION EFFECTS
## model 10 ------------------------------
formula10 <- SpeciesTotal ~ 1 + 
  year_offset + 
  NewObserver +
  temp_min_scale +
  temp_min_scale : year_offset +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE)  

## model 11 (null model) ------------------------------
formula11 <- SpeciesTotal ~ 1 + 
  year_offset + 
  NewObserver +
  f(ObserverRoute, model="iid") + 
  f(Year, model="iid") +
  f(hexID, model="bym", graph=hex.adj, constr=TRUE) 
formulas <- list(formula1,formula2,formula3,formula4,
                 formula6,formula6,formula7,formula8,
                 formula9,formula10, formula11)
