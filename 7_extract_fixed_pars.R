f_intercept <- function(a){
  if("(Intercept)" %in% rownames(a)){
    a[which(rownames(a) == "(Intercept)"), c(1,3,5)]} else {
      rep(NA,3)
    }
}  

f_year_offset <- function(a){
  if("year_offset" %in% rownames(a)){
    a[which(rownames(a) == "year_offset"), c(1,3,5)]} else {
      rep(NA,3)
    }
}  

f_infoff <- function(a){
  if("infoff" %in% rownames(a)){
    a[which(rownames(a) == "infoff"), c(1,3,5)]} else {   
      rep(NA,3)
    }
}  

f_NewObserver <- function(a){
  if("NewObserverTRUE" %in% rownames(a)){
    a[which(rownames(a) == "NewObserverTRUE"), c(1,3,5)]} else {
      rep(NA,3)
    }
}  

f_temp_min_scale <- function(a){
  if("temp_min_scale" %in% rownames(a)){
    a[which(rownames(a) == "temp_min_scale"), c(1,3,5)]} else {
      rep(NA,3)
    }
}  

f_year_offset_infoff <- function(a){
  if("year_offset:infoff" %in% rownames(a)){
    a[which(rownames(a) == "year_offset:infoff"), c(1,3,5)]} else {
      rep(NA,3)
    }
}  

f_year_offset_temp_min_scale <- function(a){
  if("year_offset:temp_min_scale" %in% rownames(a)){
    a[which(rownames(a) == "year_offset:temp_min_scale"), c(1,3,5)]
  } else {
    if("temp_min_scale:year_offset" %in% rownames(a)){
      a[which(rownames(a) == "temp_min_scale:year_offset"), c(1,3,5)]
    } else {
      rep(NA,3)}
  }
}  

f_infoff_temp_min_scale <- function(a){
  if("infoff:temp_min_scale" %in% rownames(a)){
    a[which(rownames(a) == "infoff:temp_min_scale"), c(1,3,5)]} else {
      rep(NA,3)
    }
}  

f_year_offset_infoff_temp_min_scale <- function(a){
  if("year_offset:temp_min_scale:infoff" %in% rownames(a)){
    a[which(rownames(a) == "year_offset:temp_min_scale:infoff"), c(1,3,5)]
  } else {
    if("temp_min_scale:year_offset:infoff" %in% rownames(a)){
      a[which(rownames(a) == "temp_min_scale:year_offset:infoff"), c(1,3,5)]
    } else {
      if("temp_min_scale:infoff:year_offset" %in% rownames(a)){
        a[which(rownames(a) == "temp_min_scale:infoff:year_offset"), c(1,3,5)]
      } else {
        if("year_offset:infoff:temp_min_scale" %in% rownames(a)){
          a[which(rownames(a) == "year_offset:infoff:temp_min_scale"), c(1,3,5)]
        } else {
          if("infoff:temp_min_scale:year_offset" %in% rownames(a)){
            a[which(rownames(a) == "infoff:temp_min_scale:year_offset"), c(1,3,5)]
          } else {
            if("infoff:year_offset:temp_min_scale" %in% rownames(a)){
              a[which(rownames(a) == "infoff:year_offset:temp_min_scale"), c(1,3,5)]
            } else {
              rep(NA,3)}}}}}}
  }


