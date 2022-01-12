library(lme4)
library(tidyverse)

set.seed.(321)

## SIMULATION 1 - only fied parameters -------------------------------------------------
#  Page 25 of the book - starting with only fixed parameters and no package 

# how many times I am simulating and fitting the model
reps <- 1000

# start with only fixed
b1 <- 1.2       # year_offset
b2 <- -2        # infoff
b3 <- -0.5      # NewObserver
b4 <- 1.5       # tempminscale
# this are random
ObserverRoute
Year
hexID

# matrix to store the estimates
par.est.pois <- matrix(NA, nrow= reps, ncol = 8) %>% 
  as.data.frame
colnames(par.est.pois) <- c("year_offset","year_offset_SD",         
                            "infoff", "infoff_SD",             
                            "NewObserver", "NewObserver_SD",          
                            "tempminscale", "tempminscale_SD")      
# sample size
n <- 1000

# bird mock data: preciso dos dados de x
year_offset          # integer
infoff               # binary
NewObserver          # binary
tempminscale         # numeric

nroutes <- 50
X <- matrix(NA, ncol = 4, nrow = 31 * nroutes) %>% 
  as.data.frame()
colnames(X) <- c("year_offset", "infoff", "NewObserver", "tempminscale")
X$year_offset <- rep(seq(-10,20), nroutes)
X$infoff <- rep(c(rep(0,16),rep(1,15)), nroutes)
X$NewObserver <- replicate(nroutes*31, rbinom(1, size = 1, 0.5))
X$tempminscale <- rnorm(nroutes*31, 0, 1.8)

for (i in 1:reps){
  Y <- exp(1 + b1 * X$year_offset +
             b2 * X$infoff +
             b3 * X$NewObserver +
             b4 * X$tempminscale)
  
  plot(Y)
  
  model <- glm(Y ~ X$year_offset + X$infoff + X$NewObserver + X$tempminscale,
               family = poisson)
  
  vcv <- vcov(model)
  
  par.est.pois$year_offset[i] <- model$coefficients[2]
  par.est.pois$year_offset_SD[i] <- sqrt(diag(vcv)[2])
  par.est.pois$infoff[i] <- model$coefficients[3]
  par.est.pois$infoff_SD[i] <- sqrt(diag(vcv)[3])
  par.est.pois$NewObserver[i] <- model$coefficients[4]
  par.est.pois$NewObserver_SD[i] <- sqrt(diag(vcv)[4])
  par.est.pois$tempminscale[i] <- model$coefficients[5]
  par.est.pois$tempminscale_SD[i] <- sqrt(diag(vcv)[5])
  print(i)
}

comp.est <- matrix(NA, ncol = 8, nrow= 2) %>% 
  as.data.frame()
comp.est[1,] <- colMeans(par.est.pois) 
comp.est[2,1] <- b1
comp.est[2,3] <- b2
comp.est[2,5] <- b3
comp.est[2,7] <- b4
colnames(comp.est) <- colnames(par.est.pois)#[c(TRUE, FALSE)]
rownames(comp.est) <- c("estimated", "simulated")
comp.est

## Adding the interaction effects ------------------------

# how many times I am simulating and fitting the model
reps <- 1000

# start with only fixed
b1 <- 1.2       # year_offset
b2 <- -2        # infoff
b3 <- -0.5      # NewObserver
b4 <- 1.5       # tempminscale
b5 <- -1.35
b6 <- -0.3
b7 <- -0.5
b8 <- -1.1


# matrix to store the estimates
par.est.pois <- matrix(NA, nrow= reps, ncol = 16) %>% 
  as.data.frame
colnames(par.est.pois) <- c("year_offset","year_offset_SD",         
                            "infoff", "infoff_SD",             
                            "NewObserver", "NewObserver_SD",          
                            "tempminscale", "tempminscale_SD",
                            "year_offset:infoff", "year_offset:infoff_SD",
                            "temp_min_scale:year_offset","temp_min_scale:year_offset_SD",
                            "temp_min_scale:infoff","temp_min_scale:infoff_SD",
                            "temp_min_scale:infoff:year_offset","temp_min_scale:infoff:year_offset_SD")

# sample size
n <- 1000

nroutes <- 50
X <- matrix(NA, ncol = 4, nrow = 31 * nroutes) %>% 
  as.data.frame()
colnames(X) <- c("year_offset", "infoff", "NewObserver", "tempminscale")
X$year_offset <- rep(seq(-10,20), nroutes)
X$infoff <- rep(c(rep(0,16),rep(1,15)), nroutes)
X$NewObserver <- replicate(nroutes*31, rbinom(1, size = 1, 0.5))
X$tempminscale <- rnorm(nroutes*31, 0, 1.8)

for (i in 1:reps){
  Y <- exp(1 + b1 * X$year_offset +
             b2 * X$infoff +
             b3 * X$NewObserver +
             b4 * X$tempminscale +
             b5 * X$year_offset * X$infoff +
             b6 * X$year_offset * X$tempminscale +
             b7 * X$tempminscale * X$infoff +
             b8 * X$tempminscale * X$infoff * X$year_offset
  )
  
  plot(Y)
  
  model <- glm(Y ~ X$year_offset + as.factor(X$infoff) + as.factor(X$NewObserver) + X$tempminscale + 
                 X$year_offset * as.factor(X$infoff) + X$year_offset * X$tempminscale + 
                 X$tempminscale * as.factor(X$infoff) + X$tempminscale * as.factor(X$infoff) * X$year_offset,
               family = poisson)
  
  vcv <- vcov(model)
  
  par.est.pois$year_offset[i] <- model$coefficients[2]
  par.est.pois$year_offset_SD[i] <- sqrt(diag(vcv)[2])
  
  par.est.pois$infoff[i] <- model$coefficients[3]
  par.est.pois$infoff_SD[i] <- sqrt(diag(vcv)[3])
  
  par.est.pois$NewObserver[i] <- model$coefficients[4]
  par.est.pois$NewObserver_SD[i] <- sqrt(diag(vcv)[4])
  
  par.est.pois$tempminscale[i] <- model$coefficients[5]
  par.est.pois$tempminscale_SD[i] <- sqrt(diag(vcv)[5])
  
  par.est.pois$`year_offset:infoff`[i] <- model$coefficients[6]
  par.est.pois$`year_offset:infoff_SD`[i] <- sqrt(diag(vcv)[6])
  
  par.est.pois$`temp_min_scale:year_offset`[i] <- model$coefficients[7]
  par.est.pois$`temp_min_scale:year_offset_SD`[i] <- sqrt(diag(vcv)[7])
  
  par.est.pois$`temp_min_scale:infoff`[i] <- model$coefficients[8]
  par.est.pois$`temp_min_scale:infoff_SD`[i] <- sqrt(diag(vcv)[8])
  
  par.est.pois$`temp_min_scale:infoff:year_offset`[i] <- model$coefficients[9]
  par.est.pois$`temp_min_scale:infoff:year_offset_SD`[i] <- sqrt(diag(vcv)[9])
  
  print(i)
}

comp.est <- matrix(NA, ncol = 16, nrow= 2) %>% 
  as.data.frame()
comp.est[1,] <- colMeans(par.est.pois) 
comp.est[2,1] <- b1
comp.est[2,3] <- b2
comp.est[2,5] <- b3
comp.est[2,7] <- b4
comp.est[2,9] <- b5
comp.est[2,11] <- b6
comp.est[2,13] <- b7
comp.est[2,15] <- b8
colnames(comp.est) <- colnames(par.est.pois)#[c(TRUE, FALSE)]
rownames(comp.est) <- c("estimated", "simulated")
comp.est


## add random effects to the simulation ------------------------

# how many times I am simulating and fitting the model
reps <- 1000

# start with only fixed
b0 <- -0.429
b1 <- -0.003       # year_offset
b2 <- -0.3        # infoff
b3 <- -0.067      # NewObserver
b4 <- -0.28       # tempminscale
# add interaction
b5 <- -0.004       # year_offset:infoff
b6 <- 0.037        # temp_min_scale:year_offset
b7 <- -0.1        # temp_min_scale:infoff
b8 <- -0.11        # temp_min_scale:infoff:year_offset
# add random
b9 <- 0.04         # ObserverRoute, model="iid" 
b10 <- 0.04        # Year, model="iid"
# spatial effect - so far with no 'map'
b11 <- 0.04        # hexID, model="bym"

nroutes <- 50

# similar in the same route, between years
ra_eff_obrou <- as.vector(NA)
for(i in 1:nroutes){
  rout_vals_obsrou <- outer(
    rep(rnorm(n=1, 
              mean = 0, 
              sd = sqrt(0.6)),  ## enter the variance to get deviation
        31),
    rep(rnorm(n=31, 
              mean = 0, 
              sd = sqrt(0.0005))
    ),
    FUN = "+"
  )
  rout_vals_obsrou <- rout_vals_obsrou[,1]
  ra_eff_obrou <- c(ra_eff_obrou,
                    rout_vals_obsrou   
  )
}
ra_eff_obrou <- ra_eff_obrou[-1]

# similar between years
ra_eff_years <- as.vector(NA)
rm(rout_vals_years)
for(j in 1:nroutes){
  rout_vals_years <- ceiling(outer(
    seq(1990,2020,1),
    rep(rnorm(n=31, 
              mean = 0, 
              sd = sqrt(20))
    ),
    FUN = "-"
  ))
  rout_vals_years <- rout_vals_years[,1]
  ra_eff_years <- c(ra_eff_years,
                    rout_vals_years   
  )
}
ra_eff_years <- ra_eff_years[-1]

ra_eff_years2 <- (ra_eff_years - (mean(ra_eff_years)))/sd(ra_eff_years)



ra_eff_hexId <- rep(rnorm(n= 31, 
                          mean = 0, 
                          sd = sqrt(site_var) ),   ## enter the variance to get deviation
                    each = nroutes)



# matrix to store the estimates
par.est.pois <- matrix(NA, nrow= reps, ncol = 16) %>% 
  as.data.frame
colnames(par.est.pois) <- c("year_offset","year_offset_SD",         
                            "infoff", "infoff_SD",             
                            "NewObserver", "NewObserver_SD",          
                            "tempminscale", "tempminscale_SD",
                            "year_offset:infoff", "year_offset:infoff_SD",
                            "temp_min_scale:year_offset","temp_min_scale:year_offset_SD",
                            "temp_min_scale:infoff","temp_min_scale:infoff_SD",
                            "temp_min_scale:infoff:year_offset","temp_min_scale:infoff:year_offset_SD")

X <- matrix(NA, ncol = 4, nrow = 31 * nroutes) %>% 
  as.data.frame()
colnames(X) <- c("year_offset", "infoff", "NewObserver", "tempminscale")
X$year_offset <- rep(seq(-10,20), nroutes)
X$infoff <- rep(c(rep(0,16),rep(1,15)), nroutes)
X$NewObserver <- replicate(nroutes*31, rbinom(1, size = 1, 0.5))
X$tempminscale <- rnorm(nroutes*31, 0, 1.8)
X$Year <- ra_eff_years2
X$ObsRou <- ra_eff_obrou
X$hexId


for (i in 1:reps){
  Y <- exp(1 + b1 * X$year_offset +
             b2 * X$infoff +
             b3 * X$NewObserver +
             b4 * X$tempminscale +
             b5 * X$year_offset * X$infoff +
             b6 * X$year_offset * X$tempminscale +
             b7 * X$tempminscale * X$infoff +
             b8 * X$tempminscale * X$infoff * X$year_offset +
             b9 * X$ObsRou +
             b10 * X$Year
  )
  
  plot(Y)
  
  coefini <- c(model$coefficients[1:4], "ObsRou" = 0, "Year" = 0, model$coefficients[5:9])
  
  m1 <- glm(Y ~ year_offset + infoff + NewObserver + tempminscale + 
              year_offset * infoff + year_offset * tempminscale +
              tempminscale * infoff + tempminscale * infoff * year_offset,
            family = poisson(link = "log"), data = X)
  
  model <- glm(Y ~ year_offset + infoff + NewObserver + tempminscale + 
                     ObsRou + Year,
                 data = X,
                 family = poisson(link = "log"))
  
  
  model <- glmer(Y ~ year_offset + infoff + NewObserver + tempminscale + 
                     year_offset * infoff + year_offset * tempminscale +
                     tempminscale * infoff + tempminscale * infoff * year_offset +
                     (1 | ObsRou) + 
                     (1 | Year),
                 data = X,
                 family = poisson(link = "log"),
                 start=list(fixef=coef(m1)),
                 control = glmerControl(tolPwrss=1e-3),
                 verbose = 100)
  
  vcv <- vcov(model)
  
  par.est.pois$year_offset[i] <- model$coefficients[2]
  par.est.pois$year_offset_SD[i] <- sqrt(diag(vcv)[2])
  
  par.est.pois$infoff[i] <- model$coefficients[3]
  par.est.pois$infoff_SD[i] <- sqrt(diag(vcv)[3])
  
  par.est.pois$NewObserver[i] <- model$coefficients[4]
  par.est.pois$NewObserver_SD[i] <- sqrt(diag(vcv)[4])
  
  par.est.pois$tempminscale[i] <- model$coefficients[5]
  par.est.pois$tempminscale_SD[i] <- sqrt(diag(vcv)[5])
  
  par.est.pois$`year_offset:infoff`[i] <- model$coefficients[6]
  par.est.pois$`year_offset:infoff_SD`[i] <- sqrt(diag(vcv)[6])
  
  par.est.pois$`temp_min_scale:year_offset`[i] <- model$coefficients[7]
  par.est.pois$`temp_min_scale:year_offset_SD`[i] <- sqrt(diag(vcv)[7])
  
  par.est.pois$`temp_min_scale:infoff`[i] <- model$coefficients[8]
  par.est.pois$`temp_min_scale:infoff_SD`[i] <- sqrt(diag(vcv)[8])
  
  par.est.pois$`temp_min_scale:infoff:year_offset`[i] <- model$coefficients[9]
  par.est.pois$`temp_min_scale:infoff:year_offset_SD`[i] <- sqrt(diag(vcv)[9])
  
  print(i)
}

comp.est <- matrix(NA, ncol = 16, nrow= 2) %>% 
  as.data.frame()
comp.est[1,] <- colMeans(par.est.pois) 
comp.est[2,1] <- b1
comp.est[2,3] <- b2
comp.est[2,5] <- b3
comp.est[2,7] <- b4
comp.est[2,9] <- b5
comp.est[2,11] <- b6
comp.est[2,13] <- b7
comp.est[2,15] <- b8
colnames(comp.est) <- colnames(par.est.pois)#[c(TRUE, FALSE)]
rownames(comp.est) <- c("estimated", "simulated")
comp.est






























