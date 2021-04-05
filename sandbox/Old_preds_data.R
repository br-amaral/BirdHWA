# Make predictions and plotting ---------
# summary_results <- summary_results_MAWA2
# change all MAWA2 for data object path, and summary_results_MAWA2 for summary_results$results

plot_pred <- function (yr_type, i, bird2) {
  if(i > 15) {stop("we do not have that offset!")}
  if(yr_type == "yrhwa"){
    predmean <- aggregate(bird2[[i]]$`pred.0.5quant`, list(bird2[[i]]$year_offset), FUN=mean)  ## aggregate medians by year (mean of medians)
    predUCL<-aggregate(bird2[[i]]$`pred.0.975quant`, list(bird2[[i]]$year_offset), FUN=mean)
    predLCL<-aggregate(bird2[[i]]$`pred.0.025quant`, list(bird2[[i]]$year_offset), FUN=mean)
  }
  
  if(yr_type == "year"){
    predmean <- aggregate(bird2[[i]]$`pred.0.5quant`, list(bird2[[i]]$Year), FUN=mean)  
    predUCL<-aggregate(bird2[[i]]$`pred.0.975quant`, list(bird2[[i]]$Year), FUN=mean)
    predLCL<-aggregate(bird2[[i]]$`pred.0.025quant`, list(bird2[[i]]$Year), FUN=mean)
  }
  
  names(predmean)[names(predmean)=="x"]<-"mean"
  names(predmean)[names(predmean)=="Group.1"]<-"yrnb"
  names(predUCL)[names(predUCL)=="x"]<-"UCL"
  names(predLCL)[names(predLCL)=="x"]<-"LCL"
  birdpred <- cbind(predmean, predLCL$LCL, predUCL$UCL)
  colnames(birdpred) <- c("yrnb", "mean", "LCL", "UCL")
  #freq_key <- as.data.frame(cbind(sort(unique(bird2[[i]]$YearInfested)),table(bird2[[i]]$YearInfested)))
  #colnames(freq_key) <- c("YearInfested", "Freq_yrinf")
  #rownames(freq_key) <- NULL
  #birdpred <- left_join(birdpred, freq_key, by = "YearInfested")
  
  LCLmax <- ceiling(max(birdpred$UCL))
  plot(birdpred$yrnb,birdpred$mean, pch=16, ylim=c(0,LCLmax),
       xlim = c(1988, 2018),
       xlab="years since first HWA detection", ylab="birds per route", #main="Segmented Model",
       cex.lab=1.1, cex.axis=1.1, main = glue("{i+1} year offset")) 
  lines(x = birdpred$yrnb, y = birdpred$LCL, col="darkgrey", lwd=2)
  lines(x = birdpred$yrnb, y = birdpred$UCL, col="darkgrey", lwd=2)
  abline(v = mean(bird2[[i]]$YearInfested[bird2[[i]]$YearInfested != 0 ],
                  na.rm = TRUE) + i + 1, col = "blue")
  abline(v = min(bird2[[i]]$YearInfested[bird2[[i]]$YearInfested != 0 ],
                 na.rm = TRUE)  + i + 1, col = "red")
}

inla_pred <- function (species, model, yr_type){
  summary_results2 <- summary_results[which(summary_results$species == species),]
  summary_results2 <- summary_results2[which(summary_results2$model == model),]
  #dplyr::filter(species == species) %>% 
  #dplyr::filter(model == model) %>% 
  summary_results2 <- summary_results2 %>% 
    mutate(pred_path = glue(RESULT_PATH, RESULT_GLUE2),
           pred_obj = NA,
           sps_tib = glue(EACH_SPS_DATA_PATH))
  
  preds <- list()
  for(i in 1:nrow(summary_results2)){
    print(i)
    a <- read_rds(summary_results2$pred_path[i])
    b <- as_tibble(as.data.frame(a$summary.fitted.values))
    preds[[i]] <- b
    rm(a)
  }
  
  ind_sps <- list()
  for(i in 1:nrow(summary_results2)){
    c <- read_rds(summary_results2$sps_tib[i]) %>% 
      # year_offset is standardizing yrhwa to the offset (years after infestation to the impact)
      mutate(year_offset = ifelse(YearInfested != 0, Year - YearInfested - summary_results2$year[i], 0),
             # infoff: 'infested' route according to the delay in the effect (offset)
             infoff = ifelse(year_offset <= 0, 0, ifelse(year_offset > 0, 1, NA)))
    ind_sps[[i]] <- c
  }
  
  bird1 <- Map(cbind, ind_sps, pred = preds)
  bird2 <- Map(na.omit, bird1)
  
  par(mfrow = c(3,5))
  plot_pred("year", 1, bird2)
  plot_pred("year", 2, bird2)
  plot_pred("year", 3, bird2)
  plot_pred("year", 4, bird2)
  plot_pred("year", 5, bird2)
  plot_pred("year", 6, bird2)
  plot_pred("year", 7, bird2)
  plot_pred("year", 8, bird2)
  plot_pred("year", 9, bird2)
  plot_pred("year", 10, bird2)
  plot_pred("year", 11, bird2)
  plot_pred("year", 12, bird2)
  plot_pred("year", 13, bird2)
  plot_pred("year", 14, bird2)
  plot_pred("year", 15, bird2)
  #print(glue("model {model} predictions for {species}"))
}

inla_pred("BHVI", 1, "year")
inla_pred("BHVI", 2, "year")
inla_pred("BHVI", 3, "year")
inla_pred("BHVI", 4, "year")
inla_pred("BHVI", 5, "year")
inla_pred("BHVI", 6, "year")
inla_pred("BHVI", 7, "year")
inla_pred("BHVI", 8, "year")
inla_pred("BHVI", 9, "year")
inla_pred("BHVI", 10, "year")



