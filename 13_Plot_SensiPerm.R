library(ggplot2)
library(tidyverse)
library(glue)

COEF_PATH_SENSI <- glue("data/models_res/{species}/sensi/coefs_{species}.rds")
COEF_PATH <- glue("data/models_res/{species}/coefs_{species}.rds")

plot_tib <- readRDS(COEF_PATH_SENSI)

plot_tib2 <- readRDS(COEF_PATH)
plot_tib2$par2 <- c("B0", "B1", "B2", "B4", "B3", "B5", "B6", "B7")
for(i in 1:nrow(plot_tib2)){
  if(is.na(plot_tib2$low[i])) {plot_tib2$low[i] <- 0}
  if(is.na(plot_tib2$up[i])) {plot_tib2$up[i] <- 0}
}

plot_tib2 <- plot_tib2 %>% 
  filter(!(value == 0))

colnames(plot_tib2)[3:4] <- c("low1", "up1")
  
ggplot(data = plot_tib, aes(x = par2, y = mean)) +
  geom_jitter(col = "gray") +
  #geom_errorbar(aes(ymin=low, ymax=up),
  #              size=.3,    # Thinner lines
  #              width=.2) 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none") +
  ylab("Estimates") +
  xlab("Coefficients") +
  geom_point(data = plot_tib2, aes(x = par2, y = value)) +
  geom_errorbar(data = plot_tib2, aes(ymin=low, ymax=up, x = par2),
                size=.3, width=0.1) +
  ggtitle("Sensitivity Analysis")

ggplot(data = plot_tib, aes(x = par2, y = mean)) +
  geom_jitter(col = "gray") +
  #geom_errorbar(aes(ymin=low, ymax=up),
  #              size=.3,    # Thinner lines
  #              width=.2) 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none") +
  ylab("Estimates") +
  xlab("Coefficients") +
  geom_point(data = plot_tib2, aes(x = par2, y = value)) +
  geom_segment(data = plot_tib2, aes(x = par2, y = low, xend = par2, yend = up)) +
  ggtitle("Sensitivity Analysis")


