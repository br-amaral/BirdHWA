library(ggplot2)
library(tidyverse)
library(glue)

COEF_PATH_SENSI <- glue("data/models_res/{species}/sensi/coefs_{species}.rds")
COEF_PATH <- glue("data/models_res/{species}/coefs_{species}.rds")

plot_tib <- readRDS(COEF_PATH_SENSI)

plot_tib2 <- readRDS(COEF_PATH)

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
  geom_point(data = plot_tib2, aes(x = par2, y = mean)) +
  geom_errorbar(data = plot_tib2, aes(ymin=low, ymax=up),
                size=.3, width=0.1) +
  ggtitle("Sensitivity Analysis")
