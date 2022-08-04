library(ggplot2)
library(SiZer)
library(ggtext)

# create the plots for explaining methodology
dat <- as.data.frame(matrix(NA, ncol = 2, nrow = 37))
colnames(dat) <- c("year", "res")
dat$year[1:36] <- seq(-15,20,1)
dat$res[1:22] <- seq(3.95,9.2,0.25)
dat$res[23:36] <- rev(seq(7.1, 8.4,0.1))

ggplot(dat) +
  geom_point(aes(x = year, y = res))

# basic ----------------------
model <- piecewise.linear(dat$year[1:36],dat$res[1:36], CI=FALSE)
#group according to the changepoint
dat$grp = factor(ifelse(dat$year > 6,1,0))
dat[37,1:3] <- c(6,8.5,1)  ## do I want this for line to connect???

ggplot(dat,aes(x=year,y=res,group=grp)) + 
  geom_vline(xintercept = 0, color = "gray70", linetype = "solid") +
  geom_smooth(method="lm",formula=y~x,col="black", se = F) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  #geom_vline(xintercept = 6, color = "gray28", linetype = "dashed") +
  labs(x="Standardized infestation year (<i>x</i><sub><i>i,t_std</i></sub>)",
       y = expression("Bird count (Log(N"[i]*"))")) +
  theme(axis.title.x = element_markdown(margin = unit(c(3, 0, 0, 0), "mm"))) +
  scale_x_continuous(breaks=seq(-15,20,5), labels=seq(-15,20,5), limits = c(-15,20)) +
  scale_y_continuous(breaks=seq(0,12,3), labels=seq(0,12,3), limits = c(0,13)) 

# adding the effects panel -----------------------
## equation 1 - normal bbs
dat <- as.data.frame(matrix(NA, ncol = 2, nrow = 36))
colnames(dat) <- c("year", "res")
dat$year[1:36] <- seq(1983,2018,1)
dat$res[1:36] <- seq(3.95,12.7,0.25)

ggplot(dat) + 
  geom_vline(xintercept = 1998 , color = "gray70", linetype = "solid") +
  geom_line(aes(x = year, y = res), col="black", size = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  #geom_vline(xintercept = 6, color = "gray28", linetype = "dashed") +
  labs(x="Year (<i>x</i><sub><i>i,t</i></sub>)",
       y = expression("Bird count (Log(N"[i]*"))")) +
  theme(axis.title.x = element_markdown(margin = unit(c(3, 0, 0, 0), "mm"))) +
  scale_x_continuous(breaks=seq(1983,2018,5), labels=seq(1983,2018,5), limits = c(1983,2018)) +
  scale_y_continuous(breaks=seq(0,12,3), labels=seq(0,12,3), limits = c(0,13))


##
# create the plots for explaining methodology
dat <- as.data.frame(matrix(NA, ncol = 2, nrow = 37))
colnames(dat) <- c("year", "res")
dat$year[1:36] <- seq(-15,20,1)
dat$res[1:18] <- seq(3.95,8.2,0.25)
dat$res[19:36] <- rev(seq(7.1, 8.8,0.1))


# basic ----------------------
model <- piecewise.linear(dat$year[1:36],dat$res[1:36], CI=FALSE)
#group according to the changepoint
dat$grp = factor(ifelse(dat$year > 0,1,0))
dat[37,1:3] <- c(6,8.5,1)  ## do I want this for line to connect???

ggplot(dat,aes(x=year,y=res,group=grp)) + 
  geom_vline(xintercept = 0, color = "gray70", linetype = "solid") +
  geom_smooth(method="lm",formula=y~x,col="black", se = F) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  #geom_vline(xintercept = 0, color = "gray28", linetype = "dashed") +
  labs(x="Year (<i>x</i><sub><i>i,t</i></sub>)",
       y = expression("Bird count (Log(N"[i]*"))")) +
  theme(axis.title.x = element_markdown(margin = unit(c(3, 0, 0, 0), "mm"))) +
  scale_x_continuous(breaks=seq(-15,20,5), labels=seq(1983,2018,5), limits = c(-15,20)) +
  scale_y_continuous(breaks=seq(0,12,3), labels=seq(0,12,3), limits = c(0,13)) 

ggplot(dat,aes(x=year,y=res,group=grp)) + 
  geom_vline(xintercept = 0, color = "gray70", linetype = "solid") +
  geom_smooth(method="lm",formula=y~x,col="black", se = F) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 200, l = 0))) +
  #geom_vline(xintercept = 0, color = "gray28", linetype = "dashed") +
  labs(x="Standardized infestation year (<i>x</i><sub><i>i,t_std</i></sub>)",
       y = expression("Bird count (Log(N"[i]*"))")) +
  theme(axis.title.x = element_markdown(margin = unit(c(3, 0, 0, 0), "mm"))) +
  scale_x_continuous(breaks=seq(-15,20,5), labels=seq(-15,20,5), limits = c(-15,20)) +
  scale_y_continuous(breaks=seq(0,12,3), labels=seq(0,12,3), limits = c(0,13)) 
