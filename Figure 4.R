rm(list=ls())
setwd("~~")

# Reading the data and checking the variable names
star.data <- read.csv('div_sta_area.csv',header=T)
variable.names(star.data)

#################### transformation function
scaleFUN <- function(x) sprintf("%.1f", x)
############################################

# loading the packages
library(smatr)
library(dplyr)
library(purrr)
library(tidyr)

##################################################################################
# the relationship between estar_slope and sar_ave_slope
# SAM_model
estar_sar.sma <- sma(estar_slope ~ sar_ave_slope, data = star.data)
summary(estar_sar.sma)
plot(estar_sar.sma)

# create new data set of sar_ave_slope at a high resolution (200 points from min to max)
estar_sar_preds <- data.frame(expand.grid(sar_ave_slope = seq(min(star.data$sar_ave_slope, na.rm = T), max(star.data$sar_ave_slope, na.rm = T), length.out = 200), stringsAsFactors = FALSE))

# bootstrap data and get predictions
estar_sar_preds <- star.data %>%
  # create new bootstrapped data sets
  modelr::bootstrap(n = 1000, id = 'boot_num') %>%
  # fit sma to every bootstrap
  group_by(boot_num) %>%
  mutate(., fit = map(strap, ~ sma(estar_slope ~ sar_ave_slope, data=data.frame(.), method="SMA"))) %>%
  ungroup() %>%
  # extract intercept and slope from each fit
  mutate(., intercept = map_dbl(fit, ~coef(.x)[1]),
         slope = map_dbl(fit, ~coef(.x)[2])) %>%
  select(., -fit) %>%
  # get fitted values for each bootstrapped model
  # uses the estar_sar_preds dataframe we made earlier
  group_by(boot_num) %>%
  do(data.frame(fitted = .$intercept + .$slope*estar_sar_preds$sar_ave_slope, 
                sar_ave_slope = estar_sar_preds$sar_ave_slope)) %>%
  ungroup() %>%
  # calculate the 2.5% and 97.5% quantiles at each sar_ave_slope value
  group_by(., sar_ave_slope) %>%
  dplyr::summarise(., conf_low = quantile(fitted, 0.025),
                   conf_high = quantile(fitted, 0.975)) %>%
  ungroup() %>%
  # add fitted value of actual unbootstrapped model
  mutate(., estar_slope = coef(estar_sar.sma)[1] + coef(estar_sar.sma)[2]*sar_ave_slope)

# plot with ggplot
StAR_SAR.plot <- ggplot(star.data, aes(y = estar_slope, x = sar_ave_slope)) +
  geom_point(size = 4, alpha = 0.5, color ="#0072B2") +
  geom_line(data = estar_sar_preds, size = 1.5, color="#0072B2") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.1, estar_sar_preds) +
  ylab(bquote(atop(paste("Z "[EStAR]), (NULL["slope of ecosystem stability–area"])))) +
  xlab(bquote(atop(paste("Z "[SAR]), (NULL["slope of species–area"])))) +
  labs(tag = "A", color = " ") +
  annotate("text", x = 0.4, y = 0.45, size = 5, family = "Arial", label ="paste(italic(R^2), \" = 0.18, \", italic(P),\" = 0.014\")", 
           color = "#0072B2", parse=TRUE, hjust= 0) +
  annotate("text", x = 0.4, y = 0.40, size = 5, family = "Arial", 
           colour = "#0072B2", label ="paste(italic(slope), \"  = 1.18\")", parse=TRUE, hjust= 0) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 0.5),
        panel.border = element_blank(),
        axis.text.y = element_text(size = 16, family = "Arial", color = "black"),
        axis.text.x = element_text(size = 16, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 20, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 20, family = "Arial", color = "black"),
        axis.ticks.length = unit(1, "mm"),
        axis.line = element_line(color = "black", size = 0.5),
        plot.tag = element_text(size = 22, family = "Arial", color = "black"))


############################################################
# the relationship between sstar_slope and sar_ave_slope_slope
# SAM_model
sstar_sar.sma <- sma(sstar_slope ~ sar_ave_slope, data = star.data)
summary(sstar_sar.sma)
plot(sstar_sar.sma)

# create new data set of sar_ave_slope at a high resolution (200 points from min to max)
sstar_sar_preds <- data.frame(expand.grid(sar_ave_slope = seq(min(star.data$sar_ave_slope, na.rm = T), max(star.data$sar_ave_slope, na.rm = T), length.out = 200), stringsAsFactors = FALSE))

# bootstrap data and get predictions
sstar_sar_preds <- star.data %>%
  # create new bootstrapped data sets
  modelr::bootstrap(n = 1000, id = 'boot_num') %>%
  # fit sma to every bootstrap
  group_by(boot_num) %>%
  mutate(., fit = map(strap, ~ sma(sstar_slope ~ sar_ave_slope, data=data.frame(.), method="SMA"))) %>%
  ungroup() %>%
  # extract intercept and slope from each fit
  mutate(., intercept = map_dbl(fit, ~coef(.x)[1]),
         slope = map_dbl(fit, ~coef(.x)[2])) %>%
  select(., -fit) %>%
  # get fitted values for each bootstrapped model
  # uses the sstar_sar_preds dataframe we made earlier
  group_by(boot_num) %>%
  do(data.frame(fitted = .$intercept + .$slope*sstar_sar_preds$sar_ave_slope, 
                sar_ave_slope = sstar_sar_preds$sar_ave_slope)) %>%
  ungroup() %>%
  # calculate the 2.5% and 97.5% quantiles at each sar_ave_slope value
  group_by(., sar_ave_slope) %>%
  dplyr::summarise(., conf_low = quantile(fitted, 0.025),
                   conf_high = quantile(fitted, 0.975)) %>%
  ungroup() %>%
  # add fitted value of actual unbootstrapped model
  mutate(., sstar_slope = coef(sstar_sar.sma)[1] + coef(sstar_sar.sma)[2]*sar_ave_slope)

# plot with ggplot
SpAR_SAR.plot <- ggplot(star.data, aes(y = sstar_slope, x = sar_ave_slope)) +
  geom_point(size = 4, alpha = 0.5, color ="#009E73") +
  geom_line(data = sstar_sar_preds, size = 1.5, color="#009E73",linetype = "dashed") +
  ylab(bquote(atop(paste("Z "[SStAR]), (NULL["slope of species stability–area"])))) +
  xlab(bquote(atop(paste("Z "[SAR]), (NULL["slope of species–area"])))) +
  labs(tag = "B", color = " ") +
  annotate("text", x = 0.5, y = 0.225, size = 5, family = "Arial", label ="paste(italic(R^2), \" = 0.02, \", italic(P),\" = 0.440\")", 
           color = "#009E73", parse=TRUE, hjust= 0) +
  annotate("text", x = 0.5, y = 0.202, size = 5, family = "Arial", 
           colour = "#009E73", label ="paste(italic(slope), \"  = -0.60\")", parse=TRUE, hjust= 0) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 0.5),
        panel.border = element_blank(),
        axis.text.y = element_text(size = 16, family = "Arial", color = "black"),
        axis.text.x = element_text(size = 16, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 20, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 20, family = "Arial", color = "black"),
        axis.ticks.length = unit(1, "mm"),
        axis.line = element_line(color = "black", size = 0.5),
        plot.tag = element_text(size = 22, family = "Arial", color = "black"))


############################################################
# the relationship between asar_slope and sar_ave_slope_slope
# SAM_model
asar_sar.sma <- sma(asar_slope ~ sar_ave_slope, data = star.data)
summary(asar_sar.sma)
plot(asar_sar.sma)

# create new data set of sar_ave_slope at a high resolution (200 points from min to max)
asar_sar_preds <- data.frame(expand.grid(sar_ave_slope = seq(min(star.data$sar_ave_slope, na.rm = T), max(star.data$sar_ave_slope, na.rm = T), length.out = 200), stringsAsFactors = FALSE))

# bootstrap data and get predictions
asar_sar_preds <- star.data %>%
  # create new bootstrapped data sets
  modelr::bootstrap(n = 1000, id = 'boot_num') %>%
  # fit sma to every bootstrap
  group_by(boot_num) %>%
  mutate(., fit = map(strap, ~ sma(asar_slope ~ sar_ave_slope, data=data.frame(.), method="SMA"))) %>%
  ungroup() %>%
  # extract intercept and slope from each fit
  mutate(., intercept = map_dbl(fit, ~coef(.x)[1]),
         slope = map_dbl(fit, ~coef(.x)[2])) %>%
  select(., -fit) %>%
  # get fitted values for each bootstrapped model
  # uses the asar_sar_preds dataframe we made earlier
  group_by(boot_num) %>%
  do(data.frame(fitted = .$intercept + .$slope*asar_sar_preds$sar_ave_slope, 
                sar_ave_slope = asar_sar_preds$sar_ave_slope)) %>%
  ungroup() %>%
  # calculate the 2.5% and 97.5% quantiles at each sar_ave_slope value
  group_by(., sar_ave_slope) %>%
  dplyr::summarise(., conf_low = quantile(fitted, 0.025),
                   conf_high = quantile(fitted, 0.975)) %>%
  ungroup() %>%
  # add fitted value of actual unbootstrapped model
  mutate(., asar_slope = coef(asar_sar.sma)[1] + coef(asar_sar.sma)[2]*sar_ave_slope)

# plot with ggplot
AsAR_SAR.plot <- ggplot(star.data, aes(y = asar_slope, x = sar_ave_slope)) +
  geom_point(size = 4, alpha = 0.5, color ="#D55E00") +
  geom_line(data = asar_sar_preds, size = 1.5, color="#D55E00") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.1, asar_sar_preds) +
  ylab(bquote(atop(paste("Z "[AsAR]), (NULL["slope of species asynchrony–area"])))) +
  xlab(bquote(atop(paste("Z "[SAR]), (NULL["slope of species–area"])))) +
  labs(tag = "C", color = " ") +
  annotate("text", x = 0.4, y = 0.30, size = 5, family = "Arial", label ="paste(italic(R^2), \" = 0.28, \", italic(P),\" = 0.002\")", 
           color = "#D55E00", parse=TRUE, hjust= 0) +
  annotate("text", x = 0.4, y = 0.26, size = 5, family = "Arial", 
           colour = "#D55E00", label ="paste(italic(slope), \"  = 1.10\")", parse=TRUE, hjust= 0) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 0.5),
        panel.border = element_blank(),
        axis.text.y = element_text(size = 16, family = "Arial", color = "black"),
        axis.text.x = element_text(size = 16, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 20, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 20, family = "Arial", color = "black"),
        axis.ticks.length = unit(1, "mm"),
        axis.line = element_line(color = "black", size = 0.5),
        plot.tag = element_text(size = 22, family = "Arial", color = "black"))

# Saving figures
library(cowplot)
plot_grid(StAR_SAR.plot, SpAR_SAR.plot, AsAR_SAR.plot, nrow = 1, align= "hv")

