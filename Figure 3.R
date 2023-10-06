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
# the relationship between estar_slope and sstar_slope
# SAM_model
estar_sstar.sma <- sma(estar_slope ~ sstar_slope, slope.test=1, data = star.data, method="SMA")
summary(estar_sstar.sma)
plot(estar_sstar.sma)

# create new data set of sstar_slope at a high resolution (200 points from min to max)
estar_sstar_preds <- data.frame(expand.grid(sstar_slope = seq(min(star.data$sstar_slope, na.rm = T), max(star.data$sstar_slope, na.rm = T), length.out = 200), stringsAsFactors = FALSE))

# bootstrap data and get predictions
estar_sstar_preds <- star.data %>%
  # create new bootstrapped data sets
  modelr::bootstrap(n = 1000, id = 'boot_num') %>%
  # fit sma to every bootstrap
  group_by(boot_num) %>%
  mutate(., fit = map(strap, ~ sma(estar_slope ~ sstar_slope, data=data.frame(.), method="SMA"))) %>%
  ungroup() %>%
  # extract intercept and slope from each fit
  mutate(., intercept = map_dbl(fit, ~coef(.x)[1]),
         slope = map_dbl(fit, ~coef(.x)[2])) %>%
  select(., -fit) %>%
  # get fitted values for each bootstrapped model
  # uses the estar_sstar_preds dataframe we made earlier
  group_by(boot_num) %>%
  do(data.frame(fitted = .$intercept + .$slope*estar_sstar_preds$sstar_slope, 
                sstar_slope = estar_sstar_preds$sstar_slope)) %>%
  ungroup() %>%
  # calculate the 2.5% and 97.5% quantiles at each sstar value
  group_by(., sstar_slope) %>%
  dplyr::summarise(., conf_low = quantile(fitted, 0.025),
                   conf_high = quantile(fitted, 0.975)) %>%
  ungroup() %>%
  # add fitted value of actual unbootstrapped model
  mutate(., estar_slope = coef(estar_sstar.sma)[1] + coef(estar_sstar.sma)[2]*sstar_slope)

  
# plot with ggplot
StAR_SpAR.plot <- ggplot(star.data, aes(y = estar_slope, x = sstar_slope)) +
  geom_vline(xintercept=0, linetype='dashed', color='gray', size=0.5) +
  geom_hline(yintercept=0, linetype='dashed', color='gray', size=0.5) +
  geom_abline(intercept = 0, slope = 1, size = 1, color='gray') +
  geom_point(size = 4, alpha = 0.5, color ="#0072B2") +
  geom_line(data = estar_sstar_preds, size = 1.5, color="#009E73") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.1, estar_sstar_preds) +
  ylab(bquote(atop(paste("Z "[EStAR]), (NULL["slope of ecosystem stability–area"])))) +
  xlab(bquote(atop(paste("Z "[SStAR]), (NULL["slope of species stability–area"])))) +
  ylim(-0.11, 0.5) +
  xlim(-0.11, 0.5) +
  labs(tag = "A", color = " ") +
  annotate("text", x = 0.07, y = -0.03, size = 5, family = "Arial", label ="paste(italic(R^2), \" = 0.14, \", italic(P),\" = 0.030\")", 
           color = "#009E73", parse=TRUE, hjust= 0) +
  annotate("text", x = 0.07, y = -0.1, size = 5, family = "Arial", color = "#009E73", label ="paste(italic(slope), \"  = 1.98\")", parse=TRUE, hjust= 0) +
  annotate("text", x = 0.4, y = 0.48, size = 5, family = "Arial", 
           colour = "gray", label ="1:1", parse=TRUE, hjust= 0) +
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


##################################################################################
# the relationship between estar_slope and asar_slope
# SAM_model
estar_asar.sma <- sma(estar_slope ~ asar_slope, slope.test=1, data = star.data)
summary(estar_asar.sma)
plot(estar_asar.sma)

# create new data set of asar_slope at a high resolution (200 points from min to max)
estar_asar_preds <- data.frame(expand.grid(asar_slope = seq(min(star.data$asar_slope, na.rm = T), max(star.data$asar_slope, na.rm = T), length.out = 200), stringsAsFactors = FALSE))

# bootstrap data and get predictions
estar_asar_preds <- star.data %>%
  # create new bootstrapped data sets
  modelr::bootstrap(n = 1000, id = 'boot_num') %>%
  # fit sma to every bootstrap
  group_by(boot_num) %>%
  mutate(., fit = map(strap, ~ sma(estar_slope ~ asar_slope, data=data.frame(.), method="SMA"))) %>%
  ungroup() %>%
  # extract intercept and slope from each fit
  mutate(., intercept = map_dbl(fit, ~coef(.x)[1]),
         slope = map_dbl(fit, ~coef(.x)[2])) %>%
  select(., -fit) %>%
  # get fitted values for each bootstrapped model
  # uses the estar_asar_preds dataframe we made earlier
  group_by(boot_num) %>%
  do(data.frame(fitted = .$intercept + .$slope*estar_asar_preds$asar_slope, 
                asar_slope = estar_asar_preds$asar_slope)) %>%
  ungroup() %>%
  # calculate the 2.5% and 97.5% quantiles at each asar value
  group_by(., asar_slope) %>%
  dplyr::summarise(., conf_low = quantile(fitted, 0.025),
                   conf_high = quantile(fitted, 0.975)) %>%
  ungroup() %>%
  # add fitted value of actual unbootstrapped model
  mutate(., estar_slope = coef(estar_asar.sma)[1] + coef(estar_asar.sma)[2]*asar_slope)

# plot with ggplot
StAR_AsAR.plot <- ggplot(star.data, aes(y = estar_slope, x = asar_slope)) +
  geom_vline(xintercept=0, linetype='dashed', color='gray', size=0.5) +
  geom_hline(yintercept=0, linetype='dashed', color='gray', size=0.5) +
  geom_abline(intercept = 0, slope = 1, size = 1, color='gray') +
  geom_point(size = 4, alpha = 0.5, color ="#0072B2") +
  geom_line(data = estar_asar_preds, size = 1.5, color="#D55E00") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.1, estar_asar_preds) +
  ylab(bquote(atop(paste("Z "[EStAR]), (NULL["slope of ecosystem stability–area"])))) +
  xlab(bquote(atop(paste("Z "[AsAR]), (NULL["slope of species asynchrony–area"])))) +
  ylim(-0.11, 0.5) +
  xlim(-0.11, 0.5) +
  labs(tag = "B", color = " ") +
  annotate("text", x = 0.07, y = -0.03, size = 5, family = "Arial", label ="paste(italic(R^2), \" = 0.75, \", italic(P),\" < 0.0001\")", 
           color = "#D55E00", parse=TRUE, hjust= 0) +
  annotate("text", x = 0.07, y = -0.1, size = 5, family = "Arial", 
           colour = "#D55E00", label ="paste(italic(slope), \"  = 1.07\")", parse=TRUE, hjust= 0) +
  
  annotate("text", x = 0.4, y = 0.48, size = 5, family = "Arial", 
           colour = "gray", label ="1:1", parse=TRUE, hjust= 0) +
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
plot_grid(StAR_SpAR.plot, StAR_AsAR.plot, nrow = 1, align= "hv")
