rm(list=ls())
setwd("~~")

# Reading the data and checking the variable names
star.data <- read.csv('div_sta_area.csv',header=T)
variable.names(star.data)

# loading the packages
library(piecewiseSEM)
##############################################################################################################
# Using our preliminary structural equation modeling (SEM) as a foundation (see Fig. 3A), 
# we executed several SEMs to account for potential associations among variables.
# We initiated a comprehensive model encompassing all potential pathway effects.
# After achieving a model with an optimal fit—indicated by a low AIC and p > 0.05 without 
# omitting significant pathways (verified using Shipley's test of d-separation, Lefcheck 2016).
# For our final SEM, we made selections based on the principle of model selection, 
# prioritizing the lowest AIC.
##############################################################################################################
# the first full SEM (N = 33)
SEM_AP_SR_full.m <- psem(
  lm(sar_ave_slope ~ MAT_C + MAP_mm, na.action=na.omit, data = star.data), # empirical relationships
  
  lm(sstar_slope ~ MAT_C + MAP_mm + sar_ave_slope, na.action=na.omit, data = star.data), # empirical relationships
  lm(asar_slope ~ MAT_C + MAP_mm + sar_ave_slope, na.action=na.omit, data = star.data), # empirical relationships
  
  lm(estar_slope ~ sstar_slope + asar_slope, na.action=na.omit, data = star.data), # mathematical relationships, r2 = 1.00
  MAT_C %~~% MAP_mm,
  data = star.data
)
# To evaluate the model
summary(SEM_AP_SR_full.m) # Note that there are several non-significant pathways (P ≥ 0.05).
fisherC(SEM_AP_SR_full.m) # Note that SEM_AP_SR_full.m was saturated (Fisher.c < df) and its significant estimations were very well with p > 0.05.
AIC(SEM_AP_SR_full.m)
dSep(SEM_AP_SR_full.m) # Note that SEM_AP_SR_full.m did not miss any significant pathways.


# we then fitted the SEM_AP_SR.m without these non-significant pathways.
SEM_AP_SR.m <- psem(
  lm(sar_ave_slope ~ MAT_C, na.action=na.omit, data = star.data),
  
  lm(sstar_slope ~ MAP_mm, na.action=na.omit, data = star.data),
  lm(asar_slope ~ sar_ave_slope, na.action=na.omit, data = star.data),
  
  lm(estar_slope ~ sstar_slope + asar_slope, na.action=na.omit, data = star.data),
  MAT_C %~~% MAP_mm,
  data = star.data
)
# To evaluate the model
summary(SEM_AP_SR.m) # Note that all pathway were significant (P < 0.05).
fisherC(SEM_AP_SR.m) # Note that SEM_AP_SR_full.m was saturated (Fisher.c < df) and its significant estimations were very well with p > 0.05.
AIC(SEM_AP_SR.m)
dSep(SEM_AP_SR.m) # Note that SEM_AP_SR_full.m did not miss any significant pathways.

# We can chose SEM_AP_SR.m as our finial SEM.
