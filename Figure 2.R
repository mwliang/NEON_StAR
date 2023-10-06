rm(list=ls())
setwd("~~")

# Reading the data and checking the variable names
div_sta_full.data <- read.csv('div_sta_area_full_log2.csv',header=T)
variable.names(div_sta_full.data)

#################### transformation function
scaleFUN <- function(x) sprintf("%.1f", x)
############################################

# loading the package
library(ggplot2)

## Plotting the scatter plots
# species-area relationship
sar_ave.plot <- ggplot(div_sta_full.data, aes(y = log(sr), x = log2(N), color = siteID)) +
  geom_point(size = 2.5, alpha = 0.3) +
  geom_smooth(aes(group = siteID), method = "lm", se=F) +
  ylab(bquote(atop(paste("Plant species richness"), (NULL[log[2]*"-transformed"])))) +
  xlab(bquote(paste(Area *(NULL[log[2]*'(# plots)'])))) +
  labs(tag = "A") +
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

# ecosystem stability-area relationship
com_sta.plot <- ggplot(div_sta_full.data, aes(y = log(com_sta), x = log2(N), color = siteID)) +
  geom_point(size = 2.5, alpha = 0.3) +
  geom_smooth(aes(group = siteID), method = "lm", se=F) +
  ylab(bquote(atop(paste("Ecosystem stability"), (NULL[log[2]*"-transformed"])))) +
  xlab(bquote(paste(Area *(NULL[log[2]*'(# plots)'])))) +
  scale_y_continuous(labels=scaleFUN) +
  labs(tag = "B") +
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

# species stability-area relationship
spe_sta.plot <- ggplot(div_sta_full.data, aes(y = log(spe_sta), x = log2(N), color = siteID)) +
  geom_point(size = 2.5, alpha = 0.3) +
  geom_smooth(aes(group = siteID), method = "lm", se=F) +
  ylab(bquote(atop(paste("Species stability"), (NULL[log[2]*"-transformed"])))) +
  xlab(bquote(paste(Area *(NULL[log[2]*'(# plots)'])))) +
  scale_y_continuous(labels=scaleFUN) +
  labs(tag = "C") +
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

# species asynchrony-area relationship
spe_asyn.plot <- ggplot(div_sta_full.data, aes(y = log(spe_asyn), x = log2(N), colour = factor(siteID))) +
  geom_point(size = 2.5, alpha = 0.3) +
  geom_smooth(aes(group = siteID), method = "lm", se=F) +
  ylab(bquote(atop(paste("Species asynchrony"), (NULL[log[2]*"-transformed"])))) +
  xlab(bquote(paste(Area *(NULL[log[2]*'(# plots)'])))) +
  scale_y_continuous(labels=scaleFUN) +
  labs(tag = "C") +
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

plot_grid(sar_ave.plot, spar_comm.plot,
          com_sta.plot, asar_comm.plot, 
          nrow = 2, align= "hv")


## Plotting the histgrame 
# Reading the data and checking the variable names
star.data <- read.csv('div_sta_area.csv',header=T)
variable.names(star.data)

# Extract the names of columns 20 to 23, such as sar_ave_slope, sstar_slope, asar_slope, and estar_slope
variables <- colnames(star.data)[20:23]

# Set up a 2x2 plotting area
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(4,4,2,2))

# Loop through each variable and create a histogram
for (var in variables) {
  hist(star.data[[var]], 
       main=paste("Histogram of", var), 
       xlab=var, 
       col="gray85", 
       ylim=c(0, 11),
       tcl= -0.2,
       cex.lab=1.2,
       border="black",
       axes=FALSE)  # Suppress default axes
  
  # Add custom y-axis
  axis(side=2, at=c(0, 5, 10), las=1, tcl=-0.2, cex.axis=2)
  # Add default x-axis
  axis(side=1, tcl=-0.2, cex.axis=2)
}

# Reset plotting parameters to default
par(mfrow=c(1,1), mar=c(5,4,4,2), oma=c(0,0,0,0))
