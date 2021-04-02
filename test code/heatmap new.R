# install packages
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("ggExtra")
# install.packages("plotly")
# install.packages("lattice")
# install.packages("MASS")

#  Package for heat map
require(lattice)
require(MASS)
library(lattice)
library(MASS)

# Import data
library(readr)
bimatrix_choice <- read_csv("C:\\Users\\fenix\\Dropbox\\GSR\\Continuous Bimatrix\\production_4_24\\bimatrix_pilot_18-4-24.csv")

# Data filter
data <- bimatrix_choice[,]
data1 = data[which(data[,8]==300 & data[,17]=='FALSE' & data[,21]==4),]

# Generate density function
m <- kde2d(data1$p2_strategy, data1$p1_strategy, n=100)

# Set colors in levelplot: rainbow or greyscale
n = 10
colours = rev(rainbow(n, s = 1, v = 1, start = 0, end = 0.9, alpha = 1))
#colours = rev(gray.colors(n, start = 0, end = 1, gamma = 0.8, alpha = NULL))
rgb.palette <- colorRampPalette(colours, space = "Lab")

# Change ticks in levelplot
x.scale <- list(at=seq(from = 0, to = 100, length.out = 6))
y.scale <- list(at=seq(from = 0, to = 100, length.out = 6))

# Use levelplot to generate heatmap
par(mfrow=c(1,1))
levelplot(m$z, col.regions=rgb.palette(1000), 
          scales=list(x=x.scale, y=y.scale),
          main="Bivariate Density Heatmap",
          xlab="p2_strategy", 
          ylab="p1_strategy" ,
          cuts = 1000)

# # ggplot
# library(ggplot2)
# library(dplyr)
# c <- ggplot(data1, aes(data1$p1_strategy, data1$p2_strategy))
# c + geom_bin2d(bins=10)
# 
# # other ggplot
# c + geom_density_2d()
# c + geom_point(color = "#00AFBB") + geom_density_2d(color = "#E7B800")

# # 3d surface
# library(plotly)
# m <- with(MASS::geyser, MASS::kde2d(data1$p1_strategy, data1$p2_strategy, n=100))
# m <- plot_ly(x = m$x, y = m$y, z = m$z) %>% add_surface()
