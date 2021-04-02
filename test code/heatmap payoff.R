#  Package for heat map
require(lattice)
require(MASS)
library(lattice)
library(MASS)

# Import data
library(readr)
continuous_bimatrix_pilot_choice <- read_csv("C:\\Users\\fenix\\Dropbox\\GSR\\Stochastics Bimatrix\\pilot analysis 10_18\\Bimatrix_Pilot_10_18_2017_choices.csv")

# Data filter
data <- continuous_bimatrix_pilot_choice[,]
data1 = data[which(data[,1]==1724),]

# Generate row player's payoff matrix in levelplot, change with payoff matrix in experiment
x = seq(from = 1, to = 0, length.out = 100)
y = seq(from = 0, to = 1, length.out = 100)
z = matrix(0, nrow=100, ncol=100)
for (i in 1:100){
  for (j in 1:100){
    z[i,j] =3*x[i]*y[j] + 0*x[i]*(1-y[j]) + 5*(1-x[i])*y[j] + 1*(1-x[i])*(1-y[j])
  }
}

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
levelplot(z, col.regions=rgb.palette(1000), 
          scales=list(x=x.scale, y=y.scale),
          main="Row Player's Payoff Table",
          xlab="column_action", 
          ylab="row_action" ,
          cuts = 1000)

#parallel data on the heat map, use data
par(new=TRUE)
par(mar = c(3.0, 2.8, 2.65, 3.2))
require(plotrix)
sizeplot(data1$column_action, data1$row_action, xlab='', ylab='', axes=FALSE, asp=1, col="black", pch=1, powscale=FALSE, size=c(0.5,3))


# #  Package for heat map
# require(lattice)
# require(MASS)
# library(lattice)
# library(MASS)
# 
# # Import data
# library(readr)
# continuous_bimatrix_pilot_choice <- read_csv("C:/Users/fenix/Desktop/PhD Economics/GSR/Stochastics Bimatrix/continuous_bimatrix_pilot_choice.csv")
# 
# # Data filter, here only pick data in round 7
# data <- continuous_bimatrix_pilot_choice[,]
# data1 = data[which(data[,1]==7),]
# 
# # Generate column player's payoff matrix in levelplot
# z = matrix(0, nrow=100, ncol=100)
# for (i in 1:100){
#   for (j in 1:100){
#     z[i,j] = 200*(1-x[i])*y[j] + 200*x[i]*(1-y[j])
#   }
# }
# 
# # Set colors in levelplot: rainbow or greyscale
# n = 10
# colours = rev(rainbow(n, s = 1, v = 1, start = 0, end = 0.9, alpha = 1))
# #colours = rev(gray.colors(n, start = 0, end = 1, gamma = 0.8, alpha = NULL))
# rgb.palette <- colorRampPalette(colours, space = "Lab")
# 
# # Change ticks in levelplot
# x.scale <- list(at=seq(from = 0, to = 100, length.out = 6))
# y.scale <- list(at=seq(from = 0, to = 100, length.out = 6))
# 
# # Use levelplot to generate heatmap
# par(mfrow=c(1,1))
# levelplot(z, col.regions=rgb.palette(1000), 
#           scales=list(x=x.scale, y=y.scale),
#           main="Column Player's Payoff Table",
#           xlab="column_action", 
#           ylab="row_action" ,
#           cuts = 1000)
# 
# #parallel data on the heat map, use data
# par(new=TRUE)
# par(mar = c(3.1, 2.5, 2.65, 3.6))
# plot(data1$column_action, data1$row_action, xlab='', ylab='', axes=FALSE, asp=1, col="black", pch=1)

