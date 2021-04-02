# Import data
library(readr)
bimatrix_choice <- read_csv("C:\\Users\\fenix\\Dropbox\\GSR\\Continuous Bimatrix\\production_4_25\\bimatrix_pilot_18-4-24.csv")

# Data filter
data <- bimatrix_choice[,]

#choice-time plot by group
par(mar = c(2, 2, 2, 2))
par(mfrow=c(4,2))
for (i in 1:4){
  data1 = data[which(data[,1]==132 & data[,2]==i),]
  plot(data1$tick, data1$p1_strategy, type="l",
       xlab="time", ylab="row player action", main="Row",
       #xlim=c(0,14), 
       ylim=c(0,1), pch=20)
  plot(data1$tick, data1$p2_strategy, type="l",
       xlab="time", ylab="column player action", main="Column",
       #xlim=c(0,14), 
       ylim=c(0,1), pch=20)
}

#choice-time plot by player
par(mar = c(2, 2, 2, 2))
par(mfrow=c(3,2))
for (j in 7:9){
  data2 = data[which(data[,8]==8 & data[,1]==j),]
  plot(data2$tick, data2$p1_strategy, type="p",
      xlab="time", ylab="row player action", main="Row",
      xlim=c(1,119), ylim=c(0,1), pch=20)
  data2 = data[which(data[,9]==8 & data[,1]==j),]
  plot(data2$tick, data2$p2_strategy, type="p",
      xlab="time", ylab="column player action", main="Column",
      xlim=c(1,119), ylim=c(0,1), pch=20)
}