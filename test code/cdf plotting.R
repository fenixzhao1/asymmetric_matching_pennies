# Import data
library(readr)
continuous_bimatrix_pilot_choice <- read_csv("C:\\Users\\fenix\\Dropbox\\GSR\\Stochastics Bimatrix\\pilot analysis 10_18\\Bimatrix_Pilot_10_18_2017_choices.csv")

# Data filter
data <- continuous_bimatrix_pilot_choice[,]

#plot cdf for actions by group
par(mar = c(2, 2, 2, 2))
par(mfrow=c(2,1))
data1 = data[which(data[,1]==1731),]
P = ecdf(data1$p1_strategy)
Q = ecdf(data1$p2_strategy)
plot(P, main="Row CDF", xlab="action", ylab="cdf", 
       xlim=c(0,1), ylim=c(0,1))
abline(v=median(data1$p1_strategy))
plot(Q, main="Column CDF", xlab="action", ylab="cdf", 
       xlim=c(0,1), ylim=c(0,1))
abline(v=median(data1$p2_strategy))

#plot cdf for actions by players
par(mar = c(2, 2, 2, 2))
par(mfrow=c(4,2))
x=c(1,2)
y=c(1,2)
for (j in 1:4){
  data2 = data[which(data[,1]==4 | data[,1]==5 | data[,1]==6),]
  data3 = data2[which(data2[,8]==j),]
  action = data3$p1_strategy
  if (length(action) != 0){
    CDF = ecdf(action)
    plot(CDF, main="Player's CDF as Row", xlab="action", ylab="cdf",
         xlim=c(0,1), ylim=c(0,1))
  }
  else {plot(x,y, main="Player's CDF as Row")}
  data3 = data2[which(data2[,9]==j),]
  action = data3$p2_strategy
  if (length(action) != 0){
    CDF = ecdf(action)
    plot(CDF, main="Player's CDF as Column", xlab="action", ylab="cdf",
         xlim=c(0,1), ylim=c(0,1))
  }
  else {plot(x,y, main="Player's CDF as Column")}
}