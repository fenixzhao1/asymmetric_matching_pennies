# Import data
library(readr)
bimatrix_choice <- read_csv("C:\\Users\\fenix\\Dropbox\\GSR\\Continuous Bimatrix\\production_4_25\\bimatrix_pilot_18-4-24.csv")

# Data filter
data <- bimatrix_choice[,]
data1 = data[which(data[,1]==132),]

# Calculate median
xmed = median(data1$p1_strategy)
ymed = median(data1$p2_strategy)

# Count bimatrix
j = 0
aa = 0
ab = 0
ba = 0
bb = 0
for (i in data1$tick){
  j = j + 1
}
for (i in 1:j){
  x = as.numeric(data1[i,4])
  y = as.numeric(data1[i,5])
  if(x>=0.5 & y>=0.5){aa=aa+1}
  if(x>=0.5 & y<0.5){ab=ab+1}
  if(x<0.5 & y>=0.5){ba=ba+1}
  if(x<0.5 & y<0.5){bb=bb+1}
}
aa=aa/j
ab=ab/j
ba=ba/j
bb=bb/j

# Make table
table <- matrix(c(ab,aa,bb,ba),ncol=2,byrow=TRUE)
colnames(table) <- c("P2 B","P2 A")
rownames(table) <- c("P1 A","P1 B")
table <- as.table(table)
table