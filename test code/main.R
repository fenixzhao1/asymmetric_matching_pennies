library(dplyr)
library(lattice)
library(MASS)
library(latticeExtra)
## load data 
bimatrix_choice <- read.csv("/Users/Cuedra/Documents/Academic/Research/bi-matrix/production_2_7/bimatrix_choice.csv", header = T)
config <- read.csv("/Users/Cuedra/Documents/Academic/Research/bi-matrix/production_2_7/config file.csv", header = T)

# create round variable in chocie data
bimatrix_choice$round =  as.double(substring(bimatrix_choice$subsession_id, 2, 3))

# merge config and choice data
full_data <- full_join(bimatrix_choice, config, by = "round")

# create group/pair id
full_data$pair.id = paste(full_data$p1_code, full_data$p2_code, sep = "_")

full_data$round.pair.id = paste(full_data$pair.id, full_data$round, sep = "_")

full_data = full_data %>% mutate(game1 = ifelse(payoff1Aa == 2,1,0))
full_data = full_data %>% mutate(game2 = ifelse(payoff1Aa == 3,1,0))
full_data = full_data %>% mutate(game3 = ifelse(payoff1Aa == 8,1,0))

full_data = full_data %>% mutate(game = ifelse(payoff1Aa == 8,"8002",ifelse(payoff1Aa == 3, "3117","2042")))


full_data = full_data %>% mutate(p1NEmix = ifelse(game == "8002", .5, ifelse(game == "3117", .33, 0)))
full_data = full_data %>% mutate(p2NEmix = ifelse(game == "8002", .2, ifelse(game == "3117", .75, 1)))

full_data = full_data %>% mutate(p1MMmix = ifelse(game == "8002", .2, ifelse(game == "3117", .75, 0)))
full_data = full_data %>% mutate(p2MMmix = ifelse(game == "8002", .5, ifelse(game == "3117", .67, 1)))

full_data = full_data %>% mutate(PC = ifelse(pure_strategy=="TRUE", ifelse(num_subperiods==0,1,0),0))

full_data = full_data %>% mutate(PD = ifelse(pure_strategy=="TRUE", ifelse(num_subperiods==15,1,0),0))

full_data = full_data %>% mutate(MC = ifelse(pure_strategy=="FALSE", ifelse(num_subperiods==0,1,0),0))

full_data = full_data %>% mutate(MD = ifelse(pure_strategy=="FALSE", ifelse(num_subperiods==15,1,0),0))

uniquePairs = unique(full_data$pair.id)

## some plotting parameters
# Set colors in levelplot: rainbow or greyscale
n = 10
colours = rev(rainbow(n, s = 1, v = 1, start = 0, end = 0.9, alpha = 1))
#colours = rev(gray.colors(n, start = 0, end = 1, gamma = 0.8, alpha = NULL))
rgb.palette <- colorRampPalette(colours, space = "Lab")

# Change ticks in levelplot
x.scale <- list(at=seq(from = 0, to = 100, length.out = 6))
y.scale <- list(at=seq(from = 0, to = 100, length.out = 6))

## loop over pairs
for(i in 1:length(uniquePairs)){
  pairData = subset(full_data, pair.id==uniquePairs[i])
  
  pairRounds = unique(pairData$round.pair.id)
  

  # loop over rounds in which pairs met
  for(j in 1:length(pairRounds)){
    pairRoundData = subset(pairData, round.pair.id==pairRounds[j])
    # Generate density function
    pairCor <- cor(pairRoundData$p2_mean_strategy, pairRoundData$p1_mean_strategy)
    
    m <- kde2d(pairRoundData$p2_mean_strategy, pairRoundData$p1_mean_strategy, h= .15, n=100)
    
    title = paste("pair_round", as.character(pairRounds[j]), "player 1",  sep = "_")
    file = paste("/Users/Cuedra/Documents/Academic/Research/bi-matrix/production_2_7/subject plots/", title, sep = "")
    file = paste(file, ".png", sep = "")
    png(file)
    #plot(ecdf(pairRoundData$p1_mean_strategy), main = paste("player 1 mean strategy in group",as.character(uniquePairs[i]), sep="-" ))
    hist(pairRoundData$p1_mean_strategy, main  = paste("pair_round", as.character(pairRounds[j]), "player 1",  sep = "_"))
    dev.off()

    title = paste("pair_round", as.character(pairRounds[j]), "player 2", sep = "_")
    file = paste("/Users/Cuedra/Documents/Academic/Research/bi-matrix/production_2_7/subject plots/", title, sep = "")
    file = paste(file, ".png", sep = "")
    png(file)
    hist(pairRoundData$p2_mean_strategy, main  = paste("pair_round", as.character(pairRounds[j]), "player 2",  sep = "_"))
    dev.off()
    
    title = paste("pair_round", as.character(pairRounds[j]), "joint dist",  sep = "_")
    file = paste("/Users/Cuedra/Documents/Academic/Research/bi-matrix/production_2_7/subject plots/", title, sep = "")
    file = paste(file, ".png", sep = "")
    heat = levelplot(m$z, col.regions=rgb.palette(1000), 
                     scales=list(x=x.scale, y=y.scale),
                     main=paste("Bivariate Density: pair cor = ", as.character(round(pairCor, digits = 2)), " game ", as.character(pairRoundData$game[1]), sep = ""),
                     xlab="p2_strategy", 
                     ylab="p1_strategy" ,
                     cuts = 1000)
    mean <- layer(panel.points(y=100*mean(pairRoundData$p1_mean_strategy), x= 100*mean(pairRoundData$p2_mean_strategy), col = "black"))
    meanText <- layer(panel.text(y=100*mean(pairRoundData$p1_mean_strategy), x= 100*mean(pairRoundData$p2_mean_strategy), labels= "mean", pos=4))
    
    NE <- layer(panel.points(y=100*mean(pairRoundData$p1NEmix), x= 100*mean(pairRoundData$p2NEmix), col = "black"))
    NEText <- layer(panel.text(y=100*mean(pairRoundData$p1NEmix), x= 100*mean(pairRoundData$p2NEmix), labels= "NE", pos=4))
    
    MM <- layer(panel.points(y=100*mean(pairRoundData$p1MMmix), x= 100*mean(pairRoundData$p2MMmix), col = "black"))
    MMText <- layer(panel.text(y=100*mean(pairRoundData$p1MMmix), x= 100*mean(pairRoundData$p2MMmix), labels= "MM", pos=4))
    
    
    fullplot = heat + mean + meanText + NE + NEText + MM + MMText
    
    png(file)
    print(fullplot)
    # plot(ecdf(pairRoundData$p2_mean_strategy), main = paste("player 2 mean strategy in group",as.character(uniquePairs[i]), sep="-" ))
    dev.off()
    #plot(pairRoundData$p1_mean_strategy, pairRoundData$p2_mean_strategy, main = as.character(pairRounds[j]))
    
  }
  
}

games =  unique(full_data$game)
treatments = c("PC","PD","MC","MD")
## loop over games
for(i in 1:length(games)){
  gameData = subset(full_data, game==games[i])
  

  PC = subset(gameData, PC==1 & is.element(tick, c(5:75)))
  PD = subset(gameData, PD==1 & is.element(tick, c(3:14)))
  MC = subset(gameData, MC==1 & is.element(tick, c(5:75)))
  MD = subset(gameData, MD==1 & is.element(tick, c(3:14)))
  
  treatmentData = list()
  treatmentData[[1]] = PC
  treatmentData[[2]] = PD
  treatmentData[[3]] = MC
  treatmentData[[4]] = MD
  
  for(j in 1:length(treatmentData)){  
  ##
  title = paste("game", as.character(games[i]), treatments[j], "row_players", sep = "_")
  file = paste("/Users/Cuedra/Documents/Academic/Research/bi-matrix/production_2_7/treatment plots/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file)
  #plot(ecdf(pairRoundData$p1_mean_strategy), main = paste("player 1 mean strategy in group",as.character(uniquePairs[i]), sep="-" ))
  hist(treatmentData[[j]]$p1_mean_strategy, main  = paste("game", as.character(games[i]), treatments[j], "row players", treatments[j],  sep = "_"))
  dev.off()
  
  title = paste("game", as.character(games[i]), treatments[j], "col_players", sep = "_")
  file = paste("/Users/Cuedra/Documents/Academic/Research/bi-matrix/production_2_7/treatment plots/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file)
  hist(treatmentData[[j]]$p2_mean_strategy, main  = paste("game", as.character(games[i]), treatments[j], "col players", treatments[j], sep = "_"))
  dev.off()
  
  m <- kde2d(treatmentData[[j]]$p2_mean_strategy, treatmentData[[j]]$p1_mean_strategy, h= .15, n=100)
  popCor = cor(treatmentData[[j]]$p2_mean_strategy, treatmentData[[j]]$p1_mean_strategy)
  
  title = paste("game", as.character(games[i]), treatments[j], "joint_dist",  sep = "_")
  file = paste("/Users/Cuedra/Documents/Academic/Research/bi-matrix/production_2_7/treatment plots/", title, sep = "")
  file = paste(file, ".png", sep = "")
  heat = levelplot(m$z, col.regions=rgb.palette(1000), 
                   scales=list(x=x.scale, y=y.scale),
                   main=paste("Bivariate Density: cor = ", as.character(round(popCor, digits = 2)), " game ", as.character(games[i]), treatments[j], sep = ""),
                   xlab="p2_strategy", 
                   ylab="p1_strategy" ,
                   cuts = 1000)
  mean <- layer(panel.points(y=100*mean(treatmentData[[j]]$p1_mean_strategy), x= 100*mean(treatmentData[[j]]$p2_mean_strategy), col = "black"))
  meanText <- layer(panel.text(y=100*mean(treatmentData[[j]]$p1_mean_strategy), x= 100*mean(treatmentData[[j]]$p2_mean_strategy), labels= "mean", pos=4))
  
  NE <- layer(panel.points(y=100*treatmentData[[j]]$p1NEmix[1], x= 100*treatmentData[[j]]$p2NEmix[1], col = "black"))
  NEText <- layer(panel.text(y=100*treatmentData[[j]]$p1NEmix[1], x= 100*treatmentData[[j]]$p2NEmix[1], labels= "NE", pos=4))
  
  MM <- layer(panel.points(y=100*treatmentData[[j]]$p1MMmix[1], x= 100*treatmentData[[j]]$p2MMmix[1], col = "black"))
  MMText <- layer(panel.text(y=100*treatmentData[[j]]$p1MMmix[1], x= 100*treatmentData[[j]]$p2MMmix[1], labels= "MM", pos=4))
  
  
  fullplot = heat + mean + meanText + NE + NEText + MM + MMText
  
  png(file)
  print(fullplot)
  # plot(ecdf(pairRoundData$p2_mean_strategy), main = paste("player 2 mean strategy in group",as.character(uniquePairs[i]), sep="-" ))
  dev.off()
  #plot(pairRoundData$p1_mean_strategy, pairRoundData$p2_mean_strategy, main = as.character(pairRounds[j]))
  }
  
  
}


## strategy over time plots

## loop over pairs
for(i in 1:length(uniquePairs)){
  pairData = subset(full_data, pair.id==uniquePairs[i])
  
  pairRounds = unique(pairData$round.pair.id)
  
  
  # loop over rounds in which pairs met
  for(j in 1:length(pairRounds)){
    pairRoundData = subset(pairData, round.pair.id==pairRounds[j])
    # Generate density function
    pairCor <- cor(pairRoundData$p2_mean_strategy, pairRoundData$p1_mean_strategy)
    
    title = paste("pair_round", as.character(pairRounds[j]), "strategy over time", "game ", as.character(pairRoundData$game[1]), sep = "_")
    file = paste("/Users/Cuedra/Documents/Academic/Research/bi-matrix/production_2_7/subject time plots/", title, sep = "")
    file = paste(file, ".png", sep = "")
    png(file)
    par(mfrow=c(2,1))
    #plot(ecdf(pairRoundData$p1_mean_strategy), main = paste("player 1 mean strategy in group",as.character(uniquePairs[i]), sep="-" ))
    plot(pairRoundData$tick, pairRoundData$p1_mean_strategy, main="player 1", xlab = NA, ylab = NA)
    plot(pairRoundData$tick, pairRoundData$p2_mean_strategy, main="player 2", xlab = "time", ylab = "strategy")
    dev.off()
    
    
  }
  
}


#####################################
## look into why hist get chopped for some subjects.
## chop off first quarter and last 5% of periods
## drop first couple periods a given subject sees the subject
## add trMean to the plot but leave mean (includes all data) to plot




#If not already installed
#install.packages("gplots")
#install.packages("devtools")

#Load necessary packages
library("gplots")
library("devtools")

#Load latest version of heatmap.3 function
source_url("https://raw.githubusercontent.com/obigriffith/biostar-tutorials/master/Heatmaps/heatmap.3.R")

#Set a working directory for output files
setwd("/Users/ogriffit/git/biostar-tutorials/Heatmaps")

#Create a fake dataset for demonstration purposes
prob_matrix=replicate(100, rnorm(20))
drug_names=paste("drug",letters[1:20],sep="_")
patient_ids=paste("patient",c(1:100),sep="_")
rownames(prob_matrix)=drug_names
colnames(prob_matrix)=patient_ids

#Create fake color side bars
drugclass_colors=sample(c("darkorchid","darkred"), length(drug_names), replace = TRUE, prob = NULL)
drugcategory_colors=sample(c("green","darkgreen"), length(drug_names), replace = TRUE, prob = NULL)
subtype_colors=sample(c("red","blue","cyan","pink","yellow","green"), length(patient_ids), replace = TRUE, prob = NULL)
Mcolors=sample(c("black","white","grey"), length(patient_ids), replace = TRUE, prob = NULL)
Ncolors=sample(c("black","white","grey"), length(patient_ids), replace = TRUE, prob = NULL)
Tcolors=sample(c("black","white","grey"), length(patient_ids), replace = TRUE, prob = NULL)
HER2colors=sample(c("black","white","grey"), length(patient_ids), replace = TRUE, prob = NULL)
PRcolors=sample(c("black","white","grey"), length(patient_ids), replace = TRUE, prob = NULL)
ERcolors=sample(c("black","white","grey"), length(patient_ids), replace = TRUE, prob = NULL)
rlab=t(cbind(drugclass_colors,drugcategory_colors))
clab=cbind(subtype_colors,Mcolors,Ncolors,Tcolors,HER2colors,PRcolors,ERcolors)
rownames(rlab)=c("Class","Category")
colnames(clab)=c("Subtype","M","N","T","HER2","PR","ER")

#Define custom dist and hclust functions for use with heatmaps
mydist=function(c) {dist(c,method="euclidian")}
myclust=function(c) {hclust(c,method="average")}

#Create heatmap using custom heatmap.3 source code loaded above
pdf(file="heatmap3_example.pdf")
main_title="Drug Response Predictions"
par(cex.main=1)
heatmap.3(prob_matrix, hclustfun=myclust, distfun=mydist, na.rm = TRUE, scale="none", dendrogram="both", margins=c(6,12),
          Rowv=TRUE, Colv=TRUE, ColSideColors=clab, RowSideColors=rlab, symbreaks=FALSE, key=TRUE, symkey=FALSE,
          density.info="none", trace="none", main=main_title, labCol=FALSE, labRow=drug_names, cexRow=1, col=rev(heat.colors(75)),
          ColSideColorsSize=7, RowSideColorsSize=2, KeyValueName="Prob. Response")
legend("topright",legend=c("Basal","LumA","LumB","Her2","Claudin","Normal","","Positive","Negative","NA","","Targeted","Chemo","","Approved","Experimental"),
       fill=c("red","blue","cyan","pink","yellow","green","white","black","white","grey","white","darkorchid","darkred","white","green","darkgreen"), border=FALSE, bty="n", y.intersp = 0.7, cex=0.7)
dev.off()

#Example to show that it now also works with just a single column or single row
mat <- matrix(1:100, byrow=T, nrow=10)
column_annotation <- sample(c("red", "blue", "green"), 10, replace=T)
column_annotation <- as.matrix(column_annotation)
colnames(column_annotation) <- c("Variable X")

row_annotation <- sample(c("red", "blue", "green"), 10, replace=T)
row_annotation <- as.matrix(t(row_annotation))
rownames(row_annotation) <- c("Variable Y")

heatmap.3(mat, RowSideColors=row_annotation, ColSideColors=column_annotation)

