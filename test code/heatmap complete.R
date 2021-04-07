##########Data preparation##########
# load packages
library(dplyr)
library(lattice)
library(MASS)
library(latticeExtra)

## load data 
bimatrix_choice <- read.csv("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/production_5_22/bimatrix_pilot_18-5-22.csv", header = T)

# create round variable in choice data and create full dataset
bimatrix_choice$round = as.double(substring(bimatrix_choice$subsession_id, 2, 3))
full_data <- bimatrix_choice

# create round/pair id
full_data$pair.id = paste(full_data$p1_code, full_data$p2_code, sep = "_")
full_data$round.pair.id = paste(full_data$round, full_data$pair.id,  sep = "_")

# add group variables
full_data = full_data %>% mutate(game1 = ifelse(payoff1Aa == 200,1,0))
full_data = full_data %>% mutate(game2 = ifelse(payoff1Aa == 300,1,0))
full_data = full_data %>% mutate(game3 = ifelse(payoff1Aa == 800,1,0))
full_data = full_data %>% mutate(game = ifelse(payoff1Aa == 800,"8002",ifelse(payoff1Aa == 300, "3117","2042")))
full_data = filter(full_data, game !='2042')

full_data = full_data %>% mutate(p1NEmix = ifelse(game == "8002", .5, ifelse(game == "3117", .33, 0)))
full_data = full_data %>% mutate(p2NEmix = ifelse(game == "8002", .2, ifelse(game == "3117", .75, 1)))
full_data = full_data %>% mutate(p1MMmix = ifelse(game == "8002", .2, ifelse(game == "3117", .75, 0)))
full_data = full_data %>% mutate(p2MMmix = ifelse(game == "8002", .5, ifelse(game == "3117", .67, 1)))

full_data = full_data %>% mutate(PC = ifelse(pure_strategy=="TRUE", ifelse(num_subperiods==0,1,0),0))
full_data = full_data %>% mutate(PD = ifelse(pure_strategy=="TRUE", ifelse(num_subperiods==15,1,0),0))
full_data = full_data %>% mutate(MC = ifelse(pure_strategy=="FALSE", ifelse(num_subperiods==0,1,0),0))
full_data = full_data %>% mutate(MD = ifelse(pure_strategy=="FALSE", ifelse(num_subperiods==15,1,0),0))

# payoff and time remaining
full_data = full_data %>% mutate(p1_payoff = payoff1Aa*p1_strategy*p2_strategy + payoff1Ab*p1_strategy*(1-p2_strategy) + payoff1Ba*(1-p1_strategy)*p2_strategy + payoff1Bb*(1-p1_strategy)*(1-p2_strategy))
full_data = full_data %>% mutate(p2_payoff = payoff2Aa*p1_strategy*p2_strategy + payoff2Ab*p1_strategy*(1-p2_strategy) + payoff2Ba*(1-p1_strategy)*p2_strategy + payoff2Bb*(1-p1_strategy)*(1-p2_strategy))
full_data = full_data %>% mutate(timeleft = ifelse(num_subperiods==15, 14-tick, 179-tick))

# deviation from NE and MM
full_data = full_data %>% mutate(strategy_diff = abs(p1_strategy - p2_strategy))
full_data = full_data %>% mutate(NEdiff = (p1_strategy - p1NEmix)^2 + (p2_strategy - p2NEmix)^2)
full_data = full_data %>% mutate(MMdiff = (p1_strategy - p1MMmix)^2 + (p2_strategy - p2MMmix)^2)
full_data = full_data %>% mutate(p1NEdiff = (p1_strategy - p1NEmix)^2)
full_data = full_data %>% mutate(p1MMdiff = (p1_strategy - p1MMmix)^2)
full_data = full_data %>% mutate(p2NEdiff = (p2_strategy - p2NEmix)^2)
full_data = full_data %>% mutate(p2MMdiff = (p2_strategy - p2MMmix)^2)

uniquePairs = unique(full_data$pair.id)

## Heatmap parameters
# Set colors in levelplot: rainbow or greyscale
n = 10
colours = rev(rainbow(n, s = 1, v = 1, start = 0, end = 0.9, alpha = 1))
#colours = rev(gray.colors(n, start = 0, end = 1, gamma = 0.8, alpha = NULL))
rgb.palette <- colorRampPalette(colours, space = "Lab")

# Change ticks in levelplot
x.scale <- list(at=seq(from = 0, to = 100, length.out = 6))
y.scale <- list(at=seq(from = 0, to = 100, length.out = 6))

##########Analysis 1: heatmap and choice density by pairs##########
for(i in 1:length(uniquePairs)){
  pairData = subset(full_data, pair.id==uniquePairs[i])
  
  pairRounds = unique(pairData$round.pair.id)
  
  # loop over rounds in which pairs met
  for(j in 1:length(pairRounds)){
    pairRoundData = subset(pairData, round.pair.id==pairRounds[j])
    # Generate density function
    pairCor <- cor(pairRoundData$p2_strategy, pairRoundData$p1_strategy)
    
    m <- kde2d(pairRoundData$p2_strategy, pairRoundData$p1_strategy, h= .15, n=100)
    
    title = paste("pair_round", as.character(pairRounds[j]), "player 1",  sep = "_")
    file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/production_5_22/subject plots/", title, sep = "")
    file = paste(file, ".png", sep = "")
    png(file)
    #plot(ecdf(pairRoundData$p1_strategy), main = paste("player 1 mean strategy in group",as.character(uniquePairs[i]), sep="-" ))
    hist(pairRoundData$p1_strategy, main  = paste("pair_round", as.character(pairRounds[j]), "player 1",  sep = "_"))
    dev.off()
    
    title = paste("pair_round", as.character(pairRounds[j]), "player 2", sep = "_")
    file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/production_5_22/subject plots/", title, sep = "")
    file = paste(file, ".png", sep = "")
    png(file)
    hist(pairRoundData$p2_strategy, main  = paste("pair_round", as.character(pairRounds[j]), "player 2",  sep = "_"))
    dev.off()
    
    title = paste("pair_round", as.character(pairRounds[j]), "joint dist",  sep = "_")
    file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/production_5_22/subject plots/", title, sep = "")
    file = paste(file, ".png", sep = "")
    heat = levelplot(m$z, col.regions=rgb.palette(1000), 
                     scales=list(x=x.scale, y=y.scale),
                     main=paste("Bivariate Density: pair cor = ", as.character(round(pairCor, digits = 2)), " game ", as.character(pairRoundData$game[1]), sep = ""),
                     xlab="p2_strategy", 
                     ylab="p1_strategy" ,
                     cuts = 1000)
    mean <- layer(panel.points(y=100*mean(pairRoundData$p1_strategy), x= 100*mean(pairRoundData$p2_strategy), col = "black"))
    meanText <- layer(panel.text(y=100*mean(pairRoundData$p1_strategy), x= 100*mean(pairRoundData$p2_strategy), labels= "mean", pos=4))
    
    NE <- layer(panel.points(y=100*mean(pairRoundData$p1NEmix), x= 100*mean(pairRoundData$p2NEmix), col = "black"))
    NEText <- layer(panel.text(y=100*mean(pairRoundData$p1NEmix), x= 100*mean(pairRoundData$p2NEmix), labels= "NE", pos=4))
    
    MM <- layer(panel.points(y=100*mean(pairRoundData$p1MMmix), x= 100*mean(pairRoundData$p2MMmix), col = "black"))
    MMText <- layer(panel.text(y=100*mean(pairRoundData$p1MMmix), x= 100*mean(pairRoundData$p2MMmix), labels= "MM", pos=4))
    
    
    fullplot = heat + mean + meanText + NE + NEText + MM + MMText
    
    png(file)
    print(fullplot)
    # plot(ecdf(pairRoundData$p2_strategy), main = paste("player 2 mean strategy in group",as.character(uniquePairs[i]), sep="-" ))
    dev.off()
    #plot(pairRoundData$p1_strategy, pairRoundData$p2_mean_strategy, main = as.character(pairRounds[j]))
  }
}

##########Analysis 2: heatmap and choice density by treatments##########
games =  unique(full_data$game)
treatments = c("PC","PD","MC","MD","show_nothing","show_worst","show_best","show_worstbest")

# loop over games
for(i in 1:2){
  gameData = subset(full_data, game==games[i])
  
  PC = subset(gameData, PC==1 & is.element(tick, c(20:160)))
  PD = subset(gameData, PD==1 & is.element(tick, c(3:13)))
  MC = subset(gameData, MC==1 & is.element(tick, c(20:160)))
  MD = subset(gameData, MD==1 & is.element(tick, c(3:13)))
  show_nothing = subset(gameData, show_at_worst=="FALSE" & show_best_response=="FALSE")
  show_worst = subset(gameData, show_at_worst=="TRUE" & show_best_response=="FALSE")
  show_best = subset(gameData, show_at_worst=="FALSE" & show_best_response=="TRUE")
  show_worstbest = subset(gameData, show_at_worst=="TRUE" & show_best_response=="TRUE")
  
  treatmentData = list()
  treatmentData[[1]] = PC
  treatmentData[[2]] = PD
  treatmentData[[3]] = MC
  treatmentData[[4]] = MD
  treatmentData[[5]] = show_nothing
  treatmentData[[6]] = show_worst
  treatmentData[[7]] = show_best
  treatmentData[[8]] = show_worstbest
  
  for(j in 1:length(treatmentData)){  
    
    title = paste("game", as.character(games[i]), treatments[j], "row_players", sep = "_")
    file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/production_aggregate/treatment plots/", title, sep = "")
    file = paste(file, ".png", sep = "")
    png(file)
    #plot(ecdf(pairRoundData$p1_strategy), main = paste("player 1 mean strategy in group",as.character(uniquePairs[i]), sep="-" ))
    hist(treatmentData[[j]]$p1_strategy, main  = paste("game", as.character(games[i]), treatments[j], "row players", treatments[j],  sep = "_"))
    dev.off()
    
    title = paste("game", as.character(games[i]), treatments[j], "col_players", sep = "_")
    file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/production_aggregate/treatment plots/", title, sep = "")
    file = paste(file, ".png", sep = "")
    png(file)
    hist(treatmentData[[j]]$p2_strategy, main  = paste("game", as.character(games[i]), treatments[j], "col players", treatments[j], sep = "_"))
    dev.off()
    
    m <- kde2d(treatmentData[[j]]$p2_strategy, treatmentData[[j]]$p1_strategy, h= .15, n=100)
    popCor = cor(treatmentData[[j]]$p2_strategy, treatmentData[[j]]$p1_strategy)
    
    title = paste("game", as.character(games[i]), treatments[j], "joint_dist",  sep = "_")
    file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/production_aggregate/treatment plots/", title, sep = "")
    file = paste(file, ".png", sep = "")
    heat = levelplot(m$z, col.regions=rgb.palette(1000), 
                     scales=list(x=x.scale, y=y.scale),
                     main=paste("Bivariate Density: cor = ", as.character(round(popCor, digits = 2)), " game ", as.character(games[i]), treatments[j], sep = ""),
                     xlab="p2_strategy", 
                     ylab="p1_strategy" ,
                     cuts = 1000)
    mean <- layer(panel.points(y=100*mean(treatmentData[[j]]$p1_strategy), x= 100*mean(treatmentData[[j]]$p2_strategy), col = "black"))
    meanText <- layer(panel.text(y=100*mean(treatmentData[[j]]$p1_strategy), x= 100*mean(treatmentData[[j]]$p2_strategy), labels= "mean", pos=4))
    
    NE <- layer(panel.points(y=100*treatmentData[[j]]$p1NEmix[1], x= 100*treatmentData[[j]]$p2NEmix[1], col = "black"))
    NEText <- layer(panel.text(y=100*treatmentData[[j]]$p1NEmix[1], x= 100*treatmentData[[j]]$p2NEmix[1], labels= "NE", pos=4))
    
    MM <- layer(panel.points(y=100*treatmentData[[j]]$p1MMmix[1], x= 100*treatmentData[[j]]$p2MMmix[1], col = "black"))
    MMText <- layer(panel.text(y=100*treatmentData[[j]]$p1MMmix[1], x= 100*treatmentData[[j]]$p2MMmix[1], labels= "MM", pos=4))
    
    
    fullplot = heat + mean + meanText + NE + NEText + MM + MMText
    
    png(file)
    print(fullplot)
    #plot(ecdf(pairRoundData$p2_strategy), main = paste("player 2 mean strategy in group",as.character(uniquePairs[i]), sep="-" ))
    dev.off()
    #plot(pairRoundData$p1_strategy, pairRoundData$p2_strategy, main = as.character(pairRounds[j]))
  }
}

##########Analysis 3: payoff and choice over time##########
# loop over pairs
for(i in 1:length(uniquePairs)){
  pairData = subset(full_data, pair.id==uniquePairs[i])
  pairRounds = unique(pairData$round.pair.id)
  
  # loop over rounds in which pairs met
  for(j in 1:length(pairRounds)){
    pairRoundData = subset(pairData, round.pair.id==pairRounds[j])
    # Generate density function
    pairCor <- cor(pairRoundData$p2_strategy, pairRoundData$p1_strategy)
    
    title = paste("pair_round", as.character(pairRounds[j]), "strategy over time", "game ", as.character(pairRoundData$game[1]), sep = "_")
    file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/production_5_22/subject time plots/", title, sep = "")
    file = paste(file, ".png", sep = "")
    png(file)
    par(mfrow=c(2,2))
    #plot(ecdf(pairRoundData$p1_mean_strategy), main = paste("player 1 mean strategy in group",as.character(uniquePairs[i]), sep="-" ))
    plot(pairRoundData$tick, pairRoundData$p1_strategy, type='l', col='red', main="player 1", xlab = "time", ylab = "strategy")
    plot(pairRoundData$tick, pairRoundData$p2_strategy, type='l', col='red', main="player 2", xlab = "time", ylab = "strategy")
    plot(pairRoundData$tick, pairRoundData$p1_payoff, type='l', col='blue', main="player 1", xlab = "time", ylab = "payoff")
    plot(pairRoundData$tick, pairRoundData$p2_payoff, type='l', col='blue', main="player 2", xlab = "time", ylab = "payoff")   
    dev.off()
  }
}

##########Analysis 4: Strategy over time and equilibrium##########
# loop over pairs
for(i in 1:length(uniquePairs)){
  pairData = subset(full_data, pair.id==uniquePairs[i])
  pairRounds = unique(pairData$round.pair.id)
  
  # loop over rounds in which pairs met
  for(j in 1:length(pairRounds)){
    pairRoundData = subset(pairData, round.pair.id==pairRounds[j])
    # Generate density function
    pairCor <- cor(pairRoundData$p2_strategy, pairRoundData$p1_strategy)
    
    title = paste("pair_round", as.character(pairRounds[j]), "choice over time", "game ", as.character(pairRoundData$game[1]), sep = "_")
    file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/production_5_22/strategy and equilibrium plots/", title, sep = "")
    file = paste(file, ".png", sep = "")
    png(file)
    #par(mfrow=c(2,1))
    plot(pairRoundData$tick, pairRoundData$p1_strategy, type='l', col='red', xlab = "time", ylab = "strategy", ylim=c(0:1), main = as.character(round(pairCor, digits = 2)))
    if (pairRoundData$game=="8002"){
      abline(h=0.2)
    }
    else {abline(h=0.5)}
    lines(pairRoundData$tick, pairRoundData$p2_strategy, type='l', col='blue', xlab = "time", ylab = "strategy", ylim=c(0:1))
    if (pairRoundData$game=="8002"){
      abline(h=0.5)
    }
    else {abline(h=0.86)}
    #lines(pairRoundData$tick, pairRoundData$strategy_diff, type='l', main="Choice over time" , xlab = "time", ylab = "strategy", lty=1)
  dev.off()
  }
}

#####Analysis 5: distance between p1 and p2 strategies#####
# loop over pairs
for(i in 1:length(uniquePairs)){
  pairData = subset(full_data, pair.id==uniquePairs[i])
  pairRounds = unique(pairData$round.pair.id)
  
  # loop over rounds in which pairs met
  for(j in 1:length(pairRounds)){
    pairRoundData = subset(pairData, round.pair.id==pairRounds[j])
    # Generate density function
    pairCor <- cor(pairRoundData$p2_strategy, pairRoundData$p1_strategy)
    
    title = paste("pair_round", as.character(pairRounds[j]), "choice over time", "game ", as.character(pairRoundData$game[1]), sep = "_")
    file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/production_5_22/strategy difference plots/", title, sep = "")
    file = paste(file, ".png", sep = "")
    png(file)
    #par(mfrow=c(2,1))
    plot(pairRoundData$tick, pairRoundData$p1_strategy, type='l', col='red', xlab = "time", ylab = "strategy", ylim=c(0:1), main = as.character(round(pairCor, digits = 2)))
    lines(pairRoundData$tick, pairRoundData$p2_strategy, type='l', col='blue', xlab = "time", ylab = "strategy", ylim=c(0:1))
    lines(pairRoundData$tick, pairRoundData$strategy_diff, type='l', main="Choice over time" , xlab = "time", ylab = "strategy", ylim=c(0:1))
    dev.off()
  }
}
