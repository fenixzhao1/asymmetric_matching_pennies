##########Data preparation##########
# load packages
library(MASS)
library(ggplot2)
library(dplyr)
library(lattice)
library(latticeExtra)
library(scatterplot3d)
library(rgl)
library(plot3D)
library(csv)
library(foreign)

# load data 
bimatrix_choice <- read.csv("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/data_rp.csv"
                            , header = T)

# create round variable in choice data and create full dataset
bimatrix_choice$round = as.double(substring(bimatrix_choice$subsession_id, 3, 4))
full_data = bimatrix_choice
full_data = arrange(full_data, full_data$session_code, full_data$subsession_id, full_data$id_in_subsession, full_data$tick)
rm(bimatrix_choice)

# create round/pair id
full_data$pair_id = paste(full_data$p1_code, full_data$p2_code, sep = "_")
full_data$round_pair_id = paste(full_data$round, full_data$pair_id,  sep = "_")
full_data$session_round_pair_id = paste(full_data$session_code, full_data$round_pair_id, sep = "_")
full_data$session_round_id = paste(full_data$session_code, full_data$round, sep = "_")

# drop Nah data
full_data = filter(full_data, tick > 1 | num_subperiods == 15)

# add treatment variables
full_data = full_data %>% mutate(game_AMPa = ifelse(payoff1Aa == 800,1,0))
full_data = full_data %>% mutate(game_AMPb = ifelse(payoff1Aa == 300,1,0))
full_data = full_data %>% mutate(game_idds = ifelse(payoff1Aa == 200,1,0))
full_data = full_data %>% mutate(game = ifelse(payoff1Aa == 800,"AMPa",ifelse(payoff1Aa == 300, "AMPb","IDDS")))

full_data = full_data %>% mutate(PC = ifelse(pure_strategy=="TRUE", ifelse(num_subperiods==0,1,0),0))
full_data = full_data %>% mutate(PD = ifelse(pure_strategy=="TRUE", ifelse(num_subperiods==15,1,0),0))
full_data = full_data %>% mutate(MC = ifelse(pure_strategy=="FALSE", ifelse(num_subperiods==0,1,0),0))
full_data = full_data %>% mutate(MD = ifelse(pure_strategy=="FALSE", ifelse(num_subperiods==15,1,0),0))

full_data = full_data %>% mutate(dummy_continuous = ifelse(num_subperiods==0, 1, 0))
full_data = full_data %>% mutate(dummy_pure = ifelse(pure_strategy=="TRUE", 1, 0))
full_data = full_data %>% mutate(dummy_mm = ifelse(mean_matching=='TRUE', 1, 0))
full_data = full_data %>% mutate(dummy_AMPa = ifelse(game=='AMPa', 1, 0))
full_data = full_data %>% mutate(dummy_IDDS = ifelse(game=='IDDS', 1, 0))

full_data = full_data %>% mutate(
  time = ifelse(num_subperiods==0, 'C', 'D'),
  match = ifelse(mean_matching==TRUE, 'mm', 'rp'),
  actionsets = ifelse(pure_strategy==TRUE, 'P', 'M'),
  treatmentfull = paste(game, actionsets, time, match, sep = '_'))

# add NE and MM info
full_data = full_data %>% mutate(p1NEmix = ifelse(game == "AMPa", .5, ifelse(game == "AMPb", .33, 0)))
full_data = full_data %>% mutate(p2NEmix = ifelse(game == "AMPa", .2, ifelse(game == 'AMPb', .75, 1)))
full_data = full_data %>% mutate(p1MMmix = ifelse(game == "AMPa", .2, ifelse(game == "AMPb", .75, 0)))
full_data = full_data %>% mutate(p2MMmix = ifelse(game == "AMPa", .5, ifelse(game == 'AMPb', .67, 1)))

# prepare for merge data: mean and median
full_data = full_data %>% mutate(p1_average = p1_strategy)
full_data = full_data %>% mutate(p2_average = p2_strategy)
full_data = full_data %>% mutate(p1_median = p1_strategy)
full_data = full_data %>% mutate(p2_median = p2_strategy)

# payoff and time remaining
full_data = full_data %>% mutate(p1_payoff = payoff1Aa*p1_strategy*p2_strategy + payoff1Ab*p1_strategy*(1-p2_strategy) + payoff1Ba*(1-p1_strategy)*p2_strategy + payoff1Bb*(1-p1_strategy)*(1-p2_strategy))
full_data = full_data %>% mutate(p2_payoff = payoff2Aa*p1_strategy*p2_strategy + payoff2Ab*p1_strategy*(1-p2_strategy) + payoff2Ba*(1-p1_strategy)*p2_strategy + payoff2Bb*(1-p1_strategy)*(1-p2_strategy))
full_data = full_data %>% mutate(timeleft = ifelse(num_subperiods==15, 14-tick, 179-tick))

# deviation from NE and MM
full_data = full_data %>% mutate(strategy_diff = abs(p1_strategy - p2_strategy))
full_data = full_data %>% mutate(NEdiff = sqrt((p1_strategy - p1NEmix)^2 + (p2_strategy - p2NEmix)^2))
full_data = full_data %>% mutate(MMdiff = sqrt((p1_strategy - p1MMmix)^2 + (p2_strategy - p2MMmix)^2))
full_data = full_data %>% mutate(p1NEdiff = sqrt((p1_strategy - p1NEmix)^2))
full_data = full_data %>% mutate(p1MMdiff = sqrt((p1_strategy - p1MMmix)^2))
full_data = full_data %>% mutate(p2NEdiff = sqrt((p2_strategy - p2NEmix)^2))
full_data = full_data %>% mutate(p2MMdiff = sqrt((p2_strategy - p2MMmix)^2))
full_data = full_data %>% mutate(p1NEdiffsgn = p1_strategy - p1NEmix)
full_data = full_data %>% mutate(p1MMdiffsgn = p1_strategy - p1MMmix)
full_data = full_data %>% mutate(p2NEdiffsgn = p2_strategy - p2NEmix)
full_data = full_data %>% mutate(p2MMdiffsgn = p2_strategy - p2MMmix)

# create a new treatment variable combining games, action sets and time
full_data = full_data %>% mutate(treatment = 0)
for(m in 1:length(full_data$tick)){
  if (full_data$game[m]=="AMPa" & full_data$PD[m]==1){full_data$treatment[m] = 1}
  if (full_data$game[m]=="AMPa" & full_data$PC[m]==1){full_data$treatment[m] = 2}
  if (full_data$game[m]=="AMPa" & full_data$MD[m]==1){full_data$treatment[m] = 3}
  if (full_data$game[m]=="AMPa" & full_data$MC[m]==1){full_data$treatment[m] = 4}
  
  if (full_data$game[m]=="AMPb" & full_data$PD[m]==1){full_data$treatment[m] = 5}
  if (full_data$game[m]=="AMPb" & full_data$PC[m]==1){full_data$treatment[m] = 6}
  if (full_data$game[m]=="AMPb" & full_data$MD[m]==1){full_data$treatment[m] = 7}
  if (full_data$game[m]=="AMPb" & full_data$MC[m]==1){full_data$treatment[m] = 8}
  
  if (full_data$game[m]=="IDDS" & full_data$PD[m]==1){full_data$treatment[m] = 9}
  if (full_data$game[m]=="IDDS" & full_data$PC[m]==1){full_data$treatment[m] = 10}
  if (full_data$game[m]=="IDDS" & full_data$MD[m]==1){full_data$treatment[m] = 11}
  if (full_data$game[m]=="IDDS" & full_data$MC[m]==1){full_data$treatment[m] = 12}
}

# create a new treatment variable combining matching, action sets and time
full_data = full_data %>% mutate(treatment2 = 0)
for(m in 1:length(full_data$tick)){
  if (full_data$mean_matching[m]=='FALSE' & full_data$PD[m]==1){full_data$treatment2[m] = 1}
  if (full_data$mean_matching[m]=='FALSE' & full_data$PC[m]==1){full_data$treatment2[m] = 2}
  if (full_data$mean_matching[m]=='FALSE' & full_data$MD[m]==1){full_data$treatment2[m] = 3}
  if (full_data$mean_matching[m]=='FALSE' & full_data$MC[m]==1){full_data$treatment2[m] = 4}
  
  if (full_data$mean_matching[m]=='TRUE' & full_data$PD[m]==1){full_data$treatment2[m] = 5}
  if (full_data$mean_matching[m]=='TRUE' & full_data$PC[m]==1){full_data$treatment2[m] = 6}
  if (full_data$mean_matching[m]=='TRUE' & full_data$MD[m]==1){full_data$treatment2[m] = 7}
  if (full_data$mean_matching[m]=='TRUE' & full_data$MC[m]==1){full_data$treatment2[m] = 8}
}

# drop first 3 subperiod or first 18 seconds
full_data = subset(full_data, tick>=3)
full_data = filter(full_data, tick>=36 | num_subperiods==15)

# drop first period of each block
full_data = filter(full_data, round!=1 & round!=7 & round!=13 & round!=19 & round!=25)

# create merge data
merge_data = full_data
write.csv(merge_data, "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/merge_rp.csv")
rm(merge_data)

# create new action type
full_data = full_data %>% mutate(strategy_profile = 0)
for (m in 1:length(full_data$tick)){
  if ((full_data$p1_strategy[m] >= full_data$p1NEmix[m]) & (full_data$p2_strategy[m] > full_data$p2NEmix[m]))
  {full_data$strategy_profile[m] = 'AA'}
  if ((full_data$p1_strategy[m] > full_data$p1NEmix[m]) & (full_data$p2_strategy[m] <= full_data$p2NEmix[m]))
  {full_data$strategy_profile[m] = 'AB'}
  if ((full_data$p1_strategy[m] <= full_data$p1NEmix[m]) & (full_data$p2_strategy[m] < full_data$p2NEmix[m]))
  {full_data$strategy_profile[m] = 'BB'}
  if ((full_data$p1_strategy[m] < full_data$p1NEmix[m]) & (full_data$p2_strategy[m] >= full_data$p2NEmix[m]))
  {full_data$strategy_profile[m] = 'BA'}
}

# create unique ID
uniqueID = unique(full_data$session_round_pair_id)
gametype = unique(full_data$game)
uniquePlayer = union(unique(full_data$p1_code), unique(full_data$p2_code))


##########Directional learning preparation##########
# regret and directional learning variables
full_data = full_data %>% mutate(p1_uhat_a1 = payoff1Aa*p2_strategy + payoff1Ab*(1-p2_strategy))
full_data = full_data %>% mutate(p1_uhat_a0 = payoff1Ba*p2_strategy + payoff1Bb*(1-p2_strategy))
full_data = full_data %>% mutate(p1_ahat = ifelse(p1_uhat_a1 > p1_uhat_a0, 1, ifelse(p1_uhat_a1 < p1_uhat_a0, 0, p1_strategy)))
full_data = full_data %>% mutate(p1_uhat = payoff1Aa*p1_ahat*p2_strategy + payoff1Ab*p1_ahat*(1-p2_strategy) + payoff1Ba*(1-p1_ahat)*p2_strategy + payoff1Bb*(1-p1_ahat)*(1-p2_strategy))
#full_data = full_data %>% mutate(p1_regret = (p1_uhat - p1_payoff)/100)
full_data = full_data %>% mutate(p1_regret = ifelse(game=='AMPa', (p1_uhat - p1_payoff)/800, ifelse(game=='AMPb', (p1_uhat - p1_payoff)/700, (p1_uhat - p1_payoff)/400)))
full_data = full_data %>% mutate(p1_direction = p1_ahat - p1_strategy)
full_data = full_data %>% mutate(p1_sign = ifelse(p1_direction > 0, 1, ifelse(p1_direction < 0, -1, 0)))
full_data = full_data %>% mutate(p1_regret_sign = p1_regret * p1_sign)

full_data = full_data %>% mutate(p2_uhat_a1 = payoff2Aa*p1_strategy + payoff2Ba*(1-p1_strategy))
full_data = full_data %>% mutate(p2_uhat_a0 = payoff2Ab*p1_strategy + payoff2Bb*(1-p1_strategy))
full_data = full_data %>% mutate(p2_ahat = ifelse(p2_uhat_a1 > p2_uhat_a0, 1, ifelse(p2_uhat_a1 < p2_uhat_a0, 0, p2_strategy)))
full_data = full_data %>% mutate(p2_uhat = payoff2Aa*p1_strategy*p2_ahat + payoff2Ab*p1_strategy*(1-p2_ahat) + payoff2Ba*(1-p1_strategy)*p2_ahat + payoff2Bb*(1-p1_strategy)*(1-p2_ahat))
#full_data = full_data %>% mutate(p2_regret = (p2_uhat - p2_payoff)/100)
full_data = full_data %>% mutate(p2_regret = ifelse(game=='AMPa', (p2_uhat - p2_payoff)/200, ifelse(game=='AMPb', (p2_uhat - p2_payoff)/300, (p2_uhat - p2_payoff)/600)))
full_data = full_data %>% mutate(p2_direction = p2_ahat - p2_strategy)
full_data = full_data %>% mutate(p2_sign = ifelse(p2_direction > 0, 1, ifelse(p2_direction < 0, -1, 0)))
full_data = full_data %>% mutate(p2_regret_sign = p2_regret * p2_sign)

# add interactive dummies
full_data = full_data %>% mutate(p1_regret_sign_pure = p1_regret_sign * dummy_pure)
full_data = full_data %>% mutate(p1_regret_sign_AMPa = p1_regret_sign * dummy_AMPa)
full_data = full_data %>% mutate(p1_regret_sign_IDDS = p1_regret_sign * dummy_IDDS)
full_data = full_data %>% mutate(p1_regret_sign_mm = p1_regret_sign * dummy_mm)
full_data = full_data %>% mutate(p1_regret_sign_pure_mm = p1_regret_sign * dummy_pure * dummy_mm)
full_data = full_data %>% mutate(p1_regret_sign_pure_AMPa = p1_regret_sign * dummy_pure * dummy_AMPa)
full_data = full_data %>% mutate(p1_regret_sign_pure_IDDS = p1_regret_sign * dummy_pure * dummy_IDDS)
full_data = full_data %>% mutate(p1_regret_sign_mm_AMPa = p1_regret_sign * dummy_mm * dummy_AMPa)
full_data = full_data %>% mutate(p1_regret_sign_mm_IDDS = p1_regret_sign * dummy_mm * dummy_IDDS)
full_data = full_data %>% mutate(p1_regret_sign_pure_mm_AMPa = p1_regret_sign * dummy_pure * dummy_mm * dummy_AMPa)
full_data = full_data %>% mutate(p1_regret_sign_pure_mm_IDDS = p1_regret_sign * dummy_pure * dummy_mm * dummy_IDDS)

full_data = full_data %>% mutate(p2_regret_sign_pure = p2_regret_sign * dummy_pure)
full_data = full_data %>% mutate(p2_regret_sign_AMPa = p2_regret_sign * dummy_AMPa)
full_data = full_data %>% mutate(p2_regret_sign_IDDS = p2_regret_sign * dummy_IDDS)
full_data = full_data %>% mutate(p2_regret_sign_mm = p2_regret_sign * dummy_mm)
full_data = full_data %>% mutate(p2_regret_sign_pure_mm = p2_regret_sign * dummy_pure * dummy_mm)
full_data = full_data %>% mutate(p2_regret_sign_pure_AMPa = p2_regret_sign * dummy_pure * dummy_AMPa)
full_data = full_data %>% mutate(p2_regret_sign_pure_IDDS = p2_regret_sign * dummy_pure * dummy_IDDS)
full_data = full_data %>% mutate(p2_regret_sign_mm_AMPa = p2_regret_sign * dummy_mm * dummy_AMPa)
full_data = full_data %>% mutate(p2_regret_sign_mm_IDDS = p2_regret_sign * dummy_mm * dummy_IDDS)
full_data = full_data %>% mutate(p2_regret_sign_pure_mm_AMPa = p2_regret_sign * dummy_pure * dummy_mm * dummy_AMPa)
full_data = full_data %>% mutate(p2_regret_sign_pure_mm_IDDS = p2_regret_sign * dummy_pure * dummy_mm * dummy_IDDS)

# create first order difference
full_data = arrange(full_data, session_code, subsession_id, id_in_subsession, tick)
full_data = full_data %>% mutate(p1_diff = p1_strategy)
full_data = full_data %>% mutate(p1_next = p1_strategy)
full_data = full_data %>% mutate(p2_diff = p2_strategy)
full_data = full_data %>% mutate(p2_next = p2_strategy)
for (k in 1:(length(full_data$p1_strategy)-1)){
  if (full_data$session_round_pair_id[k] == full_data$session_round_pair_id[k+1]) {
    full_data$p1_diff[k] = full_data$p1_strategy[k+1] - full_data$p1_strategy[k]
    full_data$p1_next[k] = full_data$p1_strategy[k+1]
    full_data$p2_diff[k] = full_data$p2_strategy[k+1] - full_data$p2_strategy[k]
    full_data$p2_next[k] = full_data$p2_strategy[k+1]
  }
}
full_data = full_data %>% mutate(p1_diff_speed = p1_diff / p1_strategy)
full_data = full_data %>% mutate(p2_diff_speed = p2_diff / p2_strategy)

# export dataset to stata
write.dta(full_data, "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_production_rp.dta")

# write csv file
write.csv(full_data, "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/merge_rp.csv")


##########Figure 1: Individual cyclical dynamics##########
# select game
part_data = subset(full_data, game=='AMPa')
#part_data = subset(full_data, MC == 1 | PD == 1)
uniqueID_part = unique(part_data$session_round_pair_id) 

# loop over pairs
for(i in 1:length(uniqueID_part)){

  pairRoundData = subset(part_data, session_round_pair_id==uniqueID_part[i])
  #pairCor <- cor(pairRoundData$p2_strategy, pairRoundData$p1_strategy)
  
  title = paste(as.character(uniqueID_part[i]), as.character(pairRoundData$game[1]), 
                as.character(ifelse(pairRoundData$pure_strategy[1]==TRUE, 'pure strategy', 'mixed strategy')), 
                as.character(ifelse(pairRoundData$num_subperiods[1]==0, 'continuous time', 'discrete time')),
                sep = "_")
  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/cycles_individual/", title, sep = "")
  file = paste(file, ".png", sep = "")
  
  png(file)
  #par(mai=c(0.5,0.5,0.5,1.5))
  #par(mfrow=c(2,4))
  
  # create data center
  pairRoundData = pairRoundData %>% mutate(p1center = 0.5)
  pairRoundData = pairRoundData %>% mutate(p2center = 0.5)
  
  # cycle rotation index centering at NE
  vertCW = 0
  vertCCW = 0
  horiCW = 0
  horiCCW = 0
  for(k in 2:length(pairRoundData$tick)){
    if (pairRoundData$p1_strategy[k]<pairRoundData$p1NEmix[k] && pairRoundData$p1_strategy[k-1]>=pairRoundData$p1NEmix[k-1] && pairRoundData$p2_strategy[k]<pairRoundData$p2NEmix[k] && pairRoundData$p2_strategy[k-1]<pairRoundData$p2NEmix[k-1])
    {vertCW = vertCW + 1}
    if (pairRoundData$p1_strategy[k]>pairRoundData$p1NEmix[k] && pairRoundData$p1_strategy[k-1]<=pairRoundData$p1NEmix[k-1] && pairRoundData$p2_strategy[k]<pairRoundData$p2NEmix[k] && pairRoundData$p2_strategy[k-1]<pairRoundData$p2NEmix[k-1])
    {vertCCW = vertCCW + 1}
    if (pairRoundData$p2_strategy[k]>pairRoundData$p2NEmix[k] && pairRoundData$p2_strategy[k-1]<=pairRoundData$p2NEmix[k-1] && pairRoundData$p1_strategy[k]<pairRoundData$p1NEmix[k] && pairRoundData$p1_strategy[k-1]<pairRoundData$p1NEmix[k-1])
    {horiCW = horiCW + 1}
    if (pairRoundData$p2_strategy[k]<pairRoundData$p2NEmix[k] && pairRoundData$p2_strategy[k-1]>=pairRoundData$p2NEmix[k-1] && pairRoundData$p1_strategy[k]<pairRoundData$p1NEmix[k] && pairRoundData$p1_strategy[k-1]<pairRoundData$p1NEmix[k-1])
    {horiCCW = horiCCW + 1}
  }
  vert_cycle_index = (vertCW - vertCCW)/(vertCW + vertCCW)
  hori_cycle_index = (horiCW - horiCCW)/(horiCW + horiCCW)
  
  # 3D plot by NE
  plot3D::lines3D(pairRoundData$p1_strategy, pairRoundData$p2_strategy, pairRoundData$timeleft, col='blue', 
                  xlab='p1 strategy', xlim=c(0:1),
                  ylab='p2 strategy', ylim=c(0:1),
                  zlab='time left',
                  main = paste('RP', as.character(pairRoundData$game[1]), 
                               as.character(ifelse(pairRoundData$pure_strategy[1]==TRUE, 'pure', 'mixed')), 
                               as.character(ifelse(pairRoundData$num_subperiods[1]==0, 'continuous', 'discrete')), sep = ' '),
                  # sub = paste('vertCW', as.character(vertCW),
                  #             'vertCCW', as.character(vertCCW),
                  #             'horiCW', as.character(horiCW),
                  #             'horiCCW', as.character(horiCCW), sep = ' '),
                  theta=20, phi=30, r=2, d=1, bty='g',
                  cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
  plot3D::lines3D(pairRoundData$p1NEmix, pairRoundData$p2NEmix, pairRoundData$timeleft, col='black', add=TRUE)
  plot3D::lines3D(pairRoundData$p1center, pairRoundData$p2center, pairRoundData$timeleft, col='green', add=TRUE)
  plot3D::lines3D(pairRoundData$p1MMmix, pairRoundData$p2MMmix, pairRoundData$timeleft, col='red', add=TRUE)
  
  legend3d('topright', legend = c('strategy', 'NE', 'MM', 'Center'), xpd = TRUE,
           pch = 16, col = c('blue', 'black', 'red', 'green'))
  
  # # cycle rotation index centering at MM
  # vertCW = 0
  # vertCCW = 0
  # horiCW = 0
  # horiCCW = 0
  # for(l in 2:length(pairRoundData$tick)){
  #   if (pairRoundData$p1_strategy[l]<pairRoundData$p1MMmix[l] && pairRoundData$p1_strategy[l-1]>=pairRoundData$p1MMmix[l-1] && pairRoundData$p2_strategy[l]<pairRoundData$p2MMmix[l] && pairRoundData$p2_strategy[l-1]<pairRoundData$p2MMmix[l-1])
  #   {vertCW = vertCW + 1}
  #   if (pairRoundData$p1_strategy[l]>pairRoundData$p1MMmix[l] && pairRoundData$p1_strategy[l-1]<=pairRoundData$p1MMmix[l-1] && pairRoundData$p2_strategy[l]<pairRoundData$p2MMmix[l] && pairRoundData$p2_strategy[l-1]<pairRoundData$p2MMmix[l-1])
  #   {vertCCW = vertCCW + 1}
  #   if (pairRoundData$p2_strategy[l]>pairRoundData$p2MMmix[l] && pairRoundData$p2_strategy[l-1]<=pairRoundData$p2MMmix[l-1] && pairRoundData$p1_strategy[l]<pairRoundData$p1MMmix[l] && pairRoundData$p1_strategy[l-1]<pairRoundData$p1MMmix[l-1])
  #   {horiCW = horiCW + 1}
  #   if (pairRoundData$p2_strategy[l]<pairRoundData$p2MMmix[l] && pairRoundData$p2_strategy[l-1]>=pairRoundData$p2MMmix[l-1] && pairRoundData$p1_strategy[l]<pairRoundData$p1MMmix[l] && pairRoundData$p1_strategy[l-1]<pairRoundData$p1MMmix[l-1])
  #   {horiCCW = horiCCW + 1}
  # }
  # vert_cycle_index = (vertCW - vertCCW)/(vertCW + vertCCW)
  # hori_cycle_index = (horiCW - horiCCW)/(horiCW + horiCCW)
  # 
  # # 3D plot by MM
  # plot3D::lines3D(pairRoundData$p1_strategy, pairRoundData$p2_strategy, pairRoundData$timeleft, col='blue', 
  #                 xlab='p1 strategy', xlim=c(0:1),
  #                 ylab='p2 strategy', ylim=c(0:1),
  #                 zlab='time left',
  #                 main = paste('Strategies compare MM', 'Corr', 
  #                              as.character(round(pairCor, digits = 2)), 
  #                              'vertical CRI', as.character(round(vert_cycle_index, digits = 2)), 
  #                              'horizontal CRI', as.character(round(hori_cycle_index, digits = 2)), sep = ' '),
  #                 sub = paste('vertCW', as.character(vertCW),
  #                             'vertCCW', as.character(vertCCW),
  #                             'horiCW', as.character(horiCW),
  #                             'horiCCW', as.character(horiCCW), sep = ' '),
  #                 theta=20, phi=30, r=2, d=1, bty='g')
  # plot3D::lines3D(pairRoundData$p1MMmix, pairRoundData$p2MMmix, pairRoundData$timeleft, col='red', add=TRUE)
  # plot3D::lines3D(pairRoundData$p1center, pairRoundData$p2center, pairRoundData$timeleft, col='green', add=TRUE)
  # 
  # # p1 density map
  # plot(density(pairRoundData$p1_strategy, from = 0, to = 1), ylab="density", xlab='time',
  #      main = 'P1 Density, red:NE, blue:MM')
  # if (pairRoundData$game[1]=="AMPa"){
  #   abline(v=0.5, col='red')
  # }
  # else {abline(v=0.33, col='red')}
  # if (pairRoundData$game[1]=="AMPa"){
  #   abline(v=0.2, col='blue')
  # }
  # else {abline(v=0.75, col='blue')}
  # 
  # # p2 density map
  # plot(density(pairRoundData$p2_strategy, from = 0, to = 1), ylab="density", xlab='time',
  #      main = 'P2 Density, red:NE, blue:MM')
  # if (pairRoundData$game[1]=="AMPa"){
  #   abline(v=0.2, col='red')
  # }
  # else {abline(v=0.75, col='red')}
  # if (pairRoundData$game[1]=="AMPa"){
  #   abline(v=0.5, col='blue')
  # }
  # else {abline(v=0.67, col='blue')}
  # 
  # # distance from NE
  # plot(pairRoundData$tick, pairRoundData$NEdiff, type='l', main = 'Squared deviation from NE, red:p1, blue:p2',
  #      ylim=c(0:1), xlab='time', ylab='Squared deviation from NE')
  # lines(pairRoundData$tick, pairRoundData$p1NEdiff, col='red')
  # lines(pairRoundData$tick, pairRoundData$p2NEdiff, col='blue')
  # 
  # # distance from MM
  # plot(pairRoundData$tick, pairRoundData$MMdiff, type='l', main = 'Squared deviation from MM, red:p1, blue:p2',
  #      ylim=c(0:1), xlab='time', ylab='Squared deviation from MM')
  # lines(pairRoundData$tick, pairRoundData$p1MMdiff, col='red')
  # lines(pairRoundData$tick, pairRoundData$p2MMdiff, col='blue')
  # 
  # # P1 strategy over time
  # plot(pairRoundData$tick, pairRoundData$p1_strategy, type='l', xlab = "time", ylab = "p1 strategy", ylim=c(0:1), main = 'P1 Strategy over time, red:NE, blue:MM')
  # if (pairRoundData$game[1]=="AMPa"){
  #   abline(h=0.5, col='red')
  # }
  # else {abline(h=0.33, col='red')}
  # if (pairRoundData$game[1]=="AMPa"){
  #   abline(h=0.2, col='blue')
  # }
  # else {abline(h=0.75, col='blue')}
  # 
  # # P2 strategy over time
  # plot(pairRoundData$tick, pairRoundData$p2_strategy, type='l', xlab = "time", ylab = "p2 strategy", ylim=c(0:1), main = 'P2 Strategy over time, red:NE, blue:MM')
  # if (pairRoundData$game[1]=="AMPa"){
  #   abline(h=0.2, col='red')
  # }
  # else {abline(h=0.75, col='red')}
  # if (pairRoundData$game[1]=="AMPa"){
  #   abline(h=0.5, col='blue')
  # }
  # else {abline(h=0.67, col='blue')}
  
  dev.off()
    
}


##########Figure 2 (not used): Rotation and time average##########
# loop over pairs
for(i in 1:length(uniquePairs)){
  pairData = subset(full_data, pair_id==uniquePairs[i])
  pairRounds = unique(pairData$round_pair_id)
  
  # loop over rounds in which pairs meet
  for(j in 1:length(pairRounds)){
    pairRoundData = subset(pairData, round_pair_id==pairRounds[j])
    pairCor <- cor(pairRoundData$p2_strategy, pairRoundData$p1_strategy)
    
    title = paste("pair_round", as.character(pairRounds[j]), "game", as.character(pairRoundData$game[1]), 
                  as.character(ifelse(pairRoundData$pure_strategy==TRUE, 'pure strategy', 'mixed strategy')), 
                  as.character(ifelse(pairRoundData$num_subperiods==0, 'continuous time', 'discrete time')),
                  as.character(ifelse(pairRoundData$show_at_worst==TRUE, 'show worst', ' ')),
                  as.character(ifelse(pairRoundData$show_best_response==TRUE, 'show best reponse', ' ')),
                  sep = "_")
    file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production_1_22/time_average/", title, sep = "")
    file = paste(file, ".png", sep = "")
     
    png(file, width = 1600, height = 900)
    
    # create time average data
    pairRoundData = pairRoundData %>% mutate(p1center = mean(p1_strategy))
    pairRoundData = pairRoundData %>% mutate(p2center = mean(p2_strategy))
    pairRoundData = pairRoundData %>% mutate(p1_strategy_ave = 0)
    pairRoundData = pairRoundData %>% mutate(p2_strategy_ave = 0)
    for(m in 1:length(pairRoundData$tick)){
      pairRoundData$p1_strategy_ave[m] = mean(pairRoundData$p1_strategy[1:m])
      pairRoundData$p2_strategy_ave[m] = mean(pairRoundData$p2_strategy[1:m])
    }
    
    # cycle rotation index centering at NE
    vertCW = 0
    vertCCW = 0
    horiCW = 0
    horiCCW = 0
    for(k in 2:length(pairRoundData$tick)){
      if (pairRoundData$p1_strategy[k]<pairRoundData$p1NEmix[k] && pairRoundData$p1_strategy[k-1]>=pairRoundData$p1NEmix[k-1] && pairRoundData$p2_strategy[k]<pairRoundData$p2NEmix[k] && pairRoundData$p2_strategy[k-1]<pairRoundData$p2NEmix[k-1])
      {vertCW = vertCW + 1}
      if (pairRoundData$p1_strategy[k]>pairRoundData$p1NEmix[k] && pairRoundData$p1_strategy[k-1]<=pairRoundData$p1NEmix[k-1] && pairRoundData$p2_strategy[k]<pairRoundData$p2NEmix[k] && pairRoundData$p2_strategy[k-1]<pairRoundData$p2NEmix[k-1])
      {vertCCW = vertCCW + 1}
      if (pairRoundData$p2_strategy[k]>pairRoundData$p2NEmix[k] && pairRoundData$p2_strategy[k-1]<=pairRoundData$p2NEmix[k-1] && pairRoundData$p1_strategy[k]<pairRoundData$p1NEmix[k] && pairRoundData$p1_strategy[k-1]<pairRoundData$p1NEmix[k-1])
      {horiCW = horiCW + 1}
      if (pairRoundData$p2_strategy[k]<pairRoundData$p2NEmix[k] && pairRoundData$p2_strategy[k-1]>=pairRoundData$p2NEmix[k-1] && pairRoundData$p1_strategy[k]<pairRoundData$p1NEmix[k] && pairRoundData$p1_strategy[k-1]<pairRoundData$p1NEmix[k-1])
      {horiCCW = horiCCW + 1}
    }
    vert_cycle_index = (vertCW - vertCCW)/(vertCW + vertCCW)
    hori_cycle_index = (horiCW - horiCCW)/(horiCW + horiCCW)
    
    # 3D plot by NE
    plot3D::lines3D(pairRoundData$p1_strategy_ave, pairRoundData$p2_strategy_ave, pairRoundData$timeleft, col='blue', 
                    xlab='p1 strategy', xlim=c(0:1),
                    ylab='p2 strategy', ylim=c(0:1),
                    zlab='time left', type='l', add=FALSE,
                    main = paste('Strategies compare NE', 'Corr', 
                                 as.character(round(pairCor, digits = 2)), 
                                 'vertical CRI', as.character(round(vert_cycle_index, digits = 2)), 
                                 'horizontal CRI', as.character(round(hori_cycle_index, digits = 2)), sep = ' '),
                    sub = paste('vertCW', as.character(vertCW),
                                'vertCCW', as.character(vertCCW),
                                'horiCW', as.character(horiCW),
                                'horiCCW', as.character(horiCCW), sep = ' '))
    plot3D::lines3D(pairRoundData$p1NEmix, pairRoundData$p2NEmix, pairRoundData$timeleft, col='red', add=TRUE)
    plot3D::lines3D(pairRoundData$p1MMmix, pairRoundData$p2MMmix, pairRoundData$timeleft, col='blue', add=TRUE)
    
    dev.off()
  }
}


##########Figure 3: Cyclical dynamics by pairs##########
# generate data container for pure strategy
length = rep(NA, 1)
pair_data_pure = data.frame(stay = length, CW = length, CCW = length, diagonal = length, total = length,
                       game = length, time = length, strategy = length, matching = length)
pair_data_pure = pair_data_pure[-1,]

# pure strategy settings
treatmentdata = subset(full_data, PD==1)
#treatmentdata = subset(full_data, PC==1)

# create storage list
action = list()

# create frequency table for each game
for (i in 1:length(gametype)){
  gamedata = subset(treatmentdata, game==gametype[i])
  uniquepairs = unique(gamedata$session_round_pair_id)
  action[[i]] = matrix(0, nrow=length(uniquepairs), ncol=5)
  colnames(action[[i]]) = c("stay", "CW", "CCW", "diagonal", "total")
  
  # count frequency for each pair
  for (j in 1:length(uniquepairs)){
    rounddata = subset(gamedata, session_round_pair_id==uniquepairs[j])
    for (m in 1:(length(rounddata$tick)-1)){
      if (rounddata$p1_strategy[m]==1 & rounddata$p2_strategy[m]==1){
        if (rounddata$p1_strategy[m+1]==1 & rounddata$p2_strategy[m+1]==1){action[[i]][j,1] = action[[i]][j,1] + 1}
        if (rounddata$p1_strategy[m+1]==1 & rounddata$p2_strategy[m+1]==0){action[[i]][j,2] = action[[i]][j,2] + 1}
        if (rounddata$p1_strategy[m+1]==0 & rounddata$p2_strategy[m+1]==1){action[[i]][j,3] = action[[i]][j,3] + 1}
        if (rounddata$p1_strategy[m+1]==0 & rounddata$p2_strategy[m+1]==0){action[[i]][j,4] = action[[i]][j,4] + 1}
      }
      if (rounddata$p1_strategy[m]==1 & rounddata$p2_strategy[m]==0){
        if (rounddata$p1_strategy[m+1]==1 & rounddata$p2_strategy[m+1]==1){action[[i]][j,3] = action[[i]][j,3] + 1}
        if (rounddata$p1_strategy[m+1]==1 & rounddata$p2_strategy[m+1]==0){action[[i]][j,1] = action[[i]][j,1] + 1}
        if (rounddata$p1_strategy[m+1]==0 & rounddata$p2_strategy[m+1]==1){action[[i]][j,4] = action[[i]][j,4] + 1}
        if (rounddata$p1_strategy[m+1]==0 & rounddata$p2_strategy[m+1]==0){action[[i]][j,2] = action[[i]][j,2] + 1}
      }
      if (rounddata$p1_strategy[m]==0 & rounddata$p2_strategy[m]==1){
        if (rounddata$p1_strategy[m+1]==1 & rounddata$p2_strategy[m+1]==1){action[[i]][j,2] = action[[i]][j,2] + 1}
        if (rounddata$p1_strategy[m+1]==1 & rounddata$p2_strategy[m+1]==0){action[[i]][j,4] = action[[i]][j,4] + 1}
        if (rounddata$p1_strategy[m+1]==0 & rounddata$p2_strategy[m+1]==1){action[[i]][j,1] = action[[i]][j,1] + 1}
        if (rounddata$p1_strategy[m+1]==0 & rounddata$p2_strategy[m+1]==0){action[[i]][j,3] = action[[i]][j,3] + 1}
      }
      if (rounddata$p1_strategy[m]==0 & rounddata$p2_strategy[m]==0){
        if (rounddata$p1_strategy[m+1]==1 & rounddata$p2_strategy[m+1]==1){action[[i]][j,4] = action[[i]][j,4] + 1}
        if (rounddata$p1_strategy[m+1]==1 & rounddata$p2_strategy[m+1]==0){action[[i]][j,3] = action[[i]][j,3] + 1}
        if (rounddata$p1_strategy[m+1]==0 & rounddata$p2_strategy[m+1]==1){action[[i]][j,2] = action[[i]][j,2] + 1}
        if (rounddata$p1_strategy[m+1]==0 & rounddata$p2_strategy[m+1]==0){action[[i]][j,1] = action[[i]][j,1] + 1}
      }
    }
    action[[i]][j,5] = sum(action[[i]][j,1:4])
    action[[i]][j,] = round(action[[i]][j,]/(length(rounddata$tick)-1), digits=2)
  }
  
  action[[i]] = action[[i]][order(action[[i]][,2]),]
  
  # rbind the current gamedata to pair_data
  merge_data = data.frame(action[[i]])
  merge_data = merge_data %>% mutate(game = gamedata$game[1])
  merge_data = merge_data %>% mutate(time = ifelse(gamedata$num_subperiods[1]==0, 'continuous', 'discrete'))
  merge_data = merge_data %>% mutate(strategy = ifelse(gamedata$pure_strategy[1]==TRUE, 'pure', 'mixed'))
  merge_data = merge_data %>% mutate(matching = 'pairwise')
  
  pair_data_pure = rbind(pair_data_pure, merge_data)
}

# calculate NE
actionEQ = matrix(0, nrow=2, ncol=4)
actionEQ[1,] = c(0.34, 0.25, 0.25, 0.16)
actionEQ[2,] = c(0.347, 0.243, 0.243, 0.167)
colnames(actionEQ) = c("stay", "CW", "CCW", "diagonal")
rownames(actionEQ) = c("AMPa", "AMPb")

for (i in 1:2){
  
  # build the table for figure
  data = action[[i]]
  table = as.data.frame(matrix(nrow = length(data[,1])*4, ncol = 3))
  colnames(table)=c("pair", "type", "frequency")
  for (j in 1:length(data[,1])){
    table$pair[4*j-3] = j
    table$type[4*j-3] = '1.CW'
    table$frequency[4*j-3] = data[j,2]
    
    table$pair[4*j-2] = j
    table$type[4*j-2] = '2.diagonal'
    table$frequency[4*j-2] = data[j,4]
    
    table$pair[4*j-1] = j
    table$type[4*j-1] = '3.stay'
    table$frequency[4*j-1] = data[j,1]
    
    table$pair[4*j] = j
    table$type[4*j] = '4.CCW'
    table$frequency[4*j] = data[j,3]
  }
  table = arrange(table, type)
  
  # create figure showing pairs components
  title = paste(as.character(ifelse(i==1, 'AMPa', 'AMPb')),
                'pure', 'discrete', sep = ' ')
  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/cyclical_pair/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file)
  
  # barplot from figure 3
  pic = ggplot(table, aes(x=pair, y=frequency, fill=type)) +
    geom_bar(stat="identity", position='stack', width=1, colour='white') +
    ggtitle(title) +
    scale_x_discrete(name='pair_id', waiver()) +
    scale_y_continuous(name='strategy profile type') +
    #scale_fill_manual(values=c("1.CW"="#D72B13", "4.CCW"="#4B54AF", "2.diagonal"="#ECCF05"))
    scale_fill_manual(values=c("1.CW"="#D72B13", "4.CCW"="#4B54AF", "2.diagonal"="#ECCF05", "3.stay"="#7BBE56")) +
    theme_bw() +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5, size = 30),
          axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25),
          legend.text = element_text(size = 15))
  
  print(pic)
  
  dev.off()
}


# mixed strategy settings
# generate data container for pure strategy
length = rep(NA, 1)
pair_data_mix = data.frame(CW = length, CCW = length, diagonal = length, Cdiagonal = length, stay = length, 
                           total = length, game = length, time = length, strategy = length, matching = length)
pair_data_mix = pair_data_mix[-1,]

#treatmentdata = subset(full_data, MD==1)
treatmentdata = subset(full_data, MC==1)
#treatmentdata = subset(treatmentdata, tick%%2==1)

# craete storage list
action = list()

# create frequency table for each game
for (i in 1:length(gametype)){
  gamedata = subset(treatmentdata, game==gametype[i])
  uniquepairs = unique(gamedata$session_round_pair_id)
  action[[i]] = matrix(0, nrow=length(uniquepairs), ncol=6)
  colnames(action[[i]]) = c("CW", "CCW", "diagonal", "Cdiagonal", "stay", "total")
  
  # count frequency for each pair
  for (j in 1:length(uniquepairs)){
    rounddata = subset(gamedata, session_round_pair_id==uniquepairs[j])
    for (m in 1:(length(rounddata$tick)-1)){
      if (rounddata$p1_strategy[m]>=rounddata$p1NEmix[m] & rounddata$p2_strategy[m]>rounddata$p2NEmix[m]){
        if (rounddata$p1_strategy[m+1]>rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]<rounddata$p2_strategy[m])
        {action[[i]][j,1] = action[[i]][j,1] + 1}
        if (rounddata$p1_strategy[m+1]<rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]>rounddata$p2_strategy[m])
        {action[[i]][j,2] = action[[i]][j,2] + 1}
        if (rounddata$p1_strategy[m+1]<rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]<rounddata$p2_strategy[m])
        {action[[i]][j,3] = action[[i]][j,3] + 1}
        if (rounddata$p1_strategy[m+1]>rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]>rounddata$p2_strategy[m])
        {action[[i]][j,4] = action[[i]][j,4] + 1}
        if (rounddata$p1_strategy[m+1]==rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]==rounddata$p2_strategy[m])
        {action[[i]][j,5] = action[[i]][j,5] + 1}
        if (rounddata$p1_strategy[m+1]==rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]<rounddata$p2_strategy[m])
        {action[[i]][j,1] = action[[i]][j,1] + 1}
        if (rounddata$p1_strategy[m+1]>rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]==rounddata$p2_strategy[m])
        {action[[i]][j,1] = action[[i]][j,1] + 1}
        if (rounddata$p1_strategy[m+1]<rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]==rounddata$p2_strategy[m])
        {action[[i]][j,2] = action[[i]][j,2] + 1}
        if (rounddata$p1_strategy[m+1]==rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]>rounddata$p2_strategy[m])
        {action[[i]][j,2] = action[[i]][j,2] + 1}
      }
      if (rounddata$p1_strategy[m]>rounddata$p1NEmix[m] & rounddata$p2_strategy[m]<=rounddata$p2NEmix[m]){
        if (rounddata$p1_strategy[m+1]<rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]<rounddata$p2_strategy[m])
        {action[[i]][j,1] = action[[i]][j,1] + 1}
        if (rounddata$p1_strategy[m+1]>rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]>rounddata$p2_strategy[m])
        {action[[i]][j,2] = action[[i]][j,2] + 1}
        if (rounddata$p1_strategy[m+1]<rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]>rounddata$p2_strategy[m])
        {action[[i]][j,3] = action[[i]][j,3] + 1}
        if (rounddata$p1_strategy[m+1]>rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]<rounddata$p2_strategy[m])
        {action[[i]][j,4] = action[[i]][j,4] + 1}
        if (rounddata$p1_strategy[m+1]==rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]==rounddata$p2_strategy[m])
        {action[[i]][j,5] = action[[i]][j,5] + 1}
        if (rounddata$p1_strategy[m+1]==rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]<rounddata$p2_strategy[m])
        {action[[i]][j,1] = action[[i]][j,1] + 1}
        if (rounddata$p1_strategy[m+1]<rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]==rounddata$p2_strategy[m])
        {action[[i]][j,1] = action[[i]][j,1] + 1}
        if (rounddata$p1_strategy[m+1]>rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]==rounddata$p2_strategy[m])
        {action[[i]][j,2] = action[[i]][j,2] + 1}
        if (rounddata$p1_strategy[m+1]==rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]>rounddata$p2_strategy[m])
        {action[[i]][j,2] = action[[i]][j,2] + 1}
      }
      if (rounddata$p1_strategy[m]<rounddata$p1NEmix[m] & rounddata$p2_strategy[m]>=rounddata$p2NEmix[m]){
        if (rounddata$p1_strategy[m+1]>rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]>rounddata$p2_strategy[m])
        {action[[i]][j,1] = action[[i]][j,1] + 1}
        if (rounddata$p1_strategy[m+1]<rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]<rounddata$p2_strategy[m])
        {action[[i]][j,2] = action[[i]][j,2] + 1}
        if (rounddata$p1_strategy[m+1]>rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]<rounddata$p2_strategy[m])
        {action[[i]][j,3] = action[[i]][j,3] + 1}
        if (rounddata$p1_strategy[m+1]<rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]>rounddata$p2_strategy[m])
        {action[[i]][j,4] = action[[i]][j,4] + 1}
        if (rounddata$p1_strategy[m+1]==rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]==rounddata$p2_strategy[m])
        {action[[i]][j,5] = action[[i]][j,5] + 1}
        if (rounddata$p1_strategy[m+1]==rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]>rounddata$p2_strategy[m])
        {action[[i]][j,1] = action[[i]][j,1] + 1}
        if (rounddata$p1_strategy[m+1]>rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]==rounddata$p2_strategy[m])
        {action[[i]][j,1] = action[[i]][j,1] + 1}
        if (rounddata$p1_strategy[m+1]<rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]==rounddata$p2_strategy[m])
        {action[[i]][j,2] = action[[i]][j,2] + 1}
        if (rounddata$p1_strategy[m+1]==rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]<rounddata$p2_strategy[m])
        {action[[i]][j,2] = action[[i]][j,2] + 1}
      }
      if (rounddata$p1_strategy[m]<=rounddata$p1NEmix[m] & rounddata$p2_strategy[m]<rounddata$p2NEmix[m]){
        if (rounddata$p1_strategy[m+1]<rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]>rounddata$p2_strategy[m])
        {action[[i]][j,1] = action[[i]][j,1] + 1}
        if (rounddata$p1_strategy[m+1]>rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]<rounddata$p2_strategy[m])
        {action[[i]][j,2] = action[[i]][j,2] + 1}
        if (rounddata$p1_strategy[m+1]>rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]>rounddata$p2_strategy[m])
        {action[[i]][j,3] = action[[i]][j,3] + 1}
        if (rounddata$p1_strategy[m+1]<rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]<rounddata$p2_strategy[m])
        {action[[i]][j,4] = action[[i]][j,4] + 1}
        if (rounddata$p1_strategy[m+1]==rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]==rounddata$p2_strategy[m])
        {action[[i]][j,5] = action[[i]][j,5] + 1}
        if (rounddata$p1_strategy[m+1]==rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]>rounddata$p2_strategy[m])
        {action[[i]][j,1] = action[[i]][j,1] + 1}
        if (rounddata$p1_strategy[m+1]<rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]==rounddata$p2_strategy[m])
        {action[[i]][j,1] = action[[i]][j,1] + 1}
        if (rounddata$p1_strategy[m+1]>rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]==rounddata$p2_strategy[m])
        {action[[i]][j,2] = action[[i]][j,2] + 1}
        if (rounddata$p1_strategy[m+1]==rounddata$p1_strategy[m] & rounddata$p2_strategy[m+1]<rounddata$p2_strategy[m])
        {action[[i]][j,2] = action[[i]][j,2] + 1}
      }
    }
    action[[i]][j,6] = sum(action[[i]][j,1:5])
    action[[i]][j,] = round(action[[i]][j,]/(length(rounddata$tick)-1), digits=2)
  }
  
  action[[i]] = action[[i]][order(action[[i]][,1]),]
  
  # rbind the current gamedata to pair_data
  merge_data = data.frame(action[[i]])
  merge_data = merge_data %>% mutate(game = gamedata$game[1])
  merge_data = merge_data %>% mutate(time = ifelse(gamedata$num_subperiods[1]==0, 'continuous', 'discrete'))
  merge_data = merge_data %>% mutate(strategy = ifelse(gamedata$pure_strategy[1]==TRUE, 'pure', 'mixed'))
  merge_data = merge_data %>% mutate(matching = 'pairwise')
  
  pair_data_mix = rbind(pair_data_mix, merge_data)
}

# calculate NE
actionEQ = matrix(0, nrow=2, ncol=4)
actionEQ[1,] = c(0.34, 0.25, 0.25, 0.16)
actionEQ[2,] = c(0.347, 0.243, 0.243, 0.167)
colnames(actionEQ) = c("stay", "CW", "CCW", "diagonal")
rownames(actionEQ) = c("AMPa", "AMPb")

for (i in 1:2){
  
  # build the table for figure
  data = action[[i]]
  table = as.data.frame(matrix(nrow = length(data[,1])*5, ncol = 3))
  colnames(table)=c("pair", "type", "frequency")
  for (j in 1:length(data[,1])){
    table$pair[5*j-4] = j
    table$type[5*j-4] = '1.CW'
    table$frequency[5*j-4] = data[j,1]
    
    table$pair[5*j-3] = j
    table$type[5*j-3] = '2.diagonal'
    table$frequency[5*j-3] = data[j,3]
    
    table$pair[5*j-2] = j
    table$type[5*j-2] = '3.Cdiagonal'
    table$frequency[5*j-2] = data[j,4]
    
    table$pair[5*j-1] = j
    table$type[5*j-1] = '4.stay'
    table$frequency[5*j-1] = data[j,5]
    
    table$pair[5*j] = j
    table$type[5*j] = '5.CCW'
    table$frequency[5*j] = data[j,2]
  }
  table = arrange(table, type)
  
  # create bar plot
  title = paste(as.character(ifelse(i==1, 'AMPa', 'AMPb')),
                'mixed', 'continuous', sep = ' ')
  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/cyclical_pair/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file)
  
  pic = ggplot(table, aes(x=pair, y=frequency, fill=type)) +
    geom_bar(stat="identity", position='stack', width=1, colour='white') +
    ggtitle(title) +
    scale_x_discrete(name='pair_id', waiver()) +
    scale_y_continuous(name='strategy profile type') +
    #scale_fill_manual(values=c("1.CW"="#D72B13", "5.CCW"="#4B54AF", "2.diagonal"="#ECCF05", "3.Cdiagonal"='grey'))
    scale_fill_manual(values=c("1.CW"="#D72B13", "5.CCW"="#4B54AF", "2.diagonal"="#ECCF05", "3.Cdiagonal"='grey',"4.stay"="#7BBE56")) +
    theme_bw() + 
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5, size = 30),
          axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25),
          legend.text = element_text(size = 15))
  
  print(pic)
  
  dev.off()
}


# combine pair_data_pure and pair_data_mix
pair_data_pure = subset(pair_data_pure, select = c(stay,CW,CCW,diagonal,game,time,strategy))
pair_data_mix = subset(pair_data_mix, select = c(stay,CW,CCW,diagonal,game,time,strategy))
pair_data = rbind(pair_data_pure, pair_data_mix)
pair_data = subset(pair_data, game!='IDDS')

# export dataset to stata
write.dta(pair_data, "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/stata/mp_pairwise_paircycles.dta")


# # combine pure and mix strategy setting with strategy profile type
# treatmentdata = subset(full_data, MD==1)
# 
# # create storage list
# action = list()
# 
# # create frequency table for each game
# for (i in 1:length(gametype)){
#   gamedata = subset(treatmentdata, game==gametype[i])
#   uniquepairs = unique(gamedata$session_round_pair_id)
#   action[[i]] = matrix(0, nrow=length(uniquepairs), ncol=5)
#   colnames(action[[i]]) = c("stay", "CW", "CCW", "diagonal", "total")
#   
#   # count frequency for each pair
#   for (j in 1:length(uniquepairs)){
#     rounddata = subset(gamedata, session_round_pair_id==uniquepairs[j])
#     for (m in 1:(length(rounddata$tick)-1)){
#       if (rounddata$strategy_profile[m] == 'AA'){
#         if (rounddata$strategy_profile[m+1] == 'AA'){action[[i]][j,1] = action[[i]][j,1] + 1}
#         if (rounddata$strategy_profile[m+1] == 'AB'){action[[i]][j,2] = action[[i]][j,2] + 1}
#         if (rounddata$strategy_profile[m+1] == 'BA'){action[[i]][j,3] = action[[i]][j,3] + 1}
#         if (rounddata$strategy_profile[m+1] == 'BB'){action[[i]][j,4] = action[[i]][j,4] + 1}
#       }
#       if (rounddata$strategy_profile[m] == 'AB'){
#         if (rounddata$strategy_profile[m+1] == 'AA'){action[[i]][j,3] = action[[i]][j,3] + 1}
#         if (rounddata$strategy_profile[m+1] == 'AB'){action[[i]][j,1] = action[[i]][j,1] + 1}
#         if (rounddata$strategy_profile[m+1] == 'BA'){action[[i]][j,4] = action[[i]][j,4] + 1}
#         if (rounddata$strategy_profile[m+1] == 'BB'){action[[i]][j,2] = action[[i]][j,2] + 1}
#       }
#       if (rounddata$strategy_profile[m] == 'BA'){
#         if (rounddata$strategy_profile[m+1] == 'AA'){action[[i]][j,2] = action[[i]][j,2] + 1}
#         if (rounddata$strategy_profile[m+1] == 'AB'){action[[i]][j,4] = action[[i]][j,4] + 1}
#         if (rounddata$strategy_profile[m+1] == 'BA'){action[[i]][j,1] = action[[i]][j,1] + 1}
#         if (rounddata$strategy_profile[m+1] == 'BB'){action[[i]][j,3] = action[[i]][j,3] + 1}
#       }
#       if (rounddata$strategy_profile[m] == 'BB'){
#         if (rounddata$strategy_profile[m+1] == 'AA'){action[[i]][j,4] = action[[i]][j,4] + 1}
#         if (rounddata$strategy_profile[m+1] == 'AB'){action[[i]][j,3] = action[[i]][j,3] + 1}
#         if (rounddata$strategy_profile[m+1] == 'BA'){action[[i]][j,2] = action[[i]][j,2] + 1}
#         if (rounddata$strategy_profile[m+1] == 'BB'){action[[i]][j,1] = action[[i]][j,1] + 1}
#       }
#     }
#     action[[i]][j,5] = sum(action[[i]][j,1:4])
#   }
#   action[[i]] = round(action[[i]]/(length(rounddata$tick)-1), digits=2)
# }
# 
# # calculate NE
# actionEQ = matrix(0, nrow=2, ncol=4)
# actionEQ[1,] = c(0.34, 0.25, 0.25, 0.16)
# actionEQ[2,] = c(0.347, 0.243, 0.243, 0.167)
# colnames(actionEQ) = c("stay", "CW", "CCW", "diagonal")
# rownames(actionEQ) = c("AMPa", "AMPb")
# 
# # create figure showing actual fraction vs predicted fraction
# for (i in 1:nrow(actionEQ)){
#   title = paste("cyclics counter", as.character(gametype[i]), 
#                 as.character(ifelse(rounddata$pure_strategy[1]=='TRUE', 'pure strategy', 'mixed strategy')), 
#                 as.character(ifelse(rounddata$num_subperiods[1]==0, 'continuous time', 'discrete time')),
#                 sep = " ")
#   file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/cyclical_behavior_type/", title, sep = "")
#   file = paste(file, ".png", sep = "")
#   png(file)
#   par(mfrow=c(2,2))
#   for (j in 1:ncol(actionEQ)){
#     plot(action[[i]][,j], ylim=c(0:1),
#          main=paste(as.character(colnames(actionEQ)[j])))
#     abline(h=actionEQ[i,j], col="red")
#   }
#   dev.off()
# }


##########Figure 4: Cyclical dynamics over time##########
# change the frequency of data to every two seconds (only in continuous time)
#part_data = subset(full_data, tick%%2 == 0)
part_data = full_data
new_data = subset(full_data, tick==0)
new_data = new_data %>% mutate(cyctype = NA)

# pure strategy settings
treatmentdatafull = subset(full_data, PD==1)
#treatmentdatafull = subset(part_data, PC==1)
treatmentdatafull = treatmentdatafull %>% mutate(cyctype = "initial point")

# give each observation (strategy profile) a certain cyclical type
for (i in 1:2){
  treatmentdata = subset(treatmentdatafull, game==gametype[i])
  for (m in 1:(length(treatmentdata$tick)-1)){
    if (treatmentdata$session_round_pair_id[m] == treatmentdata$session_round_pair_id[m+1]){
      if (treatmentdata$p1_strategy[m]==1 & treatmentdata$p2_strategy[m]==1){
        if (treatmentdata$p1_strategy[m+1]==1 & treatmentdata$p2_strategy[m+1]==1){treatmentdata$cyctype[m+1] = "3.stay"}
        if (treatmentdata$p1_strategy[m+1]==1 & treatmentdata$p2_strategy[m+1]==0){treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_strategy[m+1]==0 & treatmentdata$p2_strategy[m+1]==1){treatmentdata$cyctype[m+1] = "4.CCW"}
        if (treatmentdata$p1_strategy[m+1]==0 & treatmentdata$p2_strategy[m+1]==0){treatmentdata$cyctype[m+1] = "2.diagonal"}
      }
      if (treatmentdata$p1_strategy[m]==1 & treatmentdata$p2_strategy[m]==0){
        if (treatmentdata$p1_strategy[m+1]==1 & treatmentdata$p2_strategy[m+1]==1){treatmentdata$cyctype[m+1] = "4.CCW"}
        if (treatmentdata$p1_strategy[m+1]==1 & treatmentdata$p2_strategy[m+1]==0){treatmentdata$cyctype[m+1] = "3.stay"}
        if (treatmentdata$p1_strategy[m+1]==0 & treatmentdata$p2_strategy[m+1]==1){treatmentdata$cyctype[m+1] = "2.diagonal"}
        if (treatmentdata$p1_strategy[m+1]==0 & treatmentdata$p2_strategy[m+1]==0){treatmentdata$cyctype[m+1] = "1.CW"}
      }
      if (treatmentdata$p1_strategy[m]==0 & treatmentdata$p2_strategy[m]==1){
        if (treatmentdata$p1_strategy[m+1]==1 & treatmentdata$p2_strategy[m+1]==1){treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_strategy[m+1]==1 & treatmentdata$p2_strategy[m+1]==0){treatmentdata$cyctype[m+1] = "2.diagonal"}
        if (treatmentdata$p1_strategy[m+1]==0 & treatmentdata$p2_strategy[m+1]==1){treatmentdata$cyctype[m+1] = "3.stay"}
        if (treatmentdata$p1_strategy[m+1]==0 & treatmentdata$p2_strategy[m+1]==0){treatmentdata$cyctype[m+1] = "4.CCW"}
      }
      if (treatmentdata$p1_strategy[m]==0 & treatmentdata$p2_strategy[m]==0){
        if (treatmentdata$p1_strategy[m+1]==1 & treatmentdata$p2_strategy[m+1]==1){treatmentdata$cyctype[m+1] = "2.diagonal"}
        if (treatmentdata$p1_strategy[m+1]==1 & treatmentdata$p2_strategy[m+1]==0){treatmentdata$cyctype[m+1] = "4.CCW"}
        if (treatmentdata$p1_strategy[m+1]==0 & treatmentdata$p2_strategy[m+1]==1){treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_strategy[m+1]==0 & treatmentdata$p2_strategy[m+1]==0){treatmentdata$cyctype[m+1] = "3.stay"}
      }
    }
  }
  
  # drop initial point
  treatmentdata = subset(treatmentdata, cyctype != "initial point")
  #treatmentdata = subset(treatmentdata, cyctype != "3.stay")
  
  # add current data to new_data
  new_data = rbind(new_data, treatmentdata)
  
  # bar plot basic parameters
  title = paste(as.character(treatmentdata$game[1]),
                as.character(ifelse(treatmentdata$pure_strategy[1]=="TRUE", 'pure', 'mixed')),
                as.character(ifelse(treatmentdata$num_subperiods[1]==0, 'continuous', 'discrete')),
                sep = " ")

  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/cyclical_behavior/", title, sep = "")
  file = paste(file, ".png", sep = "")

  png(file, width = 800, height = 600)

  # create frequency table
  data = table(treatmentdata$tick, treatmentdata$cyctype)
  nrows = nrow(data) * ncol(data)
  table = as.data.frame(matrix(nrow=nrows, ncol=3))
  colnames(table)=c("time", "type", "frequency")
  o = 1
  for(p in 1:nrow(data)){
    for(q in 1:ncol(data)){
      table$time[o] = p
      table$type[o] = colnames(data)[q]
      table$frequency[o] = data[p,q]
      o = o + 1
    }
  }
  table = arrange(table, type)

  # barplot
  pic = ggplot(table, aes(x=time, y=frequency, fill=type)) +
    geom_bar(stat="identity", position='fill', width=1, colour='white') +
    ggtitle(title) +
    scale_x_discrete(name='time', waiver()) +
    scale_y_continuous(name='strategy profile type') +
    #scale_fill_manual(values=c("1.CW"="#D72B13", "4.CCW"="#4B54AF", "2.diagonal"="#ECCF05"))
    scale_fill_manual(values=c("1.CW"="#D72B13", "4.CCW"="#4B54AF", "2.diagonal"="#ECCF05", "3.stay"="#7BBE56")) +
    theme_bw() +
    theme(legend.position="none",
      plot.title = element_text(hjust = 0.5, size = 30),
      axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25),
      legend.text = element_text(size = 15))

  print(pic)

  dev.off()
}

# mixed strategy settings
#treatmentdatafull = subset(full_data, MD==1)
treatmentdatafull = subset(part_data, MC==1)
treatmentdatafull = treatmentdatafull %>% mutate(cyctype = "initial point")

# give each observation (strategy profile) a certain cyclical type
for (i in 1:2){
  treatmentdata = subset(treatmentdatafull, game==gametype[i])
  for (m in 1:(length(treatmentdata$tick)-1)){
    if (treatmentdata$session_round_pair_id[m] == treatmentdata$session_round_pair_id[m+1]){
      if (treatmentdata$p1_strategy[m]>=treatmentdata$p1NEmix[m] & treatmentdata$p2_strategy[m]>treatmentdata$p2NEmix[m]){
        if (treatmentdata$p1_strategy[m+1]>treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]<treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_strategy[m+1]<treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]>treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_strategy[m+1]<treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]<treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "2.diagonal"}
        if (treatmentdata$p1_strategy[m+1]>treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]>treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "3.Cdiagonal"}
        if (treatmentdata$p1_strategy[m+1]==treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]==treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "4.stay"}
        if (treatmentdata$p1_strategy[m+1]==treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]<treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_strategy[m+1]>treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]==treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_strategy[m+1]<treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]==treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_strategy[m+1]==treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]>treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
      }
      if (treatmentdata$p1_strategy[m]>treatmentdata$p1NEmix[m] & treatmentdata$p2_strategy[m]<=treatmentdata$p2NEmix[m]){
        if (treatmentdata$p1_strategy[m+1]<treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]<treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_strategy[m+1]>treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]>treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_strategy[m+1]<treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]>treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "2.diagonal"}
        if (treatmentdata$p1_strategy[m+1]>treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]<treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "3.Cdiagonal"}
        if (treatmentdata$p1_strategy[m+1]==treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]==treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "4.stay"}
        if (treatmentdata$p1_strategy[m+1]==treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]<treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_strategy[m+1]<treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]==treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_strategy[m+1]>treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]==treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_strategy[m+1]==treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]>treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
      }
      if (treatmentdata$p1_strategy[m]<treatmentdata$p1NEmix[m] & treatmentdata$p2_strategy[m]>=treatmentdata$p2NEmix[m]){
        if (treatmentdata$p1_strategy[m+1]>treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]>treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_strategy[m+1]<treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]<treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_strategy[m+1]>treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]<treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "2.diagonal"}
        if (treatmentdata$p1_strategy[m+1]<treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]>treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "3.Cdiagonal"}
        if (treatmentdata$p1_strategy[m+1]==treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]==treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "4.stay"}
        if (treatmentdata$p1_strategy[m+1]==treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]>treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_strategy[m+1]>treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]==treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_strategy[m+1]<treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]==treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_strategy[m+1]==treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]<treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
      }
      if (treatmentdata$p1_strategy[m]<=treatmentdata$p1NEmix[m] & treatmentdata$p2_strategy[m]<treatmentdata$p2NEmix[m]){
        if (treatmentdata$p1_strategy[m+1]<treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]>treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_strategy[m+1]>treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]<treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_strategy[m+1]>treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]>treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "2.diagonal"}
        if (treatmentdata$p1_strategy[m+1]<treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]<treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "3.Cdiagonal"}
        if (treatmentdata$p1_strategy[m+1]==treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]==treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "4.stay"}
        if (treatmentdata$p1_strategy[m+1]==treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]>treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_strategy[m+1]<treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]==treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_strategy[m+1]>treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]==treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_strategy[m+1]==treatmentdata$p1_strategy[m] & treatmentdata$p2_strategy[m+1]<treatmentdata$p2_strategy[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
      }
    }
  }
  
  # drop initial point
  treatmentdata = subset(treatmentdata, cyctype != "initial point")
  #treatmentdata = subset(treatmentdata, cyctype != "4.stay")
  
  # add current data to new_data
  new_data = rbind(new_data, treatmentdata)
  
  # bar plot basic parameters
  title = paste(as.character(treatmentdata$game[1]),
                as.character(ifelse(treatmentdata$pure_strategy[1]=="TRUE", 'pure', 'mixed')),
                as.character(ifelse(treatmentdata$num_subperiods[1]==0, 'continuous', 'discrete')),
                sep = " ")

  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/cyclical_behavior/", title, sep = "")
  file = paste(file, ".png", sep = "")

  png(file, width = 800, height = 600)

  # create frequency table
  data = table(treatmentdata$tick, treatmentdata$cyctype)
  nrows = nrow(data) * ncol(data)
  table = as.data.frame(matrix(nrow=nrows, ncol=3))
  colnames(table)=c("time", "type", "frequency")
  o = 1
  for(p in 1:nrow(data)){
    for(q in 1:ncol(data)){
      table$time[o] = p
      table$type[o] = colnames(data)[q]
      table$frequency[o] = data[p,q]
      o = o + 1
    }
  }
  table = arrange(table, type)

  # barplot
  pic = ggplot(table, aes(x=time, y=frequency, fill=type)) +
    geom_bar(stat="identity", position='fill', width=1, colour='white') +
    ggtitle(title) +
    scale_x_discrete(name='time', waiver()) +
    scale_y_continuous(name='strategy profile type') +
    #scale_fill_manual(values=c("1.CW"="#D72B13", "5.CCW"="#4B54AF", "2.diagonal"="#ECCF05", "3.Cdiagonal"='grey'))
    scale_fill_manual(values=c("1.CW"="#D72B13", "5.CCW"="#4B54AF", "2.diagonal"="#ECCF05", "3.Cdiagonal"='grey',"4.stay"="#7BBE56")) +
    theme_bw() +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5, size = 30),
          axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25),
          legend.text = element_text(size = 15))

  print(pic)

  dev.off()
}

# export dataset to stata
write.dta(new_data, "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/stata/mp_pairwise_cycles.dta")

# seperate legend
library(plot3D)
legend3d('topright', legend = c('1.CW', '2.diagonal', '3.Cdiagonal', '4.stay', '5.CCW'), xpd = TRUE,
         fill = c('#D72B13', '#ECCF05', 'grey', '#7BBE56', "#4B54AF"))

# combine pure and mixed strategy settings by strategy profile type
treatmentdatafull = subset(full_data, MD==1)
treatmentdatafull = treatmentdatafull %>% mutate(cyctype = "initial point")

# give each observation (strategy profile) a certain cyclical type
for (i in 1:2){
  treatmentdata = subset(treatmentdatafull, game==gametype[i])
  for (m in 1:(length(treatmentdata$tick)-1)){
    if (treatmentdata$session_round_pair_id[m] == treatmentdata$session_round_pair_id[m+1]){
      if (treatmentdata$strategy_profile[m]=='AA'){
        if (treatmentdata$strategy_profile[m+1]=='AA'){treatmentdata$cyctype[m+1] = "3.stay"}
        if (treatmentdata$strategy_profile[m+1]=='AB'){treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$strategy_profile[m+1]=='BA'){treatmentdata$cyctype[m+1] = "4.CCW"}
        if (treatmentdata$strategy_profile[m+1]=='BB'){treatmentdata$cyctype[m+1] = "2.diagonal"}
      }
      if (treatmentdata$strategy_profile[m]=='AB'){
        if (treatmentdata$strategy_profile[m+1]=='AA'){treatmentdata$cyctype[m+1] = "4.CCW"}
        if (treatmentdata$strategy_profile[m+1]=='AB'){treatmentdata$cyctype[m+1] = "3.stay"}
        if (treatmentdata$strategy_profile[m+1]=='BA'){treatmentdata$cyctype[m+1] = "2.diagonal"}
        if (treatmentdata$strategy_profile[m+1]=='BB'){treatmentdata$cyctype[m+1] = "1.CW"}
      }
      if (treatmentdata$strategy_profile[m]=='BA'){
        if (treatmentdata$strategy_profile[m+1]=='AA'){treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$strategy_profile[m+1]=='AB'){treatmentdata$cyctype[m+1] = "2.diagonal"}
        if (treatmentdata$strategy_profile[m+1]=='BA'){treatmentdata$cyctype[m+1] = "3.stay"}
        if (treatmentdata$strategy_profile[m+1]=='BB'){treatmentdata$cyctype[m+1] = "4.CCW"}
      }
      if (treatmentdata$strategy_profile[m]=='BB'){
        if (treatmentdata$strategy_profile[m+1]=='AA'){treatmentdata$cyctype[m+1] = "2.diagonal"}
        if (treatmentdata$strategy_profile[m+1]=='AB'){treatmentdata$cyctype[m+1] = "4.CCW"}
        if (treatmentdata$strategy_profile[m+1]=='BA'){treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$strategy_profile[m+1]=='BB'){treatmentdata$cyctype[m+1] = "3.stay"}
      }
    }
  }
  
  # drop initial point
  treatmentdata = subset(treatmentdata, cyctype != "initial point")
  
  # bar plot basic parameters
  title = paste("type over time", as.character(treatmentdata$game[1]), 
                as.character(ifelse(treatmentdata$pure_strategy[1]=="TRUE", 'pure strategy', 'mixed strategy')), 
                as.character(ifelse(treatmentdata$num_subperiods[1]==0, 'continuous time', 'discrete time')),
                sep = " ")
  
  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/cyclical_behavior_type/", title, sep = "")
  file = paste(file, ".png", sep = "")
  
  png(file, width = 800, height = 600)
  
  # create frequency table
  data = table(treatmentdata$tick, treatmentdata$cyctype)
  nrows = nrow(data) * ncol(data)
  table = as.data.frame(matrix(nrow=nrows, ncol=3))
  colnames(table)=c("time", "type", "frequency")
  o = 1
  for(p in 1:nrow(data)){
    for(q in 1:ncol(data)){
      table$time[o] = p
      table$type[o] = colnames(data)[q]
      table$frequency[o] = data[p,q]
      o = o + 1
    }
  }
  table = arrange(table, type)
  
  # barplot
  pic = ggplot(table, aes(x=time, y=frequency, fill=type)) +
    geom_bar(stat="identity", position='fill', width=1, colour='white') +
    ggtitle(title) +
    scale_x_discrete(name='time', waiver()) +
    scale_y_continuous(name='strategy profile type') +
    scale_fill_manual(values=c("1.CW"="#D72B13", "4.CCW"="#4B54AF", "2.diagonal"="#ECCF05", "3.stay"="#7BBE56"))
  
  print(pic)
  
  dev.off()
}

##########Figure 5 (not used): .Gif showing how dynamics works in each pair##########
# load packages
library(gganimate)

# create unique pairs
rounddata = subset(full_data, MC==1)
uniquepairs = unique(rounddata$session_round_pair_id)
rounddata = subset(rounddata, session_round_pair_id==uniquepairs[1])

ggplot(rounddata, aes(p2_strategy, p1_strategy)) +
  geom_point(alpha = 0.7, show.legend = TRUE) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(title = 'Tick: {frame_time}', x = 'column player', y = 'row player') +
  transition_time(tick) +
  ease_aes('linear') +
  enter_fade() +
  exit_fade() +
  shadow_trail() +
  view_static()

##########Figure 6 (not used): Time average in IDDS##########
part_data = subset(full_data, game=='IDDS')

# loop over IDDS treatments
for (i in 9:12){
  treatment_data = subset(part_data, treatment==i)
  treatment_data = subset(part_data, id_in_subsession==1)
  
  # create time average
  mean_data = data.frame(p1mean = tapply(treatment_data$p1_strategy, treatment_data$tick, mean),
                         p2mean = tapply(treatment_data$p2_strategy, treatment_data$tick, mean),
                         tick = tapply(treatment_data$tick, treatment_data$tick, mean))
  
  # create figure titles
  title = paste('boxplot', as.character(treatment_data$game[1]),
                as.character(ifelse(treatment_data$pure_strategy[1]=='TRUE', 'pure strategy', 'mixed strategy')), 
                as.character(ifelse(treatment_data$num_subperiods[1]==0, 'continuous time', 'discrete time')),
                'pairwise', sep = "_")
  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/IDDS/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 1200, height = 600)
  par(mfrow=c(1,2))
  
  # draw figures
  boxplot(p1_average ~ tick, data = treatment_data,
          xlab = "time", ylab = "population mean", ylim=c(0:1), col="blue",
          main = 'row population')
  abline(h = treatment_data$p1NEmix[1], lwd=3, col='darkgreen')
  abline(h = treatment_data$p1MMmix[1], lwd=3, col='purple4')
  
  boxplot(p2_average ~ tick, data = treatment_data,
          xlab = "time", ylab = "population mean", ylim=c(0:1), col="red",
          main = 'column population')
  abline(h = treatment_data$p2NEmix[1], lwd=3, col='darkgreen')
  abline(h = treatment_data$p2MMmix[1], lwd=3, col='purple4')
  
  # plot(mean_data$tick, mean_data$p1mean,
  #      type='l', xlab = "time", ylab = "population mean", ylim=c(0:1), col="blue", lwd=2,
  #      main = 'row population')
  # abline(h = treatment_data$p1NEmix[1], lwd=3)
  # 
  # plot(mean_data$tick, mean_data$p2mean,
  #      type='l', xlab = "time", ylab = "population mean", ylim=c(0:1), col="red", lwd=2,
  #      main = 'column population')
  # abline(h = treatment_data$p2NEmix[1], lwd=3)
  
  
  dev.off()
}


##########Figure 7 (not used): payoff over time in all games##########
# create empty data container
mp_data = subset(full_data, game!='IDDS')
uniqueID = unique(mp_data$session_round_pair_id)
mean_payoff = matrix(0, nrow = length(uniqueID), ncol = 3)

# loop over periods
for (i in 1:length(uniqueID)){
  
  round_data = subset(mp_data, session_round_pair_id == uniqueID[i])
  
  # plot the payoff for both players
  title = paste(as.character(round_data$session_round_pair_id[1]),
                as.character(round_data$game[1]),
                as.character(ifelse(round_data$pure_strategy[1]=='TRUE', 'pure', 'mixed')), 
                as.character(ifelse(round_data$num_subperiods[1]==0, 'continuous', 'discrete')),
                as.character(ifelse(round_data$mean_matching[1]=='TRUE', 'mm', 'rp')), sep = "_")
  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/payoff/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file)
  
  plot(round_data$tick, round_data$p1_payoff, 
       type='l', xlab = "time", ylab = "payoff", col="blue",
       main = title)
  lines(round_data$tick, round_data$p2_payoff, col="red")
  
  legend('bottomright', legend = c('row', 'column'), fill = c('blue', 'red'))
  
  dev.off()
  
  # see if there is any pair that aim at equalizing the payoff
  mean_payoff[i,1] = mean(round_data$p1_payoff)
  mean_payoff[i,2] = mean(round_data$p2_payoff)
  mean_payoff[i,3] = mean_payoff[i,1] - mean_payoff[i,2]
  
}

# plot the distribution of payoff difference
title = paste('Distribution of payoff difference by period')
file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/payoff/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file)

hist(mean_payoff[,3], xlab = "payoff difference", ylab = "frequency",
     xlim = c(-200,400), breaks = 50,
     main = 'Distribution of payoff difference by period')

dev.off()


##########Figure 8 (not used): Clustering based on entire dataset##########
# create pair level clustering dataset
test_data = subset(full_data, num_subperiods==0)
test_data = subset(test_data, game!='IDDS')
#test_data = subset(test_data, pure_strategy==FALSE)
uniquepairs = unique(test_data$session_round_pair_id)

# create data container for training sets
training_set = matrix(nrow = length(uniquepairs), ncol = 2*144)
cluster = matrix(nrow = length(uniquepairs), ncol = 1)
rownames(training_set) = uniquepairs
colnames(cluster) = 'cluster'

# loop over pairs to create training sets
for (i in 1:length(uniquepairs)){
  round_data = subset(test_data, session_round_pair_id == uniquepairs[i])
  
  training_set[i,] = t(cbind(round_data$p1_strategy, round_data$p2_strategy))
}

# random select k initial centers
k = 4
iterations = 10
center = training_set[sample(nrow(training_set), k),]
# center[1,] = cbind(rep(1,144), rep(0,144))
# center[2,] = cbind(rep(0,144), rep(1,144))
# center[3,] = cbind(rep(0,144), rep(0,144))
# center[4,] = cbind(rep(0.5,144), rep(0.5,144))
error = 0

# create k mean function
for (m in 1:iterations){
  
  # loop over observations
  for (i in 1:nrow(training_set)){
    
    # set initial distance
    distance_min = 100000
    
    # loop over centers
    for (j in 1:k){
      distance = dist(rbind(training_set[i,], center[j,]), method = 'euclidean')
      
      if (distance < distance_min){
        distance_min = distance
        cluster[i] = j
      }
    }
    # calculate total error in the last iteration
    if (m==iterations){error = error + distance_min}
  }
  
  # update the center in the current iteration
  update_data = cbind(training_set, cluster)
  update_data = data.frame(update_data)
  
  for (o in 1:k){
    update = subset(update_data, cluster==o)
    
    center[o,] = as.numeric(colMeans(update))[-289]
  }
}

# create centers figure
# set title
title = paste('center_cluster_continuous')
file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/data_summary/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 1000, height = 1000)

par(mfrow=c(2,2))

# draw k centers
for (p in 1:k){
  plot(center[p,1:144], col = 'blue', type = 'l', ylim = c(0:1),
       main = paste('center', as.character(p)))
  lines(center[p,145:288], col = 'red', type = 'l')
}

dev.off()


##########Figure 9 (not used): Clustering based on pair level dynamics##########
# lower the dimension of dynamics from 4 to 2
pair_data = pair_data %>% mutate(type_cw = CW)
pair_data = pair_data %>% mutate(type_adv = CCW + diagonal)
pair_data = pair_data %>% mutate(cluster = 0)

# random select k initial centers
k = 4
iterations = 10
center = data.frame(type_cw = c(0, 1, 0.5, 0),
                    type_adv = c(1, 0, 0.5, 0))
error = 0

# create k mean function
for (m in 1:iterations){
  
  # loop over observations
  for (i in 1:length(pair_data$type_cw)){
    
    # set initial distance
    distance_min = 100000
    
    # loop over centers
    for (j in 1:k){
      distance = sqrt((pair_data$type_cw[i] - center$type_cw[j])^2 + (pair_data$type_adv[i] - center$type_adv[j])^2)
      
      if (distance < distance_min){
        distance_min = distance
        pair_data$cluster[i] = j
      }
    }
    # calculate total error in the last iteration
    if (m==iterations){error = error + distance_min}
  }
  
  # update the center in the current iteration
  for (h in 1:k){
    update = subset(pair_data, cluster==h)
    center$type_cw[h] = mean(update$type_cw)
    center$type_adv[h] = mean(update$type_adv)
  }
}

# create clustering figure
# set title
title = paste('simple clustering of dynamics')
file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/data_summary/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 600, height = 600)

# ggplot with rectanglers, NE and MM points.
pic = ggplot() +
  
  geom_point(data = pair_data, mapping = aes(x = type_cw, y = type_adv, colour = factor(cluster))) +
  
  geom_point(data = center, mapping = aes(x = type_cw, y = type_adv, size = 1.5))

ggtitle('clustering of dynamics') +
  scale_fill_gradient(low = "red", high = "purple") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 20))

print(pic)
dev.off()
