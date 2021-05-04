##########Data preparation##########
# load packages
library(MASS)
library(ggplot2)
library(dplyr)
library(lattice)
library(latticeExtra)
library(haven)
library(grid)
library(csv)
library(foreign)
library(scatterplot3d)
library(rgl)
library(plot3D)

# load data 
bimatrix_choice <- read.csv("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/data_mm.csv"
                            , header = T)

# create round variable in choice data and create full dataset
bimatrix_choice$round = as.double(substring(bimatrix_choice$subsession_id, 2, 3))
full_data = bimatrix_choice
full_data = arrange(full_data, full_data$session_code, full_data$subsession_id, full_data$id_in_subsession, full_data$tick)
rm(bimatrix_choice)

# create round/pair id
full_data$session_round_id = paste(full_data$session_code, full_data$round, sep = "_")

# drop Nah data
full_data = filter(full_data, tick > 1 | num_subperiods == 25)

# add group variables
full_data = full_data %>% mutate(game_AMPa = ifelse(payoff1Aa == 800,1,0))
full_data = full_data %>% mutate(game_AMPb = ifelse(payoff1Aa == 300,1,0))
full_data = full_data %>% mutate(game_idds = ifelse(payoff1Aa == 200,1,0))
full_data = full_data %>% mutate(game = ifelse(payoff1Aa == 800,"AMPa",ifelse(payoff1Aa == 300, "AMPb","IDDS")))

full_data = full_data %>% mutate(PC = ifelse(pure_strategy=="TRUE", ifelse(num_subperiods==0,1,0),0))
full_data = full_data %>% mutate(PD = ifelse(pure_strategy=="TRUE", ifelse(num_subperiods==25,1,0),0))
full_data = full_data %>% mutate(MC = ifelse(pure_strategy=="FALSE", ifelse(num_subperiods==0,1,0),0))
full_data = full_data %>% mutate(MD = ifelse(pure_strategy=="FALSE", ifelse(num_subperiods==25,1,0),0))

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
full_data = full_data %>% mutate(p2NEmix = ifelse(game == "AMPa", .2, ifelse(game == "AMPb", .75, 1)))
full_data = full_data %>% mutate(p1MMmix = ifelse(game == "AMPa", .2, ifelse(game == "AMPb", .75, 0)))
full_data = full_data %>% mutate(p2MMmix = ifelse(game == "AMPa", .5, ifelse(game == "AMPb", .67, 1)))

# calculate mean strategy
full_data = full_data %>% mutate(player_in_group = ifelse(p9_code=="", 4, ifelse(p11_code=="", 5, 6)))
for (i in 1:length(full_data$tick)){
  if (full_data$player_in_group[i] == 4)
  {full_data$p1_average[i] = (full_data$p1_strategy[i] + full_data$p3_strategy[i] + full_data$p5_strategy[i] + full_data$p7_strategy[i]) / 4
   full_data$p2_average[i] = (full_data$p2_strategy[i] + full_data$p4_strategy[i] + full_data$p6_strategy[i] + full_data$p8_strategy[i]) / 4
   full_data$p1_median[i] = median(full_data$p1_strategy[i], full_data$p3_strategy[i], full_data$p5_strategy[i], full_data$p7_strategy[i])
   full_data$p2_median[i] = median(full_data$p2_strategy[i], full_data$p4_strategy[i], full_data$p6_strategy[i], full_data$p8_strategy[i])
  }
  
  if (full_data$player_in_group[i] == 5)
  {full_data$p1_average[i] = (full_data$p1_strategy[i] + full_data$p3_strategy[i] + full_data$p5_strategy[i] + full_data$p7_strategy[i] + full_data$p9_strategy[i]) / 5
   full_data$p2_average[i] = (full_data$p2_strategy[i] + full_data$p4_strategy[i] + full_data$p6_strategy[i] + full_data$p8_strategy[i] + full_data$p10_strategy[i]) / 5
   full_data$p1_median[i] = median(full_data$p1_strategy[i], full_data$p3_strategy[i], full_data$p5_strategy[i], full_data$p7_strategy[i])
   full_data$p2_median[i] = median(full_data$p2_strategy[i], full_data$p4_strategy[i], full_data$p6_strategy[i], full_data$p8_strategy[i])
  }
  
  if (full_data$player_in_group[i] == 6)
  {full_data$p1_average[i] = (full_data$p1_strategy[i] + full_data$p3_strategy[i] + full_data$p5_strategy[i] + full_data$p7_strategy[i] + full_data$p9_strategy[i] + full_data$p11_strategy[i]) / 6
   full_data$p2_average[i] = (full_data$p2_strategy[i] + full_data$p4_strategy[i] + full_data$p6_strategy[i] + full_data$p8_strategy[i] + full_data$p10_strategy[i] + full_data$p12_strategy[i]) / 6
   full_data$p1_median[i] = median(full_data$p1_strategy[i], full_data$p3_strategy[i], full_data$p5_strategy[i], full_data$p7_strategy[i])
   full_data$p2_median[i] = median(full_data$p2_strategy[i], full_data$p4_strategy[i], full_data$p6_strategy[i], full_data$p8_strategy[i])
  }
}

# calculate players payoff
full_data = full_data %>% mutate(p1_payoff = payoff1Aa*p1_strategy*p2_average + payoff1Ab*p1_strategy*(1-p2_average) + payoff1Ba*(1-p1_strategy)*p2_average + payoff1Bb*(1-p1_strategy)*(1-p2_average))
full_data = full_data %>% mutate(p3_payoff = payoff1Aa*p3_strategy*p2_average + payoff1Ab*p3_strategy*(1-p2_average) + payoff1Ba*(1-p3_strategy)*p2_average + payoff1Bb*(1-p3_strategy)*(1-p2_average))
full_data = full_data %>% mutate(p5_payoff = payoff1Aa*p5_strategy*p2_average + payoff1Ab*p5_strategy*(1-p2_average) + payoff1Ba*(1-p5_strategy)*p2_average + payoff1Bb*(1-p5_strategy)*(1-p2_average))
full_data = full_data %>% mutate(p7_payoff = payoff1Aa*p7_strategy*p2_average + payoff1Ab*p7_strategy*(1-p2_average) + payoff1Ba*(1-p7_strategy)*p2_average + payoff1Bb*(1-p7_strategy)*(1-p2_average))
full_data = full_data %>% mutate(p9_payoff = payoff1Aa*p9_strategy*p2_average + payoff1Ab*p9_strategy*(1-p2_average) + payoff1Ba*(1-p9_strategy)*p2_average + payoff1Bb*(1-p9_strategy)*(1-p2_average))
full_data = full_data %>% mutate(p11_payoff = payoff1Aa*p11_strategy*p2_average + payoff1Ab*p11_strategy*(1-p2_average) + payoff1Ba*(1-p11_strategy)*p2_average + payoff1Bb*(1-p11_strategy)*(1-p2_average))

full_data = full_data %>% mutate(p2_payoff = payoff2Aa*p1_average*p2_strategy + payoff2Ab*p1_average*(1-p2_strategy) + payoff2Ba*(1-p1_average)*p2_strategy + payoff2Bb*(1-p1_average)*(1-p2_strategy))
full_data = full_data %>% mutate(p4_payoff = payoff2Aa*p1_average*p4_strategy + payoff2Ab*p1_average*(1-p4_strategy) + payoff2Ba*(1-p1_average)*p4_strategy + payoff2Bb*(1-p1_average)*(1-p4_strategy))
full_data = full_data %>% mutate(p6_payoff = payoff2Aa*p1_average*p6_strategy + payoff2Ab*p1_average*(1-p6_strategy) + payoff2Ba*(1-p1_average)*p6_strategy + payoff2Bb*(1-p1_average)*(1-p6_strategy))
full_data = full_data %>% mutate(p8_payoff = payoff2Aa*p1_average*p8_strategy + payoff2Ab*p1_average*(1-p8_strategy) + payoff2Ba*(1-p1_average)*p8_strategy + payoff2Bb*(1-p1_average)*(1-p8_strategy))
full_data = full_data %>% mutate(p10_payoff = payoff2Aa*p1_average*p10_strategy + payoff2Ab*p1_average*(1-p10_strategy) + payoff2Ba*(1-p1_average)*p10_strategy + payoff2Bb*(1-p1_average)*(1-p10_strategy))
full_data = full_data %>% mutate(p12_payoff = payoff2Aa*p1_average*p12_strategy + payoff2Ab*p1_average*(1-p12_strategy) + payoff2Ba*(1-p1_average)*p12_strategy + payoff2Bb*(1-p1_average)*(1-p12_strategy))

# time remaining
full_data = full_data %>% mutate(timeleft = ifelse(num_subperiods==25, 24-tick, 299-tick))

# deviation from NE and MM
full_data = full_data %>% mutate(strategy_diff = abs(p1_average - p2_average))
full_data = full_data %>% mutate(NEdiff = sqrt((p1_average - p1NEmix)^2 + (p2_average - p2NEmix)^2))
full_data = full_data %>% mutate(MMdiff = sqrt((p1_average - p1MMmix)^2 + (p2_average - p2MMmix)^2))
full_data = full_data %>% mutate(p1NEdiff = sqrt((p1_average - p1NEmix)^2))
full_data = full_data %>% mutate(p1MMdiff = sqrt((p1_average - p1MMmix)^2))
full_data = full_data %>% mutate(p2NEdiff = sqrt((p2_average - p2NEmix)^2))
full_data = full_data %>% mutate(p2MMdiff = sqrt((p2_average - p2MMmix)^2))
full_data = full_data %>% mutate(p1NEdiffsgn = p1_strategy - p1NEmix)
full_data = full_data %>% mutate(p1MMdiffsgn = p1_strategy - p1MMmix)
full_data = full_data %>% mutate(p2NEdiffsgn = p2_strategy - p2NEmix)
full_data = full_data %>% mutate(p2MMdiffsgn = p2_strategy - p2MMmix)

# create a new treatment variable combining games, action sets and time
full_data = full_data %>% mutate(treatment = 0)
for(m in 1:length(full_data$p1_strategy)){
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
full_data = filter(full_data, tick>=36 | num_subperiods==25)

# drop first period of each block
full_data = filter(full_data, round!=1 & round!=5 & round!=9 & round!=13 & round!=17)

# create unique pairs
gametype = unique(full_data$game)
uniqueround = unique(full_data$session_round_id)

# create merge dataset
merge_data = full_data
# merge_data = merge_data %>% select(-c(p1_code, p1_role, p1_strategy, p1_target, p1_payoff))
# merge_data = merge_data %>% select(-c(p2_code, p2_role, p2_strategy, p2_target, p2_payoff))
# merge_data = merge_data %>% select(-c(p3_code, p3_role, p3_strategy, p3_target, p3_payoff))
# merge_data = merge_data %>% select(-c(p4_code, p4_role, p4_strategy, p4_target, p4_payoff))
# merge_data = merge_data %>% select(-c(p5_code, p5_role, p5_strategy, p5_target, p5_payoff))
# merge_data = merge_data %>% select(-c(p6_code, p6_role, p6_strategy, p6_target, p6_payoff))
# merge_data = merge_data %>% select(-c(p7_code, p7_role, p7_strategy, p7_target, p7_payoff))
# merge_data = merge_data %>% select(-c(p8_code, p8_role, p8_strategy, p8_target, p8_payoff))
# merge_data = merge_data %>% select(-c(p9_code, p9_role, p9_strategy, p9_target, p9_payoff))
# merge_data = merge_data %>% select(-c(p10_code, p10_role, p10_strategy, p10_target, p10_payoff))
# merge_data = merge_data %>% select(-c(p11_code, p11_role, p11_strategy, p11_target, p11_payoff))
# merge_data = merge_data %>% select(-c(p12_code, p12_role, p12_strategy, p12_target, p12_payoff))
# merge_data = merge_data %>% select(-c(player_in_group))
write.csv(merge_data, "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/merge_mm.csv")
rm(merge_data)


##########Create dataset similar to pairwise matching##########
# create dataset for player 1 and 2
data_1 = full_data
data_1 = data_1 %>% select(-c(p3_code, p3_role, p3_strategy, p3_target, p3_payoff))
data_1 = data_1 %>% select(-c(p4_code, p4_role, p4_strategy, p4_target, p4_payoff))
data_1 = data_1 %>% select(-c(p5_code, p5_role, p5_strategy, p5_target, p5_payoff))
data_1 = data_1 %>% select(-c(p6_code, p6_role, p6_strategy, p6_target, p6_payoff))
data_1 = data_1 %>% select(-c(p7_code, p7_role, p7_strategy, p7_target, p7_payoff))
data_1 = data_1 %>% select(-c(p8_code, p8_role, p8_strategy, p8_target, p8_payoff))
data_1 = data_1 %>% select(-c(p9_code, p9_role, p9_strategy, p9_target, p9_payoff))
data_1 = data_1 %>% select(-c(p10_code, p10_role, p10_strategy, p10_target, p10_payoff))
data_1 = data_1 %>% select(-c(p11_code, p11_role, p11_strategy, p11_target, p11_payoff))
data_1 = data_1 %>% select(-c(p12_code, p12_role, p12_strategy, p12_target, p12_payoff))
data_1 = data_1 %>% select(-c(player_in_group))

# create dataset for player 3 and 4
data_2 = full_data
data_2 = data_2 %>% select(-c(p1_code, p1_role, p1_strategy, p1_target, p1_payoff))
data_2 = data_2 %>% select(-c(p2_code, p2_role, p2_strategy, p2_target, p2_payoff))
data_2 = data_2 %>% select(-c(p5_code, p5_role, p5_strategy, p5_target, p5_payoff))
data_2 = data_2 %>% select(-c(p6_code, p6_role, p6_strategy, p6_target, p6_payoff))
data_2 = data_2 %>% select(-c(p7_code, p7_role, p7_strategy, p7_target, p7_payoff))
data_2 = data_2 %>% select(-c(p8_code, p8_role, p8_strategy, p8_target, p8_payoff))
data_2 = data_2 %>% select(-c(p9_code, p9_role, p9_strategy, p9_target, p9_payoff))
data_2 = data_2 %>% select(-c(p10_code, p10_role, p10_strategy, p10_target, p10_payoff))
data_2 = data_2 %>% select(-c(p11_code, p11_role, p11_strategy, p11_target, p11_payoff))
data_2 = data_2 %>% select(-c(p12_code, p12_role, p12_strategy, p12_target, p12_payoff))
data_2 = data_2 %>% select(-c(player_in_group))
names(data_2)[names(data_2)=="p3_code"]="p1_code"
names(data_2)[names(data_2)=="p3_role"]="p1_role"
names(data_2)[names(data_2)=="p3_strategy"]="p1_strategy"
names(data_2)[names(data_2)=="p3_target"]="p1_target"
names(data_2)[names(data_2)=="p3_payoff"]="p1_payoff"
names(data_2)[names(data_2)=="p4_code"]="p2_code"
names(data_2)[names(data_2)=="p4_role"]="p2_role"
names(data_2)[names(data_2)=="p4_strategy"]="p2_strategy"
names(data_2)[names(data_2)=="p4_target"]="p2_target"
names(data_2)[names(data_2)=="p4_payoff"]="p2_payoff"
data_2 = data_2 %>% mutate(id_in_subsession = 2)

# create dataset for player 5 and 6
data_3 = full_data
data_3 = data_3 %>% select(-c(p1_code, p1_role, p1_strategy, p1_target, p1_payoff))
data_3 = data_3 %>% select(-c(p2_code, p2_role, p2_strategy, p2_target, p2_payoff))
data_3 = data_3 %>% select(-c(p3_code, p3_role, p3_strategy, p3_target, p3_payoff))
data_3 = data_3 %>% select(-c(p4_code, p4_role, p4_strategy, p4_target, p4_payoff))
data_3 = data_3 %>% select(-c(p7_code, p7_role, p7_strategy, p7_target, p7_payoff))
data_3 = data_3 %>% select(-c(p8_code, p8_role, p8_strategy, p8_target, p8_payoff))
data_3 = data_3 %>% select(-c(p9_code, p9_role, p9_strategy, p9_target, p9_payoff))
data_3 = data_3 %>% select(-c(p10_code, p10_role, p10_strategy, p10_target, p10_payoff))
data_3 = data_3 %>% select(-c(p11_code, p11_role, p11_strategy, p11_target, p11_payoff))
data_3 = data_3 %>% select(-c(p12_code, p12_role, p12_strategy, p12_target, p12_payoff))
data_3 = data_3 %>% select(-c(player_in_group))
names(data_3)[names(data_3)=="p5_code"]="p1_code"
names(data_3)[names(data_3)=="p5_role"]="p1_role"
names(data_3)[names(data_3)=="p5_strategy"]="p1_strategy"
names(data_3)[names(data_3)=="p5_target"]="p1_target"
names(data_3)[names(data_3)=="p5_payoff"]="p1_payoff"
names(data_3)[names(data_3)=="p6_code"]="p2_code"
names(data_3)[names(data_3)=="p6_role"]="p2_role"
names(data_3)[names(data_3)=="p6_strategy"]="p2_strategy"
names(data_3)[names(data_3)=="p6_target"]="p2_target"
names(data_3)[names(data_3)=="p6_payoff"]="p2_payoff"
data_3 = data_3 %>% mutate(id_in_subsession = 3)

# create dataset for player 7 and 8
data_4 = full_data
data_4 = data_4 %>% select(-c(p1_code, p1_role, p1_strategy, p1_target, p1_payoff))
data_4 = data_4 %>% select(-c(p2_code, p2_role, p2_strategy, p2_target, p2_payoff))
data_4 = data_4 %>% select(-c(p3_code, p3_role, p3_strategy, p3_target, p3_payoff))
data_4 = data_4 %>% select(-c(p4_code, p4_role, p4_strategy, p4_target, p4_payoff))
data_4 = data_4 %>% select(-c(p5_code, p5_role, p5_strategy, p5_target, p5_payoff))
data_4 = data_4 %>% select(-c(p6_code, p6_role, p6_strategy, p6_target, p6_payoff))
data_4 = data_4 %>% select(-c(p9_code, p9_role, p9_strategy, p9_target, p9_payoff))
data_4 = data_4 %>% select(-c(p10_code, p10_role, p10_strategy, p10_target, p10_payoff))
data_4 = data_4 %>% select(-c(p11_code, p11_role, p11_strategy, p11_target, p11_payoff))
data_4 = data_4 %>% select(-c(p12_code, p12_role, p12_strategy, p12_target, p12_payoff))
data_4 = data_4 %>% select(-c(player_in_group))
names(data_4)[names(data_4)=="p7_code"]="p1_code"
names(data_4)[names(data_4)=="p7_role"]="p1_role"
names(data_4)[names(data_4)=="p7_strategy"]="p1_strategy"
names(data_4)[names(data_4)=="p7_target"]="p1_target"
names(data_4)[names(data_4)=="p7_payoff"]="p1_payoff"
names(data_4)[names(data_4)=="p8_code"]="p2_code"
names(data_4)[names(data_4)=="p8_role"]="p2_role"
names(data_4)[names(data_4)=="p8_strategy"]="p2_strategy"
names(data_4)[names(data_4)=="p8_target"]="p2_target"
names(data_4)[names(data_4)=="p8_payoff"]="p2_payoff"
data_4 = data_4 %>% mutate(id_in_subsession = 4)

# create dataset for player 9 and 10
data_5 = full_data
data_5 = data_5 %>% select(-c(p1_code, p1_role, p1_strategy, p1_target, p1_payoff))
data_5 = data_5 %>% select(-c(p2_code, p2_role, p2_strategy, p2_target, p2_payoff))
data_5 = data_5 %>% select(-c(p3_code, p3_role, p3_strategy, p3_target, p3_payoff))
data_5 = data_5 %>% select(-c(p4_code, p4_role, p4_strategy, p4_target, p4_payoff))
data_5 = data_5 %>% select(-c(p5_code, p5_role, p5_strategy, p5_target, p5_payoff))
data_5 = data_5 %>% select(-c(p6_code, p6_role, p6_strategy, p6_target, p6_payoff))
data_5 = data_5 %>% select(-c(p7_code, p7_role, p7_strategy, p7_target, p7_payoff))
data_5 = data_5 %>% select(-c(p8_code, p8_role, p8_strategy, p8_target, p8_payoff))
data_5 = data_5 %>% select(-c(p11_code, p11_role, p11_strategy, p11_target, p11_payoff))
data_5 = data_5 %>% select(-c(p12_code, p12_role, p12_strategy, p12_target, p12_payoff))
data_5 = data_5 %>% select(-c(player_in_group))
names(data_5)[names(data_5)=="p9_code"]="p1_code"
names(data_5)[names(data_5)=="p9_role"]="p1_role"
names(data_5)[names(data_5)=="p9_strategy"]="p1_strategy"
names(data_5)[names(data_5)=="p9_target"]="p1_target"
names(data_5)[names(data_5)=="p9_payoff"]="p1_payoff"
names(data_5)[names(data_5)=="p10_code"]="p2_code"
names(data_5)[names(data_5)=="p10_role"]="p2_role"
names(data_5)[names(data_5)=="p10_strategy"]="p2_strategy"
names(data_5)[names(data_5)=="p10_target"]="p2_target"
names(data_5)[names(data_5)=="p10_payoff"]="p2_payoff"
data_5 = data_5 %>% mutate(id_in_subsession = 5)

# create dataset for player 11 and 12
data_6 = full_data
data_6 = data_6 %>% select(-c(p1_code, p1_role, p1_strategy, p1_target, p1_payoff))
data_6 = data_6 %>% select(-c(p2_code, p2_role, p2_strategy, p2_target, p2_payoff))
data_6 = data_6 %>% select(-c(p3_code, p3_role, p3_strategy, p3_target, p3_payoff))
data_6 = data_6 %>% select(-c(p4_code, p4_role, p4_strategy, p4_target, p4_payoff))
data_6 = data_6 %>% select(-c(p5_code, p5_role, p5_strategy, p5_target, p5_payoff))
data_6 = data_6 %>% select(-c(p6_code, p6_role, p6_strategy, p6_target, p6_payoff))
data_6 = data_6 %>% select(-c(p7_code, p7_role, p7_strategy, p7_target, p7_payoff))
data_6 = data_6 %>% select(-c(p8_code, p8_role, p8_strategy, p8_target, p8_payoff))
data_6 = data_6 %>% select(-c(p9_code, p9_role, p9_strategy, p9_target, p9_payoff))
data_6 = data_6 %>% select(-c(p10_code, p10_role, p10_strategy, p10_target, p10_payoff))
data_6 = data_6 %>% select(-c(player_in_group))
names(data_6)[names(data_6)=="p11_code"]="p1_code"
names(data_6)[names(data_6)=="p11_role"]="p1_role"
names(data_6)[names(data_6)=="p11_strategy"]="p1_strategy"
names(data_6)[names(data_6)=="p11_target"]="p1_target"
names(data_6)[names(data_6)=="p11_payoff"]="p1_payoff"
names(data_6)[names(data_6)=="p12_code"]="p2_code"
names(data_6)[names(data_6)=="p12_role"]="p2_role"
names(data_6)[names(data_6)=="p12_strategy"]="p2_strategy"
names(data_6)[names(data_6)=="p12_target"]="p2_target"
names(data_6)[names(data_6)=="p12_payoff"]="p2_payoff"
data_6 = data_6 %>% mutate(id_in_subsession = 6)

# merge all six datasets and adjust a few variables
merge_data = rbind(data_1, data_2, data_3, data_4, data_5, data_6)
merge_data = arrange(merge_data, merge_data$session_code, merge_data$subsession_id, merge_data$id_in_subsession, merge_data$tick)

merge_data$pair_id = paste(merge_data$p1_code, merge_data$p2_code, sep = "_")
merge_data$round_pair_id = paste(merge_data$round, merge_data$pair_id,  sep = "_")
merge_data$session_round_pair_id = paste(merge_data$session_code, merge_data$round_pair_id, sep = "_")

# remove NA data
merge_data = filter(merge_data, is.na(p1_strategy)==FALSE | is.na(p2_strategy)==FALSE)

# write csv file
write.csv(merge_data, "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/merge_mm.csv")
rm(data_1, data_2, data_3, data_4, data_5, data_6)


##########Directional learning preparation##########
# replace wide data with long data
full_data = merge_data
rm(merge_data)

# regret and directional learning variables
full_data = full_data %>% mutate(p1_uhat_a1 = payoff1Aa*p2_average + payoff1Ab*(1-p2_average))
full_data = full_data %>% mutate(p1_uhat_a0 = payoff1Ba*p2_average + payoff1Bb*(1-p2_average))
full_data = full_data %>% mutate(p1_ahat = ifelse(p1_uhat_a1 > p1_uhat_a0, 1, ifelse(p1_uhat_a1 < p1_uhat_a0, 0, p1_strategy)))
full_data = full_data %>% mutate(p1_uhat = payoff1Aa*p1_ahat*p2_average + payoff1Ab*p1_ahat*(1-p2_average) + payoff1Ba*(1-p1_ahat)*p2_average + payoff1Bb*(1-p1_ahat)*(1-p2_average))
#full_data = full_data %>% mutate(p1_regret = (p1_uhat - p1_payoff)/100)
full_data = full_data %>% mutate(p1_regret = ifelse(game=='AMPa', (p1_uhat - p1_payoff)/800, ifelse(game=='AMPb', (p1_uhat - p1_payoff)/700, (p1_uhat - p1_payoff)/400)))
full_data = full_data %>% mutate(p1_direction = p1_ahat - p1_strategy)
full_data = full_data %>% mutate(p1_sign = ifelse(p1_direction > 0, 1, ifelse(p1_direction < 0, -1, 0)))
full_data = full_data %>% mutate(p1_regret_sign = p1_regret * p1_sign)

full_data = full_data %>% mutate(p2_uhat_a1 = payoff2Aa*p1_average + payoff2Ba*(1-p1_average))
full_data = full_data %>% mutate(p2_uhat_a0 = payoff2Ab*p1_average + payoff2Bb*(1-p1_average))
full_data = full_data %>% mutate(p2_ahat = ifelse(p2_uhat_a1 > p2_uhat_a0, 1, ifelse(p2_uhat_a1 < p2_uhat_a0, 0, p2_strategy)))
full_data = full_data %>% mutate(p2_uhat = payoff2Aa*p1_average*p2_ahat + payoff2Ab*p1_average*(1-p2_ahat) + payoff2Ba*(1-p1_average)*p2_ahat + payoff2Bb*(1-p1_average)*(1-p2_ahat))
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
write.dta(full_data, "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_production_mm.dta")

# write csv file
write.csv(full_data, "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/merge_mm.csv")


##########Figure 1 (not used): Individual and population movement##########
# row player movements
for (i in 1:length(uniqueround)){
  round_data = subset(full_data, session_round_id == uniqueround[i])
  
  title = paste(as.character(uniqueround[i]), as.character(round_data$game[1]), 
                as.character(ifelse(round_data$pure_strategy[1]=='TRUE', 'pure strategy', 'mixed strategy')), 
                as.character(ifelse(round_data$num_subperiods[1]==0, 'continuous time', 'discrete time')),
                sep = "_")
  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/mean_matching/row player/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 1200, height = 800)
  par(mfrow=c(2,3))
  
  plot(round_data$tick, round_data$p1_strategy,
       type='l', xlab = "time", ylab = "p1 strategy", ylim=c(0:1), col="blue",
       main = 'p1 strategy over time')
  lines(round_data$tick, round_data$p2_average, col="red")
  abline(h = round_data$p1NEmix[1], col="black")
  
  plot(round_data$tick, round_data$p3_strategy,
       type='l', xlab = "time", ylab = "p3 strategy", ylim=c(0:1), col="blue",
       main = 'p3 strategy over time')
  lines(round_data$tick, round_data$p2_average, col="red")
  abline(h = round_data$p1NEmix[1], col="black")
  
  plot(round_data$tick, round_data$p5_strategy,
       type='l', xlab = "time", ylab = "p5 strategy", ylim=c(0:1), col="blue",
       main = 'p5 strategy over time')
  lines(round_data$tick, round_data$p2_average, col="red")
  abline(h = round_data$p1NEmix[1], col="black")
  
  plot(round_data$tick, round_data$p7_strategy,
       type='l', xlab = "time", ylab = "p7 strategy", ylim=c(0:1), col="blue",
       main = 'p7 strategy over time')
  lines(round_data$tick, round_data$p2_average, col="red")
  abline(h = round_data$p1NEmix[1], col="black")
  
  plot(round_data$tick, round_data$p9_strategy,
       type='l', xlab = "time", ylab = "p9 strategy", ylim=c(0:1), col="blue",
       main = 'p9 strategy over time')
  lines(round_data$tick, round_data$p2_average, col="red")
  abline(h = round_data$p1NEmix[1], col="black")
  
  plot(round_data$tick, round_data$p11_strategy,
       type='l', xlab = "time", ylab = "p11 strategy", ylim=c(0:1), col="blue",
       main = 'p11 strategy over time')
  lines(round_data$tick, round_data$p2_average, col="red")
  abline(h = round_data$p1NEmix[1], col="black")
  
  # plot(round_data$tick, round_data$p1_average, 
  #      type='l', xlab = "time", ylab = "mean strategy", ylim=c(0:1), col="blue",
  #      main = 'mean strategy over time')
  # lines(round_data$tick, round_data$p2_average, col="red")
  # abline(h = round_data$p1NEmix[1], col="blue")
  # abline(h = round_data$p2NEmix[1], col="red")
  
  dev.off()
}

# column player movements
for (i in 1:length(uniqueround)){
  round_data = subset(full_data, session_round_id == uniqueround[i])
  
  title = paste(as.character(uniqueround[i]), as.character(round_data$game[1]), 
                as.character(ifelse(round_data$pure_strategy[1]=='TRUE', 'pure strategy', 'mixed strategy')), 
                as.character(ifelse(round_data$num_subperiods[1]==0, 'continuous time', 'discrete time')),
                sep = "_")
  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/mean_matching/column player/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 1200, height = 800)
  par(mfrow=c(2,3))
  
  plot(round_data$tick, round_data$p2_strategy,
       type='l', xlab = "time", ylab = "p2 strategy", ylim=c(0:1), col="red",
       main = 'p2 strategy over time')
  lines(round_data$tick, round_data$p1_average, col="blue")
  abline(h = round_data$p2NEmix[1], col="black")
  
  plot(round_data$tick, round_data$p4_strategy,
       type='l', xlab = "time", ylab = "p4 strategy", ylim=c(0:1), col="red",
       main = 'p4 strategy over time')
  lines(round_data$tick, round_data$p1_average, col="blue")
  abline(h = round_data$p2NEmix[1], col="black")
  
  plot(round_data$tick, round_data$p6_strategy,
       type='l', xlab = "time", ylab = "p6 strategy", ylim=c(0:1), col="red",
       main = 'p6 strategy over time')
  lines(round_data$tick, round_data$p1_average, col="blue")
  abline(h = round_data$p2NEmix[1], col="black")
  
  plot(round_data$tick, round_data$p8_strategy,
       type='l', xlab = "time", ylab = "p8 strategy", ylim=c(0:1), col="red",
       main = 'p8 strategy over time')
  lines(round_data$tick, round_data$p1_average, col="blue")
  abline(h = round_data$p2NEmix[1], col="black")
  
  plot(round_data$tick, round_data$p10_strategy,
       type='l', xlab = "time", ylab = "p10 strategy", ylim=c(0:1), col="red",
       main = 'p10 strategy over time')
  lines(round_data$tick, round_data$p1_average, col="blue")
  abline(h = round_data$p2NEmix[1], col="black")
  
  plot(round_data$tick, round_data$p12_strategy,
       type='l', xlab = "time", ylab = "p12 strategy", ylim=c(0:1), col="red",
       main = 'p12 strategy over time')
  lines(round_data$tick, round_data$p1_average, col="blue")
  abline(h = round_data$p2NEmix[1], col="black")
  
  # plot(round_data$tick, round_data$p1_average, 
  #      type='l', xlab = "time", ylab = "mean strategy", ylim=c(0:1), col="blue",
  #      main = 'mean strategy over time')
  # lines(round_data$tick, round_data$p2_average, col="red")
  # abline(h = round_data$p1NEmix[1], col="blue")
  # abline(h = round_data$p2NEmix[1], col="red")
  
  dev.off()
}

##########Figure 2 (not used): Population movement by treatments##########
# line charts by treatments
# loop over treatments
for (i in 1:max(full_data$treatment)){
  treatment_data = subset(full_data, treatment==i)
  uniqueround_treatment = unique(treatment_data$session_round_id)
  
  # loop over rounds
  for (j in 1:length(uniqueround_treatment)){
    round_data = subset(treatment_data, session_round_id == uniqueround_treatment[j])
    
    # create figure for each treatment
    title = paste('population movement', as.character(round_data$game[1]),
                  as.character(round_data$round[1]),
                  as.character(ifelse(round_data$pure_strategy[1]=='TRUE', 'pure strategy', 'mixed strategy')), 
                  as.character(ifelse(round_data$num_subperiods[1]==0, 'continuous time', 'discrete time')),
                  sep = "_")
    file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/mean_matching/pairs/", title, sep = "")
    file = paste(file, ".png", sep = "")
    png(file, width = 400, height = 300)
    #par(mai=c(0.5,0.5,0.5,1.5))
    #par(mfrow=c(2,4))
    
    # draw population average for row players and column players
    plot(round_data$tick, round_data$p1_average, 
         type='l', xlab = "time", ylab = "population mean", ylim=c(0:1), col="blue",
         main = paste('Sample figures', as.character(round_data$game[1]),
                      as.character(ifelse(round_data$pure_strategy[1]=='TRUE', 'pure', 'mixed')), 
                      as.character(ifelse(round_data$num_subperiods[1]==0, 'continuous', 'discrete')),
                      sep = " "))
    lines(round_data$tick, round_data$p2_average, col="red")
    abline(h = round_data$p1NEmix[1], col="darkgreen", lwd = 2)
    abline(h = round_data$p2NEmix[1], col="purple4", lwd = 2)
    
    legend3d('topright', legend = c('row', 'column', 'row NE', 'column NE'), xpd = TRUE,
           col = c('blue', 'red', 'darkgreen', 'purple4'), pch = 16)
    
    dev.off()
  }
}

# box plot by games and treatments
# loop over games
for (i in 1:length(gametype)){
  
  game_data = subset(full_data, game == gametype[i])
  
  # Pure Discrete
  treatment_data = subset(game_data, PD == 1)
  
  title = paste('population aggregate', as.character(treatment_data$game[1]),
                as.character(ifelse(treatment_data$pure_strategy[1]=='TRUE', 'pure strategy', 'mixed strategy')), 
                as.character(ifelse(treatment_data$num_subperiods[1]==0, 'continuous time', 'discrete time')),
                sep = "_")
  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/mean_matching/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 800, height = 400)
  par(mfrow=c(1,2))
  par(mai=c(0.5,0.5,0.5,1.5))
  
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
  
  legend(x=27, y=0.1, legend = c('row', 'column', 'NE', 'MM'), xpd = TRUE,
         fill = c('blue', 'red', 'darkgreen', 'purple4'))
  
  dev.off()
  
  # Pure Continuous
  treatment_data = subset(game_data, PC == 1)
  
  title = paste('population aggregate', as.character(treatment_data$game[1]),
                as.character(ifelse(treatment_data$pure_strategy[1]=='TRUE', 'pure strategy', 'mixed strategy')), 
                as.character(ifelse(treatment_data$num_subperiods[1]==0, 'continuous time', 'discrete time')),
                sep = "_")
  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/mean_matching/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 800, height = 800)
  par(mfrow=c(2,1))
  par(mai=c(0.5,0.5,0.5,1.5))
  
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
  
  legend(x=320, y=0.1, legend = c('row', 'column', 'NE', 'MM'), xpd = TRUE,
         fill = c('blue', 'red', 'darkgreen', 'purple4'))
  
  dev.off()
  
  # Mixed Discrete
  treatment_data = subset(game_data, MD == 1)
  
  title = paste('population aggregate', as.character(treatment_data$game[1]),
                as.character(ifelse(treatment_data$pure_strategy[1]=='TRUE', 'pure strategy', 'mixed strategy')), 
                as.character(ifelse(treatment_data$num_subperiods[1]==0, 'continuous time', 'discrete time')),
                sep = "_")
  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/mean_matching/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 800, height = 400)
  par(mfrow=c(1,2))
  par(mai=c(0.5,0.5,0.5,1.5))
  
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
  
  legend(x=27, y=0.1, legend = c('row', 'column', 'NE', 'MM'), xpd = TRUE,
         fill = c('blue', 'red', 'darkgreen', 'purple4'))
  
  dev.off()
  
  # Mixed Continuous
  treatment_data = subset(game_data, MC == 1)
  
  title = paste('population aggregate', as.character(treatment_data$game[1]),
                as.character(ifelse(treatment_data$pure_strategy[1]=='TRUE', 'pure strategy', 'mixed strategy')), 
                as.character(ifelse(treatment_data$num_subperiods[1]==0, 'continuous time', 'discrete time')),
                sep = "_")
  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/mean_matching/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 800, height = 800)
  par(mfrow=c(2,1))
  par(mai=c(0.5,0.5,0.5,1.5))
  
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
  
  legend(x=320, y=0.1, legend = c('row', 'column', 'NE', 'MM'), xpd = TRUE,
         fill = c('blue', 'red', 'darkgreen', 'purple4'))
  
  dev.off()
  
}

##########Figure 3 (not used): Population distance from NE by treatments##########
# loop over treatments
for (i in 1:max(full_data$treatment)){
  treatment_data = subset(full_data, treatment==i)
  uniqueround_treatment = unique(treatment_data$session_round_id)
  
  # create figure for eacah treatment
  title = paste('distance from NE', as.character(treatment_data$game[1]),
                as.character(ifelse(treatment_data$pure_strategy[1]=='TRUE', 'pure strategy', 'mixed strategy')), 
                as.character(ifelse(treatment_data$num_subperiods[1]==0, 'continuous time', 'discrete time')),
                sep = "_")
  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/mean_matching/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 1200, height = 600)
  par(mfrow=c(2,4))
  
  # loop over rounds
  for (j in 1:length(uniqueround_treatment)){
    round_data = subset(treatment_data, session_round_id == uniqueround_treatment[j])
    
    # draw population average for row players and column players
    plot(round_data$tick, round_data$p1NEdiff, 
         type='l', xlab = "time", ylab = "population mean", ylim=c(0:1), col="blue",
         main = paste('round', as.character(round_data$round[1]),
                      'row player: blue', 'column player:red',
                      sep = ' '))
    lines(round_data$tick, round_data$p2NEdiff, col="red")
  }
  
  dev.off()
}

##########Figure 4 (not used): Time average in IDDS##########
part_data = subset(full_data, game=='IDDS')

# loop over IDDS treatments
for (i in 9:12){
  treatment_data = subset(part_data, treatment==i)
  
  # create time average
  mean_data = data.frame(p1mean = tapply(treatment_data$p1_average, treatment_data$tick, mean),
                         p2mean = tapply(treatment_data$p2_average, treatment_data$tick, mean),
                         tick = tapply(treatment_data$tick, treatment_data$tick, mean))
  
  # create figure titles
  title = paste(as.character(treatment_data$game[1]),
                as.character(ifelse(treatment_data$pure_strategy[1]=='TRUE', 'pure strategy', 'mixed strategy')), 
                as.character(ifelse(treatment_data$num_subperiods[1]==0, 'continuous time', 'discrete time')),
                'meanmatch', sep = "_")
  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/IDDS/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 1200, height = 600)
  par(mfrow=c(1,2))
  
  # draw figures
  plot(mean_data$tick, mean_data$p1mean, 
       type='l', xlab = "time", ylab = "population mean", ylim=c(0:1), col="blue", lwd=2,
       main = 'row population')
  abline(h = treatment_data$p1NEmix[1], lwd=3)
  
  plot(mean_data$tick, mean_data$p2mean, 
       type='l', xlab = "time", ylab = "population mean", ylim=c(0:1), col="red", lwd=2,
       main = 'column population')
  abline(h = treatment_data$p2NEmix[1], lwd=3)
  
  
  dev.off()
}


##########Figure 5: Individual cyclic behavior##########
# select game
part_data = subset(full_data, game=='AMPa')
uniqueID_part = unique(part_data$session_round_id) 

# loop over pairs
for(i in 1:length(uniqueID_part)){
  
  pairRoundData = subset(part_data, session_round_id==uniqueID_part[i])
  
  title = paste(as.character(uniqueID_part[i]), as.character(pairRoundData$game[1]), 
                as.character(ifelse(pairRoundData$pure_strategy[1]==TRUE, 'pure strategy', 'mixed strategy')), 
                as.character(ifelse(pairRoundData$num_subperiods[1]==0, 'continuous time', 'discrete time')),
                sep = "_")
  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/mean_matching/pairs/", title, sep = "")
  file = paste(file, ".png", sep = "")
  
  png(file)
  #par(mai=c(0.5,0.5,0.5,1.5))
  #par(mfrow=c(2,4))
  
  # create data center
  pairRoundData = pairRoundData %>% mutate(p1center = 0.5)
  pairRoundData = pairRoundData %>% mutate(p2center = 0.5)
  
  # 3D plot by NE
  plot3D::lines3D(pairRoundData$p1_average, pairRoundData$p2_average, pairRoundData$timeleft, col='blue', 
                  xlab='p1 average', xlim=c(0:1),
                  ylab='p2 average', ylim=c(0:1),
                  zlab='time left',
                  main = paste('MM', as.character(pairRoundData$game[1]), 
                               as.character(ifelse(pairRoundData$pure_strategy[1]==TRUE, 'pure', 'mixed')), 
                               as.character(ifelse(pairRoundData$num_subperiods[1]==0, 'continuous', 'discrete')), sep = ' '),
                  theta=20, phi=30, r=2, d=1, bty='g',
                  cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
  plot3D::lines3D(pairRoundData$p1NEmix, pairRoundData$p2NEmix, pairRoundData$timeleft, col='black', add=TRUE)
  plot3D::lines3D(pairRoundData$p1center, pairRoundData$p2center, pairRoundData$timeleft, col='green', add=TRUE)
  plot3D::lines3D(pairRoundData$p1MMmix, pairRoundData$p2MMmix, pairRoundData$timeleft, col='red', add=TRUE)
  
  legend3d('topright', legend = c('strategy', 'NE', 'MM', 'Center'), xpd = TRUE,
           pch = 16, col = c('blue', 'black', 'red', 'green'))
  
  dev.off()
  
}


##########Figure 6: Cyclical behaviour over time on population level##########
part_data = full_data
#part_data = subset(part_data, tick%%4==0 | num_subperiods == 25)

new_data = subset(full_data, tick==0)
new_data = new_data %>% mutate(cyctype = NA)

# pure strategy settings
#treatmentdatafull = subset(part_data, PD==1)
#treatmentdatafull = subset(part_data, PC==1)
#treatmentdatafull = subset(full_data, MD==1)
treatmentdatafull = subset(part_data, MC==1)

treatmentdatafull = treatmentdatafull %>% mutate(cyctype = "initial point")

# give each observation (strategy profile) a certain cyclical type
for (i in 1:2){
  treatmentdata = subset(treatmentdatafull, game==gametype[i])
  for (m in 1:(length(treatmentdata$tick)-1)){
    if (treatmentdata$session_round_id[m] == treatmentdata$session_round_id[m+1]){
      if (treatmentdata$p1_average[m]>=treatmentdata$p1NEmix[m] & treatmentdata$p2_average[m]>treatmentdata$p2NEmix[m]){
        if (treatmentdata$p1_average[m+1]>treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]<treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_average[m+1]<treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]>treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_average[m+1]<treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]<treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "2.diagonal"}
        if (treatmentdata$p1_average[m+1]>treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]>treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "3.Cdiagonal"}
        if (treatmentdata$p1_average[m+1]==treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]<treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_average[m+1]>treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]==treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_average[m+1]<treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]==treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_average[m+1]==treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]>treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_average[m+1]==treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]==treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "4.stay"}
        #if (abs(treatmentdata$p1_average[m+1]-treatmentdata$p1_average[m])<0.05 & abs(treatmentdata$p2_average[m+1]-treatmentdata$p2_average[m])<0.05)
        #{treatmentdata$cyctype[m+1] = "4.stay"}
      }
      if (treatmentdata$p1_average[m]>treatmentdata$p1NEmix[m] & treatmentdata$p2_average[m]<=treatmentdata$p2NEmix[m]){
        if (treatmentdata$p1_average[m+1]<treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]<treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_average[m+1]>treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]>treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_average[m+1]<treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]>treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "2.diagonal"}
        if (treatmentdata$p1_average[m+1]>treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]<treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "3.Cdiagonal"}
        if (treatmentdata$p1_average[m+1]==treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]<treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_average[m+1]<treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]==treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_average[m+1]>treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]==treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_average[m+1]==treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]>treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_average[m+1]==treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]==treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "4.stay"}
        #if (abs(treatmentdata$p1_average[m+1]-treatmentdata$p1_average[m])<0.05 & abs(treatmentdata$p2_average[m+1]-treatmentdata$p2_average[m])<0.05)
        #{treatmentdata$cyctype[m+1] = "4.stay"}
      }
      if (treatmentdata$p1_average[m]<treatmentdata$p1NEmix[m] & treatmentdata$p2_average[m]>=treatmentdata$p2NEmix[m]){
        if (treatmentdata$p1_average[m+1]>treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]>treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_average[m+1]<treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]<treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_average[m+1]>treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]<treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "2.diagonal"}
        if (treatmentdata$p1_average[m+1]<treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]>treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "3.Cdiagonal"}
        if (treatmentdata$p1_average[m+1]==treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]>treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_average[m+1]>treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]==treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_average[m+1]<treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]==treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_average[m+1]==treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]<treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_average[m+1]==treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]==treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "4.stay"}
        #if (abs(treatmentdata$p1_average[m+1]-treatmentdata$p1_average[m])<0.05 & abs(treatmentdata$p2_average[m+1]-treatmentdata$p2_average[m])<0.05)
        #{treatmentdata$cyctype[m+1] = "4.stay"}
      }
      if (treatmentdata$p1_average[m]<=treatmentdata$p1NEmix[m] & treatmentdata$p2_average[m]<treatmentdata$p2NEmix[m]){
        if (treatmentdata$p1_average[m+1]<treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]>treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_average[m+1]>treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]<treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_average[m+1]>treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]>treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "2.diagonal"}
        if (treatmentdata$p1_average[m+1]<treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]<treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "3.Cdiagonal"}
        if (treatmentdata$p1_average[m+1]==treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]>treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_average[m+1]<treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]==treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "1.CW"}
        if (treatmentdata$p1_average[m+1]>treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]==treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_average[m+1]==treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]<treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "5.CCW"}
        if (treatmentdata$p1_average[m+1]==treatmentdata$p1_average[m] & treatmentdata$p2_average[m+1]==treatmentdata$p2_average[m])
        {treatmentdata$cyctype[m+1] = "4.stay"}
        #if (abs(treatmentdata$p1_average[m+1]-treatmentdata$p1_average[m])<0.05 & abs(treatmentdata$p2_average[m+1]-treatmentdata$p2_average[m])<0.05)
        #{treatmentdata$cyctype[m+1] = "4.stay"}
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

  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/mean_matching/", title, sep = "")
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

# # export dataset to stata
# write.dta(new_data, "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/stata/mp_mm_cycles.dta")
# 
# # seperate legend
# library(plot3D)
# legend3d('topright', legend = c('1.CW', '2.diagonal', '3.Cdiagonal', '4.stay', '5.CCW'), xpd = TRUE,
#          fill = c('#D72B13', '#ECCF05', 'grey', '#7BBE56', "#4B54AF"))