library(dplyr)
library(lattice)
library(MASS)
library(latticeExtra)
library(multiwayvcov)
library(lmtest)
library(plm)
library(mice)
library(xts)
library(tseries)
library(haven)

##########Data preparation##########
# load data 
bimatrix_choice <- read.csv("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/production_aggregate/bimatrix_pilot_aggregate.csv", header = T)

# create round variable in choice data and create full dataset
bimatrix_choice$round = as.double(substring(bimatrix_choice$subsession_id, 2, 3))
full_data <- bimatrix_choice

# create round/pair id
full_data$pair_id = paste(full_data$p1_code, full_data$p2_code, sep = "")
full_data$round_pair_id = paste(full_data$round, full_data$pair_id,  sep = "")

# add group variables
full_data = full_data %>% mutate(game1 = ifelse(payoff1Aa == 200,1,0))
full_data = full_data %>% mutate(game2 = ifelse(payoff1Aa == 300,1,0))
full_data = full_data %>% mutate(game3 = ifelse(payoff1Aa == 800,1,0))
full_data = full_data %>% mutate(game = ifelse(payoff1Aa == 800,"8002",ifelse(payoff1Aa == 300, "3117","2042")))
#full_data = filter(full_data, game !='2042')

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

# regret and directional learning variables
full_data = full_data %>% mutate(p1_uhat_a1 = payoff1Aa*p2_strategy + payoff1Ab*(1-p2_strategy))
full_data = full_data %>% mutate(p1_uhat_a0 = payoff1Ba*p2_strategy + payoff1Bb*(1-p2_strategy))
full_data = full_data %>% mutate(p1_ahat = ifelse(p1_uhat_a1 > p1_uhat_a0, 1, ifelse(p1_uhat_a1 < p1_uhat_a0, 0, p1_strategy)))
full_data = full_data %>% mutate(p1_uhat = payoff1Aa*p1_ahat*p2_strategy + payoff1Ab*p1_ahat*(1-p2_strategy) + payoff1Ba*(1-p1_ahat)*p2_strategy + payoff1Bb*(1-p1_ahat)*(1-p2_strategy))
#full_data = full_data %>% mutate(p1_regret = (p1_uhat - p1_payoff)/100)
full_data = full_data %>% mutate(p1_regret = ifelse(game=='8002', (p1_uhat - p1_payoff)/800, (p1_uhat - p1_payoff)/700))
full_data = full_data %>% mutate(p1_direction = p1_ahat - p1_strategy)
full_data = full_data %>% mutate(p1_sign = ifelse(p1_direction > 0, 1, ifelse(p1_direction < 0, -1, 0)))
full_data = full_data %>% mutate(p1_regret_sign = p1_regret * p1_sign)

full_data = full_data %>% mutate(p2_uhat_a1 = payoff2Aa*p1_strategy + payoff2Ba*(1-p1_strategy))
full_data = full_data %>% mutate(p2_uhat_a0 = payoff2Ab*p1_strategy + payoff2Bb*(1-p1_strategy))
full_data = full_data %>% mutate(p2_ahat = ifelse(p2_uhat_a1 > p2_uhat_a0, 1, ifelse(p2_uhat_a1 < p2_uhat_a0, 0, p2_strategy)))
full_data = full_data %>% mutate(p2_uhat = payoff2Aa*p1_strategy*p2_ahat + payoff2Ab*p1_strategy*(1-p2_ahat) + payoff2Ba*(1-p1_strategy)*p2_ahat + payoff2Bb*(1-p1_strategy)*(1-p2_ahat))
#full_data = full_data %>% mutate(p2_regret = (p2_uhat - p2_payoff)/100)
full_data = full_data %>% mutate(p2_regret = ifelse(game=='8002', (p2_uhat - p2_payoff)/200, (p2_uhat - p2_payoff)/300))
full_data = full_data %>% mutate(p2_direction = p2_ahat - p2_strategy)
full_data = full_data %>% mutate(p2_sign = ifelse(p2_direction > 0, 1, ifelse(p2_direction < 0, -1, 0)))
full_data = full_data %>% mutate(p2_regret_sign = p2_regret * p2_sign)

# treatment variables for regression
full_data = full_data %>% mutate(dummy_continuous = ifelse(num_subperiods==0, 1, 0))
full_data = full_data %>% mutate(dummy_pure = ifelse(pure_strategy==TRUE, 1, 0))
full_data = full_data %>% mutate(dummy_show_worst = ifelse(show_at_worst==TRUE, 1, 0))
full_data = full_data %>% mutate(dummy_show_best = ifelse(show_best_response==TRUE, 1, 0))
full_data = full_data %>% mutate(dummy_8002 = ifelse(game=='8002', 1, 0))

full_data = full_data %>% mutate(p1_regret_sign_continuous = p1_regret_sign * dummy_continuous)
full_data = full_data %>% mutate(p1_regret_sign_pure = p1_regret_sign * dummy_pure)
full_data = full_data %>% mutate(p1_regret_sign_showworst = p1_regret_sign * dummy_show_worst)
full_data = full_data %>% mutate(p1_regret_sign_showbest = p1_regret_sign * dummy_show_best)
full_data = full_data %>% mutate(p1_regret_sign_8002 = p1_regret_sign * dummy_8002)

full_data = full_data %>% mutate(p2_regret_sign_continuous = p2_regret_sign * dummy_continuous)
full_data = full_data %>% mutate(p2_regret_sign_pure = p2_regret_sign * dummy_pure)
full_data = full_data %>% mutate(p2_regret_sign_showworst = p2_regret_sign * dummy_show_worst)
full_data = full_data %>% mutate(p2_regret_sign_showbest = p2_regret_sign * dummy_show_best)
full_data = full_data %>% mutate(p2_regret_sign_8002 = p2_regret_sign * dummy_8002)

# create first order difference
arrange(full_data, session_code, subsession_id, id_in_subsession, tick)
full_data = full_data %>% mutate(p1_diff = p1_strategy)
full_data = full_data %>% mutate(p1_next = p1_strategy)
full_data = full_data %>% mutate(p2_diff = p2_strategy)
full_data = full_data %>% mutate(p2_next = p2_strategy)
for (k in 1:(length(full_data$p1_strategy)-1)){
  if (full_data$round_pair_id[k] == full_data$round_pair_id[k+1]) {
    full_data$p1_diff[k] = full_data$p1_strategy[k+1] - full_data$p1_strategy[k]
    full_data$p1_next[k] = full_data$p1_strategy[k+1]
    full_data$p2_diff[k] = full_data$p2_strategy[k+1] - full_data$p2_strategy[k]
    full_data$p2_next[k] = full_data$p2_strategy[k+1]
  }
}
full_data = full_data %>% mutate(p1_diff_speed = p1_diff / p1_strategy)
full_data = full_data %>% mutate(p2_diff_speed = p2_diff / p2_strategy)

# export dataset to stata
write_dta(full_data, "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/production_aggregate/bimatrix_pilot_stata.dta")

# MC and MD datasets
uniquePairs = unique(full_data$pair_id)
MC_data = filter(full_data, MC == 1)
MC_data = filter(MC_data, tick>=10 & tick<=170)
MD_data = filter(full_data, MD == 1)
MD_data = filter(MD_data, tick>=2 & tick<=12)
# MC_data = filter(MC_data, session_code == '18_5_22')

##########Analysis 1: Directional learing regression##########
# MC regression for player 1, dependent is directional learning speed, OLS
MC_p1_1 = lm(p1_diff ~ p1_regret_sign, data = MC_data)
vcov_p1_1 = cluster.vcov(MC_p1_1, MC_data$round_pair_id)
coeftest(MC_p1_1, vcov_p1_1)

MC_p1_2 = lm(p1_diff ~ p1_regret_sign + dummy_show_worst + dummy_show_best + dummy_8002, 
             data = MC_data)
vcov_p1_2 = cluster.vcov(MC_p1_2, MC_data$round_pair_id)
coeftest(MC_p1_2, vcov_p1_2)

MC_p1_3 = lm(p1_diff ~ p1_regret_sign + p1_regret_sign_showworst 
            + p1_regret_sign_showbest + p1_regret_sign_8002, data = MC_data)
vcov_p1_3 = cluster.vcov(MC_p1_3, MC_data$round_pair_id)
coeftest(MC_p1_3, vcov_p1_3)

MC_p1_4 = lm(p1_diff ~ p1_regret_sign + dummy_show_worst + dummy_show_best + dummy_8002 
            + p1_regret_sign_showworst + p1_regret_sign_showbest + p1_regret_sign_8002, data = MC_data)
vcov_p1_4 = cluster.vcov(MC_p1_4, MC_data$round_pair_id)
coeftest(MC_p1_4, vcov_p1_4)

# MC regression for player 2, dependent is directional learning speed, OLS
MC_p2_1 = lm(p2_diff ~ p2_regret_sign, 
             data = MC_data)
vcov_p2_1 = cluster.vcov(MC_p2_1, MC_data$round_pair_id)
coeftest(MC_p2_1, vcov_p2_1)

MC_p2_2 = lm(p2_diff ~ p2_regret_sign + dummy_show_worst + dummy_show_best + dummy_8002, 
             data = MC_data)
vcov_p2_2 = cluster.vcov(MC_p2_2, MC_data$round_pair_id)
coeftest(MC_p2_2, vcov_p2_2)

MC_p2_3 = lm(p2_diff ~ p2_regret_sign + p2_regret_sign_showworst 
             + p2_regret_sign_showbest + p2_regret_sign_8002, data = MC_data)
vcov_p2_3 = cluster.vcov(MC_p2_3, MC_data$round_pair_id)
coeftest(MC_p2_3, vcov_p2_3)

MC_p2_4 = lm(p2_diff ~ p2_regret_sign + dummy_show_worst + dummy_show_best + dummy_8002 
             + p2_regret_sign_showworst + p2_regret_sign_showbest + p2_regret_sign_8002, data = MC_data)
vcov_p2_4 = cluster.vcov(MC_p2_4, MC_data$round_pair_id)
coeftest(MC_p2_4, vcov_p2_4)

# MD regression for player 1, dependent is directional learning speed, OLS
MD_p1_1 = lm(p1_diff ~ p1_regret_sign, data = MD_data)
vcov_p1_1 = cluster.vcov(MD_p1_1, MD_data$round_pair_id)
coeftest(MD_p1_1, vcov_p1_1)

MD_p1_2 = lm(p1_diff ~ p1_regret_sign + dummy_show_worst + dummy_show_best + dummy_8002, 
             data = MD_data)
vcov_p1_2 = cluster.vcov(MD_p1_2, MD_data$round_pair_id)
coeftest(MD_p1_2, vcov_p1_2)

MD_p1_3 = lm(p1_diff ~ p1_regret_sign + p1_regret_sign_showworst 
             + p1_regret_sign_showbest + p1_regret_sign_8002, data = MD_data)
vcov_p1_3 = cluster.vcov(MD_p1_3, MD_data$round_pair_id)
coeftest(MD_p1_3, vcov_p1_3)

MD_p1_4 = lm(p1_diff ~ p1_regret_sign + dummy_show_worst + dummy_show_best + dummy_8002 
             + p1_regret_sign_showworst + p1_regret_sign_showbest + p1_regret_sign_8002, data = MD_data)
vcov_p1_4 = cluster.vcov(MD_p1_4, MD_data$round_pair_id)
coeftest(MD_p1_4, vcov_p1_4)

# MC regression for player 2, dependent is directional learning speed, OLS
MD_p2_1 = lm(p2_diff ~ p2_regret_sign, 
             data = MD_data)
vcov_p2_1 = cluster.vcov(MD_p2_1, MD_data$round_pair_id)
coeftest(MD_p2_1, vcov_p2_1)

MD_p2_2 = lm(p2_diff ~ p2_regret_sign + dummy_show_worst + dummy_show_best + dummy_8002, 
             data = MD_data)
vcov_p2_2 = cluster.vcov(MD_p2_2, MD_data$round_pair_id)
coeftest(MD_p2_2, vcov_p2_2)

MD_p2_3 = lm(p2_diff ~ p2_regret_sign + p2_regret_sign_showworst 
             + p2_regret_sign_showbest + p2_regret_sign_8002, data = MD_data)
vcov_p2_3 = cluster.vcov(MD_p2_3, MD_data$round_pair_id)
coeftest(MD_p2_3, vcov_p2_3)

MD_p2_4 = lm(p2_diff ~ p2_regret_sign + dummy_show_worst + dummy_show_best + dummy_8002 
             + p2_regret_sign_showworst + p2_regret_sign_showbest + p2_regret_sign_8002, data = MD_data)
vcov_p2_4 = cluster.vcov(MD_p2_4, MD_data$round_pair_id)
coeftest(MD_p2_4, vcov_p2_4)

# MC regression no intercept term 
MC_p1_1 = lm(p1_diff ~ p1_regret_sign - 1, data = MC_data)
vcov_p1_1 = cluster.vcov(MC_p1_1, MC_data$round_pair_id)
coeftest(MC_p1_1, vcov_p1_1)

MC_p1_2 = lm(p1_diff ~ p1_regret_sign + p1_regret_sign_showworst 
             + p1_regret_sign_showbest + p1_regret_sign_8002 - 1, 
             data = MC_data)
vcov_p1_2 = cluster.vcov(MC_p1_2, MC_data$round_pair_id)
coeftest(MC_p1_2, vcov_p1_2)

MC_p2_1 = lm(p2_diff ~ p2_regret_sign - 1, data = MC_data)
vcov_p2_1 = cluster.vcov(MC_p2_1, MC_data$round_pair_id)
coeftest(MC_p2_1, vcov_p2_1)

MC_p2_2 = lm(p2_diff ~ p2_regret_sign + p2_regret_sign_showworst 
             + p2_regret_sign_showbest + p2_regret_sign_8002 - 1, 
             data = MC_data)
vcov_p2_2 = cluster.vcov(MC_p2_2, MC_data$round_pair_id)
coeftest(MC_p2_2, vcov_p2_2)

# MD regression no intercept term 
MD_p1_1 = lm(p1_diff ~ p1_regret_sign - 1, data = MD_data)
vcov_p1_1 = cluster.vcov(MD_p1_1, MD_data$round_pair_id)
coeftest(MD_p1_1, vcov_p1_1)

MD_p1_2 = lm(p1_diff ~ p1_regret_sign + p1_regret_sign_showworst 
             + p1_regret_sign_showbest + p1_regret_sign_8002 - 1, 
             data = MD_data)
vcov_p1_2 = cluster.vcov(MD_p1_2, MD_data$round_pair_id)
coeftest(MD_p1_2, vcov_p1_2)

MD_p2_1 = lm(p2_diff ~ p2_regret_sign - 1, data = MD_data)
vcov_p2_1 = cluster.vcov(MD_p2_1, MD_data$round_pair_id)
coeftest(MD_p2_1, vcov_p2_1)

MD_p2_2 = lm(p2_diff ~ p2_regret_sign + p2_regret_sign_showworst 
             + p2_regret_sign_showbest + p2_regret_sign_8002 - 1, 
             data = MD_data)
vcov_p2_2 = cluster.vcov(MD_p2_2, MD_data$round_pair_id)
coeftest(MD_p2_2, vcov_p2_2)

##########Analysis 2: Directional and anticipatory learning figures##########
# loop over pairs
for(i in 1:length(uniquePairs)){
  pairData = subset(full_data, pair_id==uniquePairs[i])
  pairRounds = unique(pairData$round_pair_id)
  
  # loop over rounds in which pairs meet
  for(j in 1:length(pairRounds)){
    pairRoundData = subset(pairData, round_pair_id==pairRounds[j])

    title = paste("pair_round", as.character(pairRounds[j]), "game", as.character(pairRoundData$game[1]), 
                  as.character(ifelse(pairRoundData$pure_strategy==TRUE, 'pure strategy', 'mixed strategy')), 
                  as.character(ifelse(pairRoundData$num_subperiods==0, 'continuous time', 'discrete time')),
                  as.character(ifelse(pairRoundData$show_at_worst==TRUE, 'show worst', ' ')),
                  as.character(ifelse(pairRoundData$show_best_response==TRUE, 'show best reponse', ' ')),
                  sep = "_")
    file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/production_aggregate/impulse_balancing/", title, sep = "")
    file = paste(file, ".png", sep = "")
    
    # calculate directional learning and anticipatory learning
    dir_learn_1 = 0
    anti_learn_1 = 0
    dir_learn_2 = 0
    anti_learn_2 = 0
    
    for(m in 1:(length(pairRoundData$tick)-1)){
      if(pairRoundData$p1_diff[m] >0 && pairRoundData$p1_regret_sign[m] >0)
         {dir_learn_1 = dir_learn_1 + 1}
      if(pairRoundData$p1_diff[m] <0 && pairRoundData$p1_regret_sign[m] <0)
         {dir_learn_1 = dir_learn_1 + 1}
      if(pairRoundData$p1_diff[m] ==0 && pairRoundData$p1_regret_sign[m] ==0)
         {dir_learn_1 = dir_learn_1 + 1}
        
      if(pairRoundData$p2_diff[m] >0 && pairRoundData$p2_regret_sign[m] >0)
         {dir_learn_2 = dir_learn_2 + 1}
      if(pairRoundData$p2_diff[m] <0 && pairRoundData$p2_regret_sign[m] <0)
         {dir_learn_2 = dir_learn_2 + 1}
      if(pairRoundData$p2_diff[m] ==0 && pairRoundData$p2_regret_sign[m] ==0)
         {dir_learn_2 = dir_learn_2 + 1} 
         
      if(pairRoundData$p1_diff[m] >0 && pairRoundData$p2_regret_sign[m] >0)
         {anti_learn_1 = anti_learn_1 + 1}
      if(pairRoundData$p1_diff[m] <0 && pairRoundData$p2_regret_sign[m] <0)
         {anti_learn_1 = anti_learn_1 + 1}
       # if(pairRoundData$p1_diff[m] >0 && pairRoundData$p1_regret_sign[m] >0 && pairRoundData$p2_regret_sign[m] ==0)
       #    {anti_learn_1 = anti_learn_1 + 1}
       # if(pairRoundData$p1_diff[m] <0 && pairRoundData$p1_regret_sign[m] <0 && pairRoundData$p2_regret_sign[m] ==0)
       #    {anti_learn_1 = anti_learn_1 + 1}
      
      if(pairRoundData$p2_diff[m] <0 && pairRoundData$p1_regret_sign[m] >0)
         {anti_learn_2 = anti_learn_2 + 1}
      if(pairRoundData$p2_diff[m] >0 && pairRoundData$p1_regret_sign[m] <0)
         {anti_learn_2 = anti_learn_2 + 1}
       # if(pairRoundData$p2_diff[m] >0 && pairRoundData$p2_regret_sign[m] >0 && pairRoundData$p1_regret_sign[m] ==0)
       #    {anti_learn_2 = anti_learn_2 + 1}
       # if(pairRoundData$p2_diff[m] <0 && pairRoundData$p2_regret_sign[m] <0 && pairRoundData$p1_regret_sign[m] ==0)
       #    {anti_learn_2 = anti_learn_2 + 1}
    }
    
    # if(pairRoundData$num_subperiods == 0){
    #   dir_learn_1 = dir_learn_1 / 177
    #   anti_learn_1 = anti_learn_1 / 177
    #   dir_learn_2 = dir_learn_2 / 177
    #   anti_learn_2 = anti_learn_2 / 177
    # }
    # else {
    #   dir_learn_1 = dir_learn_1 / 13
    #   anti_learn_1 = anti_learn_1 / 13
    #   dir_learn_2 = dir_learn_2 / 13
    #   anti_learn_2 = anti_learn_2 / 13
    # }
    
    # plot movement and regret
    png(file, width = 1000, height = 1000)
    par(mfrow=c(2,1))
    
    plot(pairRoundData$tick, pairRoundData$p1_diff, col='red', type='l',
         xlab='time', ylab='Direction of regret and strategy movement', #ylim = c(-1,1),
         main=paste('player 1 strategy (red), own regret (blue) and others regret(black)',
                    'Dir learning', as.character(round(dir_learn_1, digit=2)),
                    'Anti learning', as.character(round(anti_learn_1, digit=2)), sep = ' '))
    lines(pairRoundData$tick, pairRoundData$p1_regret_sign, col='blue', bg='blue', type='p', pch=21)
    lines(pairRoundData$tick, pairRoundData$p2_regret_sign, col='black', bg='black', type='p', pch=22)
    abline(h=0)
    
    plot(pairRoundData$tick, pairRoundData$p2_diff, col='red', type='l',
         xlab='time', ylab='Direction of regret and strategy movement', #ylim = c(-1,1),
         main=paste('player 2 strategy (red), own regret (blue) and others regret(black)',
                    'Dir learning', as.character(round(dir_learn_2, digit=2)),
                    'Anti learning', as.character(round(anti_learn_2, digit=2)), sep = ' '))
    lines(pairRoundData$tick, pairRoundData$p2_regret_sign, col='blue', bg='blue', type='p', pch=21)
    lines(pairRoundData$tick, pairRoundData$p1_regret_sign, col='black', bg='black', type='p', pch=22)
    abline(h=0)
    
    dev.off()
    
  }
}

##########Analysis 3: Strategy asymmetry between player 1 and 2 using player dummy##########
# combine p1 column and p2 column
full_data_p1 = full_data
full_data_p2 = full_data

full_data_p1 = full_data_p1 %>% mutate(player_diff = p1_diff)
full_data_p1 = full_data_p1 %>% mutate(player_regret_sign = p1_regret_sign)
full_data_p1 = full_data_p1 %>% mutate(player_regret_sign_pure = p1_regret_sign_pure)
full_data_p1 = full_data_p1 %>% mutate(player_regret_sign_showworst = p1_regret_sign_showworst)
full_data_p1 = full_data_p1 %>% mutate(player_regret_sign_showbest = p1_regret_sign_showbest)
full_data_p1 = full_data_p1 %>% mutate(player_regret_sign_8002 = p1_regret_sign_8002)
full_data_p1 = full_data_p1 %>% mutate(player2_dummy = 0)
full_data_p1 = full_data_p1 %>% mutate(player_regret_sign_player2dummy = player_regret_sign * player2_dummy)

full_data_p2 = full_data_p2 %>% mutate(player_diff = p2_diff)
full_data_p2 = full_data_p2 %>% mutate(player_regret_sign = p2_regret_sign)
full_data_p2 = full_data_p2 %>% mutate(player_regret_sign_pure = p2_regret_sign_pure)
full_data_p2 = full_data_p2 %>% mutate(player_regret_sign_showworst = p2_regret_sign_showworst)
full_data_p2 = full_data_p2 %>% mutate(player_regret_sign_showbest = p2_regret_sign_showbest)
full_data_p2 = full_data_p2 %>% mutate(player_regret_sign_8002 = p2_regret_sign_8002)
full_data_p2 = full_data_p2 %>% mutate(player2_dummy = 1)
full_data_p2 = full_data_p2 %>% mutate(player_regret_sign_player2dummy = player_regret_sign * player2_dummy)

MC_data_p1 = filter(full_data_p1, MC == 1)
MD_data_p1 = filter(full_data_p1, MD == 1)
MC_data_p2 = filter(full_data_p2, MC == 1)
MD_data_p2 = filter(full_data_p2, MD == 1)

# merge two MC datasets
player_diff = c(MC_data_p1$player_diff, MC_data_p2$player_diff)
player_regret_sign = c(MC_data_p1$player_regret_sign, MC_data_p2$player_regret_sign)
player_regret_sign_pure = c(MC_data_p1$player_regret_sign_pure, MC_data_p2$player_regret_sign_pure)
player_regret_sign_showworst = c(MC_data_p1$player_regret_sign_showworst, MC_data_p2$player_regret_sign_showworst)
player_regret_sign_showbest = c(MC_data_p1$player_regret_sign_showbest, MC_data_p2$player_regret_sign_showbest)
player_regret_sign_8002 = c(MC_data_p1$player_regret_sign_8002, MC_data_p2$player_regret_sign_8002)
player2_dummy = c(MC_data_p1$player2_dummy, MC_data_p2$player2_dummy)
player_regret_sign_player2dummy = c(MC_data_p1$player_regret_sign_player2dummy, MC_data_p2$player_regret_sign_player2dummy)
round_pair_id = c(MC_data_p1$round_pair_id, MC_data_p2$round_pair_id)
tick = c(MC_data_p1$tick, MC_data_p2$tick)

# regression on asymmetry
MC_combine_1 = lm(player_diff ~ player_regret_sign + player2_dummy + player_regret_sign_player2dummy- 1)
vcov_combine_1 = cluster.vcov(MC_combine_1, round_pair_id)
coeftest(MC_combine_1, vcov_combine_1)
MC_combine_1 = plm(player_diff ~ player_regret_sign + player2_dummy + player_regret_sign_player2dummy- 1, effect = 'twoways', model='within')
summary(MC_combine_1)
MC_combine_2 = lm(player_diff ~ player_regret_sign + player2_dummy 
                  + player_regret_sign_showworst + player_regret_sign_showbest 
                  + player_regret_sign_8002 + player_regret_sign_player2dummy - 1)
vcov_combine_2 = cluster.vcov(MC_combine_2, round_pair_id)
coeftest(MC_combine_2, vcov_combine_2)

# merge two MD datasets
player_diff = c(MD_data_p1$player_diff, MD_data_p2$player_diff)
player_regret_sign = c(MD_data_p1$player_regret_sign, MD_data_p2$player_regret_sign)
player_regret_sign_showworst = c(MD_data_p1$player_regret_sign_showworst, MD_data_p2$player_regret_sign_showworst)
player_regret_sign_showbest = c(MD_data_p1$player_regret_sign_showbest, MD_data_p2$player_regret_sign_showbest)
player_regret_sign_8002 = c(MD_data_p1$player_regret_sign_8002, MD_data_p2$player_regret_sign_8002)
player2_dummy = c(MD_data_p1$player2_dummy, MD_data_p2$player2_dummy)
player_regret_sign_player2dummy = c(MD_data_p1$player_regret_sign_player2dummy, MD_data_p2$player_regret_sign_player2dummy)
round_pair_id = c(MD_data_p1$round_pair_id, MD_data_p2$round_pair_id)
tick = c(MD_data_p1$tick, MD_data_p2$tick)

# regression on asymmetry
MD_combine_1 = lm(player_diff ~ player_regret_sign + player2_dummy - 1)
vcov_combine_1 = cluster.vcov(MD_combine_1, round_pair_id)
coeftest(MD_combine_1, vcov_combine_1)

MD_combine_2 = lm(player_diff ~ player_regret_sign + player2_dummy 
                  + player_regret_sign_showworst + player_regret_sign_showbest + player_regret_sign_8002 - 1)
vcov_combine_2 = cluster.vcov(MD_combine_2, round_pair_id)
coeftest(MD_combine_2, vcov_combine_2)

##########Analysis 4: Panel data analysis##########
# create regressions and  panel dataset
regression_p1 = MC_data$p1_diff ~ MC_data$p1_regret_sign - 1
regression_p2 = MC_data$p2_diff ~ MC_data$p2_regret_sign - 1

regression_p1 = MC_data$p1_diff ~ MC_data$p1_regret_sign + MC_data$p1_regret_sign_showworst + MC_data$p1_regret_sign_showbest + MC_data$p1_regret_sign_8002 - 1
regression_p2 = MC_data$p2_diff ~ MC_data$p2_regret_sign + MC_data$p2_regret_sign_showworst + MC_data$p2_regret_sign_showbest + MC_data$p2_regret_sign_8002 - 1
MC_data = pdata.frame(MC_data, index=c('round_pair_id','tick'))
MD_data = pdata.frame(MD_data, index=c('round_pair_id','tick'))

# # stability test
# tlist1 <- xts(MC_data$p1_diff, as.Date(MC_data$tick))
# adf.test(tlist1)
# tlist2 <- xts(MC_data$p1_regret_sign,as.Date(MC_data$tick))
# adf.test(tlist2)
# # granger test
# granger.test(MC_data,p=2)

# regression for player 1
pool_1 = plm(regression_p1, data = MC_data, model='pool')
summary(pool_1)
fe_1 = plm(regression_p1, data = MC_data, effect='twoways', model='within')
summary(fe_1)

# regression test for player 1
pooltest(pool_1, fe_1)
fixef(fe_1, effect = 'time')
fixef(fe_1, effect = 'individual')
plmtest(regression_p1, data = MC_data, effect="twoways")
phtest(regression_p1, data = MC_data)
pbgtest(regression_p1, data = MC_data, model='within')

#variable coefficient regression for player 1
fe_11 = pvcm(regression_p1, data = MC_data, effect='individual', model='within')
summary(fe_11)
fe_12 = pvcm(regression_p1, data = MC_data, effect='time', model='within')
summary(fe_12)

# regression for player 2
pool_2 = plm(regression_p2, data = MC_data, model='pool')
summary(pool_2)
fe_2 = plm(regression_p2, data = MC_data, effect='twoways', model='within')
summary(fe_2)

# regression test for player 2
pooltest(pool_2, fe_2)
fixef(fe_2, effect = 'time')
fixef(fe_2, effect = 'individual')
plmtest(regression_p2, data = MC_data, effect="twoways")
phtest(regression_p2, data = MC_data)
pbgtest(regression_p2, data = MC_data, model='within')

#variable coefficient regression for player 2
fe_21 = pvcm(regression_p2, data = MC_data, effect='individual', model='within')
summary(fe_21)
fe_22 = pvcm(regression_p2, data = MC_data, effect='time', model='within')
summary(fe_22)

##########Analysis 5: Anticipatory learning process##########
regression_p1 = MC_data$p1_diff ~ MC_data$p1_regret_sign + MC_data$p1_regret_sign_showworst + MC_data$p1_regret_sign_showbest + MC_data$p1_regret_sign_8002 + MC_data$p2_regret_sign - 1
regression_p2 = MC_data$p2_diff ~ MC_data$p2_regret_sign + MC_data$p2_regret_sign_showworst + MC_data$p2_regret_sign_showbest + MC_data$p2_regret_sign_8002 + MC_data$p1_regret_sign - 1

regression_p1 = MC_data$p1_diff ~ MC_data$p1_regret_sign + MC_data$p1_regret_sign_showworst + MC_data$p1_regret_sign_showbest + MC_data$p1_regret_sign_8002 + MC_data$p2_regret_sign + MC_data$p2_regret_sign_showworst + MC_data$p2_regret_sign_showbest + MC_data$p2_regret_sign_8002 - 1
regression_p2 = MC_data$p2_diff ~ MC_data$p2_regret_sign + MC_data$p2_regret_sign_showworst + MC_data$p2_regret_sign_showbest + MC_data$p2_regret_sign_8002 + MC_data$p1_regret_sign + MC_data$p1_regret_sign_showworst + MC_data$p1_regret_sign_showbest + MC_data$p1_regret_sign_8002 - 1
MC_data = pdata.frame(MC_data, index=c('round_pair_id','tick'))

# repeat previous penal data section with new regression and dataset
# regression for player 1
pool_1 = plm(regression_p1, data = MC_data, model='pool')
summary(pool_1)
fe_1 = plm(regression_p1, data = MC_data, effect='twoways', model='within')
summary(fe_1)

# regression test for player 1
pooltest(pool_1, fe_1)
fixef(fe_1, effect = 'time')
fixef(fe_1, effect = 'individual')
plmtest(regression_p1, data = MC_data, effect="twoways")
phtest(regression_p1, data = MC_data)
pbgtest(regression_p1, data = MC_data, model='within')

#variable coefficient regression for player 1
fe_11 = pvcm(regression_p1, data = MC_data, effect='individual', model='within')
summary(fe_11)
fe_12 = pvcm(regression_p1, data = MC_data, effect='time', model='within')
summary(fe_12)

# regression for player 2
pool_2 = plm(regression_p2, data = MC_data, model='pool')
summary(pool_2)
fe_2 = plm(regression_p2, data = MC_data, effect='twoways', model='within')
summary(fe_2)

# regression test for player 2
pooltest(pool_2, fe_2)
fixef(fe_2, effect = 'time')
fixef(fe_2, effect = 'individual')
plmtest(regression_p2, data = MC_data, effect="twoways")
phtest(regression_p2, data = MC_data)
pbgtest(regression_p2, data = MC_data, model='within')

#variable coefficient regression for player 2
fe_21 = pvcm(regression_p2, data = MC_data, effect='individual', model='within')
summary(fe_21)
fe_22 = pvcm(regression_p2, data = MC_data, effect='time', model='within')
summary(fe_22)