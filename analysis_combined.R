##########Data preparation##########
# load packages
library(ggplot2)
library(dplyr)
library(lattice)
library(latticeExtra)
library(csv)
library(xtable)
library(scatterplot3d)
library(rgl)
library(plot3D)
library(foreign)

# import rp and mm data
merge_mm = read.csv("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/merge_mm.csv", header = T)
merge_rp = read.csv("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/merge_rp.csv", header = T)

# merge two datasets
full_data = rbind(merge_mm, merge_rp)
rm(merge_mm, merge_rp)

# add new group id (rp by pairs, mm by periods)
full_data = full_data %>% mutate(group_id = ifelse(mean_matching=='TRUE', session_round_id, session_round_pair_id))

# create unique id
uniqueID = unique(full_data$group_id)
gametype = unique(full_data$game)
treatmenttype = unique(full_data$treatmentfull)

# export dataset to stata
write.dta(full_data, "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_production.dta")


##########Descriptive figure: QRE precision function heatmap##########
# read data from gambit
qre_data = read.csv("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/gambit/qre_8002.csv", header = T)
qre_precision = subset(qre_data, lambda<1)

## draw line charts showing precision functions wrt lambda
title = 'AMPa game QRE precision'
file = paste("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/gambit/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file)

plot(qre_precision$lambda, qre_precision$p1_row, col = "blue", ylim = c(0:1),
     type='l', xlab = "lambda", ylab = "equilibrium", main = 'QRE precision function')
lines(qre_precision$lambda, qre_precision$p2_row, col="red")
legend('topright', legend = c('row', 'column'), fill = c('blue', 'red'))

dev.off()

## draw parametrized curve on the action space
title = 'AMPa game QRE parametrized curve'
file = paste("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/gambit/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file)

pic = ggplot() +
  
  geom_line(data = qre_precision, mapping = aes(x = p2_row, y = p1_row)) +
  geom_text(aes(x = 0.2, y = 0.5,
                label = 'NE'), vjust = 0, hjust = 0) +
  geom_text(aes(x = 0.5, y = 0.2,
                label = 'MM'), vjust = 0, hjust = 0) +
  geom_text(aes(x = 0.5, y = 0.5,
                label = 'Center'), vjust = 0, hjust = 0) +
  ggtitle(title) +
  scale_x_continuous(name='column strategy', limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_y_continuous(name='row strategy', limits = c(0,1), breaks = seq(0,1,0.2)) +
  theme_bw() 

print(pic)

dev.off()

## draw parametrized curve on the heatmap
# Generate row player's payoff matrix in levelplot, change with payoff matrix in experiment
x = seq(from = 0, to = 1, length.out = 100)
y = seq(from = 0, to = 1, length.out = 100)
z = matrix(0, nrow=100, ncol=100)
for (i in 1:100){
  for (j in 1:100){
    z[i,j] =800*x[i]*y[j] + 0*x[i]*(1-y[j]) + 0*(1-x[i])*y[j] + 200*(1-x[i])*(1-y[j])
  }
}

# Set colors in levelplot: rainbow or heatcolor
n = 10
#colours = rev(rainbow(n, s = 1, v = 1, start = 0, end = 0.9, alpha = 1))
colours = rev(heat.colors(n, alpha = 1))
rgb.palette <- colorRampPalette(colours, space = "Lab")

# Change ticks in levelplot
x.scale <- list(at=seq(from = 0, to = 100, length.out = 6))
y.scale <- list(at=seq(from = 0, to = 100, length.out = 6))

# draw the graph
heat = levelplot(z, col.regions=rgb.palette(1000), 
                 scales=list(x=x.scale, y=y.scale),
                 main="Predictions on row player payoff heatmap",
                 xlab="column action", 
                 ylab="row action" ,
                 cuts = 1000)

NE <- layer(panel.points(y=100*0.5, x= 100*0.2, col = "black", cex=1.5))
NEText <- layer(panel.text(y=100*0.5, x= 100*0.2, labels= "NE", pos=4, cex=1.5))

MM <- layer(panel.points(y=100*0.2, x= 100*0.5, col = "black", cex=1.5))
MMText <- layer(panel.text(y=100*0.2, x= 100*0.5, labels= "MM", pos=4, cex=1.5))

Mid <- layer(panel.points(y=100*0.5, x= 100*0.5, col = "black", cex=1.5))
MidText <- layer(panel.text(y=100*0.5, x= 100*0.5, labels= "Center", pos=4, cex=1.5))

curve <- layer(panel.xyplot(100*qre_precision$p2_row, 100*qre_precision$p1_row, type = 'l', col = 'black'))

fullplot = heat + NE + NEText + MM + MMText + Mid + MidText + curve

title = 'AMPaheatmap+ptpreds_redscale'
file = paste("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/gambit/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file)
print(fullplot)
dev.off()


##########Figure: time average scatter plot by games##########
# create empty dataset
length = rep(NA, length(treatmenttype))
mean_data = data.frame(p1_average = length, p2_average = length, p1_median = length, p2_median = length, 
                       p1_q1 = length, p2_q1 = length, p1_q3 = length, p2_q3 = length, 
                       sd_p1 = length, sd_p2 = length, p1_payoff = length, p2_payoff = length,
                       p1NEmix = length, p2NEmix = length, p1MMmix = length, p2MMmix = length,
                       time = length, actionsets = length, match = length, game = length, treatments = length)

# loop over treatments
for (i in 1:length(treatmenttype)){
  round_data = filter(full_data, treatmentfull == treatmenttype[i])
  
  # fill in the mean_data row i
  mean_data$p1_average[i] = mean(round_data$p1_strategy)
  mean_data$p2_average[i] = mean(round_data$p2_strategy)
  mean_data$p1_median[i] = median(round_data$p1_strategy)
  mean_data$p2_median[i] = median(round_data$p2_strategy)
  mean_data$p1_q1[i] = quantile(round_data$p1_strategy, 0.25)
  mean_data$p1_q3[i] = quantile(round_data$p1_strategy, 0.75)
  mean_data$p2_q1[i] = quantile(round_data$p2_strategy, 0.25)
  mean_data$p2_q3[i] = quantile(round_data$p2_strategy, 0.75)
  mean_data$sd_p1[i] = sd(round_data$p1_strategy)
  mean_data$sd_p2[i] = sd(round_data$p2_strategy)
  
  mean_data$p1NEmix[i] = round_data$p1NEmix[1]
  mean_data$p2NEmix[i] = round_data$p2NEmix[1]
  mean_data$p1MMmix[i] = round_data$p1MMmix[1]
  mean_data$p2MMmix[i] = round_data$p2MMmix[1]
  mean_data$p1_payoff[i] = mean(round_data$p1_payoff)
  mean_data$p2_payoff[i] = mean(round_data$p2_payoff)
  
  mean_data$time[i] = as.character(round_data$time[1])
  mean_data$actionsets[i] = as.character(round_data$actionsets[1])
  mean_data$match[i] = as.character(round_data$match[1])
  mean_data$game[i] = as.character(round_data$game[1])
  mean_data$treatments[i] = as.character(round_data$treatmentfull[1])
}

mean_data = arrange(mean_data, treatments)

# Generate scatter plot
# loop over games
for (i in 1:length(gametype)){
  game_data = subset(mean_data, game == gametype[i])
  
  # AMPb game
  if (gametype[i] == 'AMPb'){
    # read qre info
    qre_data = read.csv("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/gambit/qre_3117.csv", header = T)
    qre_precision = subset(qre_data, lambda<2)
    qre_precision = arrange(qre_precision, lambda)
    
    # add RDCE data
    rec1 = data.frame(pmin=0, pmax=0.33, qmin=0.5, qmax=0.75)
    rec2 = data.frame(pmin=0.33, pmax=0.5, qmin=0, qmax=0.5)
    
    # set title
    title = paste('scatter_plot', as.character(game_data$game[1]), sep = '_')
    file = paste("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/writeup/figs/", title, sep = "")
    file = paste(file, ".png", sep = "")
    png(file, width = 600, height = 450)
    
    # scatter plot
    pic = ggplot() +
      geom_rect(mapping = aes(xmin = 0.5, ymin = 0, xmax = 0.75, ymax = 0.33), 
                color = 'azure2', fill = 'azure2', alpha = 0.5, size = 1.5) +
      geom_rect(mapping = aes(xmin = 0, ymin = 0.33, xmax = 0.5, ymax = 0.5), 
                color = 'azure2', fill = 'azure2', alpha = 0.5, size = 1.5) +
      geom_point(data = qre_precision, aes(x = p2_row, y = p1_row)) +
      geom_point(data = game_data, aes(x = p2_average, y = p1_average, color = treatments), size=3) +
      geom_text(aes(x = game_data$p2NEmix[1], y = game_data$p1NEmix[1],
                    label = 'NE'), vjust = -0.2, hjust = -0.2) +
      geom_text(aes(x = game_data$p2MMmix[1], y = game_data$p1MMmix[1],
                    label = 'MM'), vjust = -0.2, hjust = -0.2) +
      geom_text(aes(x = 0.5, y = 0.5,
                    label = 'Center'), vjust = -0.1, hjust = -0.1) +
      geom_point(data = game_data, aes(x = p2NEmix[1], y = p1NEmix[1]), size=3) +
      geom_point(data = game_data, aes(x = p2MMmix[1], y = p1MMmix[1]), size=3) +
      geom_point(data = game_data, aes(x = 0.5, y = 0.5), size=3) +
      
      ggtitle(title) +
      scale_x_continuous(name='column strategy', limits = c(0,1), breaks = seq(0,1,0.1)) +
      scale_y_continuous(name='row strategy', limits = c(0,1), breaks = seq(0,1,0.1)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 25),
            axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
            legend.text = element_text(size = 15))
    
    print(pic)
    dev.off()
  }
  
  # AMPa game
  else if (gametype[i] == 'AMPa'){
    # read qre info
    qre_data = read.csv("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/gambit/qre_8002.csv", header = T)
    qre_precision = subset(qre_data, lambda<2)
    qre_precision = arrange(qre_precision, lambda)
    
    # add RDCE data
    rec1 = data.frame(pmin=0.5, pmax=1, qmin=0.2, qmax=0.5)
    
    # set title
    title = paste('scatter_plot', as.character(game_data$game[1]), sep = '_')
    file = paste("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/writeup/figs/", title, sep = "")
    file = paste(file, ".png", sep = "")
    png(file, width = 600, height = 450)
    
    # scatter plot
    pic = ggplot() +
      geom_rect(mapping = aes(xmin = 0.2, ymin = 0.5, xmax = 0.5, ymax = 1), 
                color = 'azure2', fill = 'azure2', alpha = 0.5, size = 1.5) +
      geom_point(data = qre_precision, aes(x = p2_row, y = p1_row)) +
      geom_point(data = game_data, aes(x = p2_average, y = p1_average, color = treatments), size=3) +
      geom_text(aes(x = game_data$p2NEmix[1], y = game_data$p1NEmix[1],
                    label = 'NE'), vjust = -0.2, hjust = -0.2) +
      geom_text(aes(x = game_data$p2MMmix[1], y = game_data$p1MMmix[1],
                    label = 'MM'), vjust = -0.2, hjust = -0.2) +
      geom_text(aes(x = 0.5, y = 0.5,
                    label = 'Center'), vjust = -0.1, hjust = -0.1) +
      geom_point(data = game_data, aes(x = p2NEmix[1], y = p1NEmix[1]), size=3) +
      geom_point(data = game_data, aes(x = p2MMmix[1], y = p1MMmix[1]), size=3) +
      geom_point(data = game_data, aes(x = 0.5, y = 0.5), size=3) +
      ggtitle(title) +
      scale_x_continuous(name='column strategy', limits = c(0,1), breaks = seq(0,1,0.1)) +
      scale_y_continuous(name='row strategy', limits = c(0,1), breaks = seq(0,1,0.1)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 25),
            axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
            legend.text = element_text(size = 15))
    
    print(pic)
    dev.off()
  }
  
  # IDDS game
  else{
    # set title
    title = paste('scatter_plot', as.character(game_data$game[1]), sep = '_')
    file = paste("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/writeup/figs/", title, sep = "")
    file = paste(file, ".png", sep = "")
    png(file, width = 600, height = 450)
    
    # add RDCE data
    rec1 = data.frame(pmin=0, pmax=0.5, qmin=0.5, qmax=1)
    
    # scatter plot
    pic = ggplot() +
      geom_rect(mapping = aes(xmin = 0.5, ymin = 0, xmax = 1, ymax = 0.5), 
                color = 'azure2', fill = 'azure2', alpha = 0.5, size = 1.5) +
      geom_point(aes(x = seq(from=0.5, to=game_data$p2NEmix[1], by=0.01), 
                     y = seq(from=0.5, to=game_data$p1NEmix[1], by=-0.01))) +
      geom_point(data = game_data, aes(x = p2_average, y = p1_average, color = treatments), size=3) +
      geom_text(aes(x = game_data$p2NEmix[1], y = game_data$p1NEmix[1],
                    label = 'NE'), vjust = -0.2, hjust = -0.2) +
      geom_text(aes(x = 0.5, y = 0.5,
                    label = 'Center'), vjust = -0.1, hjust = -0.1) +
      geom_point(data = game_data, aes(x = p2NEmix[1], y = p1NEmix[1]), size=3) +
      geom_point(data = game_data, aes(x = 0.5, y = 0.5), size=3) +
      ggtitle(title) +
      scale_x_continuous(name='column strategy', limits = c(0,1), breaks = seq(0,1,0.1)) +
      scale_y_continuous(name='row strategy', limits = c(0,1), breaks = seq(0,1,0.1)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 25),
            axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
            legend.text = element_text(size = 15))
    
    print(pic)
    dev.off()
  }
}

# remove temp data
rm(game_data, mean_data, pic, qre_data, qre_precision, rec1, rec2, round_data)


##########Table (TBC): Mean data of time average by treatments##########
## build summary data
last_data = full_data
# keep the last 30 seconds
# rp_d = full_data %>% filter(mean_matching==FALSE & num_subperiods!=0) %>% 
#        mutate(last30 = ifelse(tick >= 10, 1, 0))
# rp_c = full_data %>% filter(mean_matching==FALSE & num_subperiods==0) %>% 
#        mutate(last30 = ifelse(tick >= 120, 1, 0))
# mm_d = full_data %>% filter(mean_matching==TRUE & num_subperiods!=0) %>% 
#        mutate(last30 = ifelse(tick >= 20, 1, 0))
# mm_c = full_data %>% filter(mean_matching==TRUE & num_subperiods==0) %>% 
#        mutate(last30 = ifelse(tick >= 240, 1, 0))
# last_data = rbind(rp_d, rp_c, mm_d, mm_c)
# last_data = filter(last_data, last30==1)
# rm(rp_d, rp_c, mm_d, mm_c)

# create empty dataset
length = rep(NA, length(uniqueID))
mean_data = data.frame(session_id = length, group_id = length, treatments = length,
                       p1_average = length, p2_average = length, p1_median = length, p2_median = length, 
                       p1_q1 = length, p2_q1 = length, p1_q3 = length, p2_q3 = length, 
                       sd_p1 = length, sd_p2 = length, p1_payoff = length, p2_payoff = length,
                       p1NEmix = length, p2NEmix = length, p1MMmix = length, p2MMmix = length,
                       time = length, actionsets = length, match = length, game = length)

# loop over group_id
for (i in 1:length(uniqueID)){
  round_data = subset(last_data, group_id == uniqueID[i])
  
  # fill in the mean_data row i
  mean_data$session_id[i] = as.character(round_data$session_code[1])
  mean_data$group_id[i] = as.character(round_data$group_id[1])
  
  mean_data$p1_average[i] = mean(round_data$p1_strategy)
  mean_data$p2_average[i] = mean(round_data$p2_strategy)
  mean_data$p1_median[i] = median(round_data$p1_strategy)
  mean_data$p2_median[i] = median(round_data$p2_strategy)
  mean_data$p1_q1[i] = quantile(round_data$p1_strategy, 0.25)
  mean_data$p1_q3[i] = quantile(round_data$p1_strategy, 0.75)
  mean_data$p2_q1[i] = quantile(round_data$p2_strategy, 0.25)
  mean_data$p2_q3[i] = quantile(round_data$p2_strategy, 0.75)
  mean_data$sd_p1[i] = sd(round_data$p1_strategy)
  mean_data$sd_p2[i] = sd(round_data$p2_strategy)
  
  mean_data$p1NEmix[i] = round_data$p1NEmix[1]
  mean_data$p2NEmix[i] = round_data$p2NEmix[1]
  mean_data$p1MMmix[i] = round_data$p1MMmix[1]
  mean_data$p2MMmix[i] = round_data$p2MMmix[1]
  mean_data$p1_payoff[i] = mean(round_data$p1_payoff)
  mean_data$p2_payoff[i] = mean(round_data$p2_payoff)
  
  mean_data$time[i] = as.character(round_data$time[1])
  mean_data$actionsets[i] = as.character(round_data$actionsets[1])
  mean_data$match[i] = as.character(round_data$match[1])
  mean_data$game[i] = as.character(round_data$game[1])
  mean_data$treatments[i] = as.character(round_data$treatmentfull[1])
}

mean_data = arrange(mean_data, group_id)

# add necessary variables
mean_data = mean_data %>% mutate(Deviation_NE = sqrt((p1_average - p1NEmix)^2 + (p2_average - p2NEmix)^2))
mean_data = mean_data %>% mutate(Deviation_MM = sqrt((p1_average - p1MMmix)^2 + (p2_average - p2MMmix)^2))
mean_data = mean_data %>% mutate(Deviation_Mid = sqrt((p1_average - 0.5)^2 + (p2_average - 0.5)^2))
mean_data = mean_data %>% mutate(IQR_p1 = p1_q3 - p1_q1)
mean_data = mean_data %>% mutate(IQR_p2 = p2_q3 - p2_q1)
mean_data = mean_data %>% mutate(IQR_mean = (IQR_p1 + IQR_p2) / 2)
mean_data = mean_data %>% mutate(IQR_square = sqrt(IQR_p1^2 + IQR_p2^2))
mean_data = mean_data %>% mutate(IQR_harmonic = (IQR_p1 * IQR_p2) / (IQR_p1 + IQR_p2))
mean_data = mean_data %>% mutate(IQR_geometric = sqrt(IQR_p1 * IQR_p2))
mean_data = mean_data %>% mutate(sd_mean = (sd_p1 + sd_p2) / 2)
mean_data = mean_data %>% mutate(sd_square = sqrt(sd_p1^2 + sd_p2^2))
mean_data = mean_data %>% mutate(sd_harmonic = (sd_p1 * sd_p2) / (sd_p1 + sd_p2))
mean_data = mean_data %>% mutate(sd_geometric = sqrt(sd_p1 * sd_p2))

# # read qre data from gambit
# qre_AMPa = read.csv("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/gambit/qre_8002.csv", header = T)
# qre_AMPb = read.csv("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/gambit/qre_3117.csv", header = T)
# 
# # add qre distance and optimal lambda
# mean_data = mean_data %>% mutate(lambda = 0)
# mean_data = mean_data %>% mutate(Deviation_QRE = 0)
# 
# for (i in 1:length(mean_data$session_round_id)){
#   
#   # select qre dataset based on game type
#   if (mean_data$game[i] == 'AMPb'){qre_data = qre_AMPb}
#   else{qre_data = qre_AMPa}
#   
#   # find the optimal lambda
#   distance_min = 100000
#   for (k in 1:length(qre_data$lambda)){
#     p1 = qre_data$p1_row[k]
#     p2 = qre_data$p2_row[k]
#     distance = sqrt((mean_data$p1_average[i] - p1)^2 + (mean_data$p2_average[i] - p2)^2)
#     if (distance < distance_min){
#       distance_min = distance
#       lambda_min = qre_data$lambda[k]
#     }
#   }
#   
#   mean_data$lambda[i] = lambda_min
#   mean_data$Deviation_QRE[i] = distance_min
# }

# export dataset to stata
write.dta(mean_data, "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_summary.dta")


## Generate Table 3
# load package
library(MASS)

# create list
median_table = list()

# loop over game
for (i in 1:length(gametype)){
  game_data = subset(mean_data, game == gametype[i])
  
  median_table[[i]] = matrix(0, nrow = 6, ncol = 6)
  rownames(median_table[[i]]) = c('mm', 'rp', 'Mixed', 'Pure', 'Continuous', 'Discrete')
  colnames(median_table[[i]]) = c('Mean Distance to NE', ' ','Mean Distance to Center', 
                                  ' ', 'Mean Distance to MM', 'Num of Groups')
  
  # mm vs rp
  data1 = subset(game_data, match == 'mm')
  median_table[[i]][1,1] = mean(data1$Deviation_NE)
  test = t.test(data1$Deviation_NE, data1$Deviation_Mid, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][1,2] = round(test$p.value, digits = 3)
  median_table[[i]][1,3] = mean(data1$Deviation_Mid)
  test = t.test(data1$Deviation_Mid, data1$Deviation_MM, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][1,4] = round(test$p.value, digits = 3)
  median_table[[i]][1,5] = mean(data1$Deviation_MM)
  median_table[[i]][1,6] = length(data1$group_id)
  
  data2 = subset(game_data, match == 'rp')
  median_table[[i]][2,1] = mean(data2$Deviation_NE)
  test = t.test(data2$Deviation_NE, data2$Deviation_Mid, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][2,2] = round(test$p.value, digits = 3)
  median_table[[i]][2,3] = mean(data2$Deviation_Mid)
  test = t.test(data2$Deviation_Mid, data2$Deviation_MM, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][2,4] = round(test$p.value, digits = 3)
  median_table[[i]][2,5] = mean(data2$Deviation_MM)
  median_table[[i]][2,6] = length(data2$group_id)
  
  # mixed vs pure
  data1 = subset(game_data, actionsets == 'M')
  median_table[[i]][3,1] = mean(data1$Deviation_NE)
  test = t.test(data1$Deviation_NE, data1$Deviation_Mid, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][3,2] = round(test$p.value, digits = 3)
  median_table[[i]][3,3] = mean(data1$Deviation_Mid)
  test = t.test(data1$Deviation_Mid, data1$Deviation_MM, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][3,4] = round(test$p.value, digits = 3)
  median_table[[i]][3,5] = mean(data1$Deviation_MM)
  median_table[[i]][3,6] = length(data1$group_id)
  
  data2 = subset(game_data, actionsets == 'P')
  median_table[[i]][4,1] = mean(data2$Deviation_NE)
  test = t.test(data2$Deviation_NE, data2$Deviation_Mid, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][4,2] = round(test$p.value, digits = 3)
  median_table[[i]][4,3] = mean(data2$Deviation_Mid)
  test = t.test(data2$Deviation_Mid, data2$Deviation_MM, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][4,4] = round(test$p.value, digits = 3)
  median_table[[i]][4,5] = mean(data2$Deviation_MM)
  median_table[[i]][4,6] = length(data2$group_id)
  
  # continuous vs discrete
  data1 = subset(game_data, time == 'C')
  median_table[[i]][5,1] = mean(data1$Deviation_NE)
  test = t.test(data1$Deviation_NE, data1$Deviation_Mid, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][5,2] = round(test$p.value, digits = 3)
  median_table[[i]][5,3] = mean(data1$Deviation_Mid)
  test = t.test(data1$Deviation_Mid, data1$Deviation_MM, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][5,4] = round(test$p.value, digits = 3)
  median_table[[i]][5,5] = mean(data1$Deviation_MM)
  median_table[[i]][5,6] = length(data1$group_id)
  
  data2 = subset(game_data, time == 'D')
  median_table[[i]][6,1] = mean(data2$Deviation_NE)
  test = t.test(data2$Deviation_NE, data2$Deviation_Mid, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][6,2] = round(test$p.value, digits = 3)
  median_table[[i]][6,3] = mean(data2$Deviation_Mid)
  test = t.test(data2$Deviation_Mid, data2$Deviation_MM, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][6,4] = round(test$p.value, digits = 3)
  median_table[[i]][6,5] = mean(data2$Deviation_MM)
  median_table[[i]][6,6] = length(data2$group_id)
}

xtable(head(median_table[[1]]),digits=3,caption="Distance to predictions.")
xtable(head(median_table[[2]]),digits=3,caption="Distance to predictions.")
xtable(head(median_table[[3]]),digits=3,caption="Distance to predictions.")

rm(game_data, last_data, round_data)
rm(data1, data2, median_table, test)


## Generate Table 4 (full version of Table 3)
# create list
median_table = list()

# loop over game
for (i in 1:length(gametype)){
  game_data = subset(mean_data, game == gametype[i])
  
  median_table[[i]] = matrix(0, nrow = 9, ncol = 9)
  rownames(median_table[[i]]) = c('mm', 'rp', 'p-value', 
                                  'Mixed', 'Pure', 'p-value',
                                  'Continuous', 'Discrete', 'p-value')
  colnames(median_table[[i]]) = c('row average', 'column average',
                                  'To NE', 'p-value','To Center', 'p-value', 'To MM', 
                                  'IQR Harmonic', 'IQR Geometric')
  
  # mm vs rp
  data1 = subset(game_data, match == 'mm')
  median_table[[i]][1,1] = mean(data1$p1_average)
  median_table[[i]][1,2] = mean(data1$p2_average)
  median_table[[i]][1,3] = mean(data1$Deviation_NE)
  test = t.test(data1$Deviation_NE, data1$Deviation_Mid, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][1,4] = round(test$p.value, digits = 3)
  median_table[[i]][1,5] = mean(data1$Deviation_Mid)
  test = t.test(data1$Deviation_Mid, data1$Deviation_MM, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][1,6] = round(test$p.value, digits = 3)
  median_table[[i]][1,7] = mean(data1$Deviation_MM)
  median_table[[i]][1,8] = mean(data1$sd_harmonic, na.rm=TRUE)
  median_table[[i]][1,9] = mean(data1$sd_geometric)
  
  data2 = subset(game_data, match == 'rp')
  median_table[[i]][2,1] = mean(data2$p1_average)
  median_table[[i]][2,2] = mean(data2$p2_average)
  median_table[[i]][2,3] = mean(data2$Deviation_NE)
  test = t.test(data2$Deviation_NE, data2$Deviation_Mid, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][2,4] = round(test$p.value, digits = 3)
  median_table[[i]][2,5] = mean(data2$Deviation_Mid)
  test = t.test(data2$Deviation_Mid, data2$Deviation_MM, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][2,6] = round(test$p.value, digits = 3)
  median_table[[i]][2,7] = mean(data2$Deviation_MM)
  median_table[[i]][2,8] = mean(data2$sd_harmonic, na.rm=TRUE)
  median_table[[i]][2,9] = mean(data2$sd_geometric)
  
  test = t.test(data1$p1_average, data2$p1_average, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][3,1] = round(test$p.value, digits = 3)
  test = t.test(data1$p2_average, data2$p2_average, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][3,2] = round(test$p.value, digits = 3)
  test = t.test(data1$Deviation_NE, data2$Deviation_NE, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][3,3] = round(test$p.value, digits = 3)
  test = t.test(data1$Deviation_Mid, data2$Deviation_Mid, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][3,5] = round(test$p.value, digits = 3)
  test = t.test(data1$Deviation_MM, data2$Deviation_MM, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][3,7] = round(test$p.value, digits = 3)
  test = t.test(data1$sd_harmonic, data2$sd_harmonic, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][3,8] = round(test$p.value, digits = 3)
  test = t.test(data1$sd_geometric, data2$sd_geometric, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][3,9] = round(test$p.value, digits = 3)
  
  
  # mixed vs pure
  data1 = subset(game_data, actionsets == 'M')
  median_table[[i]][4,1] = mean(data1$p1_average)
  median_table[[i]][4,2] = mean(data1$p2_average)
  median_table[[i]][4,3] = mean(data1$Deviation_NE)
  test = t.test(data1$Deviation_NE, data1$Deviation_Mid, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][4,4] = round(test$p.value, digits = 3)
  median_table[[i]][4,5] = mean(data1$Deviation_Mid)
  test = t.test(data1$Deviation_Mid, data1$Deviation_MM, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][4,6] = round(test$p.value, digits = 3)
  median_table[[i]][4,7] = mean(data1$Deviation_MM)
  median_table[[i]][4,8] = mean(data1$sd_harmonic, na.rm=TRUE)
  median_table[[i]][4,9] = mean(data1$sd_geometric)
  
  data2 = subset(game_data, actionsets == 'P')
  median_table[[i]][5,1] = mean(data2$p1_average)
  median_table[[i]][5,2] = mean(data2$p2_average)
  median_table[[i]][5,3] = mean(data2$Deviation_NE)
  test = t.test(data2$Deviation_NE, data2$Deviation_Mid, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][5,4] = round(test$p.value, digits = 3)
  median_table[[i]][5,5] = mean(data2$Deviation_Mid)
  test = t.test(data2$Deviation_Mid, data2$Deviation_MM, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][5,6] = round(test$p.value, digits = 3)
  median_table[[i]][5,7] = mean(data2$Deviation_MM)
  median_table[[i]][5,8] = mean(data2$sd_harmonic, na.rm=TRUE)
  median_table[[i]][5,9] = mean(data2$sd_geometric)
  
  test = t.test(data1$p1_average, data2$p1_average, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][6,1] = round(test$p.value, digits = 3)
  test = t.test(data1$p2_average, data2$p2_average, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][6,2] = round(test$p.value, digits = 3)
  test = t.test(data1$Deviation_NE, data2$Deviation_NE, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][6,3] = round(test$p.value, digits = 3)
  test = t.test(data1$Deviation_Mid, data2$Deviation_Mid, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][6,5] = round(test$p.value, digits = 3)
  test = t.test(data1$Deviation_MM, data2$Deviation_MM, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][6,7] = round(test$p.value, digits = 3)
  test = t.test(data1$sd_harmonic, data2$sd_harmonic, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][6,8] = round(test$p.value, digits = 3)
  test = t.test(data1$sd_geometric, data2$sd_geometric, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][6,9] = round(test$p.value, digits = 3)
  
  
  # continuous vs discrete
  data1 = subset(game_data, time == 'C')
  median_table[[i]][7,1] = mean(data1$p1_average)
  median_table[[i]][7,2] = mean(data1$p2_average)
  median_table[[i]][7,3] = mean(data1$Deviation_NE)
  test = t.test(data1$Deviation_NE, data1$Deviation_Mid, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][7,4] = round(test$p.value, digits = 3)
  median_table[[i]][7,5] = mean(data1$Deviation_Mid)
  test = t.test(data1$Deviation_Mid, data1$Deviation_MM, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][7,6] = round(test$p.value, digits = 3)
  median_table[[i]][7,7] = mean(data1$Deviation_MM)
  median_table[[i]][7,8] = mean(data1$sd_harmonic, na.rm=TRUE)
  median_table[[i]][7,9] = mean(data1$sd_geometric)
  
  data2 = subset(game_data, time == 'D')
  median_table[[i]][8,1] = mean(data2$p1_average)
  median_table[[i]][8,2] = mean(data2$p2_average)
  median_table[[i]][8,3] = mean(data2$Deviation_NE)
  test = t.test(data2$Deviation_NE, data2$Deviation_Mid, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][8,4] = round(test$p.value, digits = 3)
  median_table[[i]][8,5] = mean(data2$Deviation_Mid)
  test = t.test(data2$Deviation_Mid, data2$Deviation_MM, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][8,6] = round(test$p.value, digits = 3)
  median_table[[i]][8,7] = mean(data2$Deviation_MM)
  median_table[[i]][8,8] = mean(data2$sd_harmonic, na.rm=TRUE)
  median_table[[i]][8,9] = mean(data2$sd_geometric)
  
  test = t.test(data1$p1_average, data2$p1_average, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][9,1] = round(test$p.value, digits = 3)
  test = t.test(data1$p2_average, data2$p2_average, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][9,2] = round(test$p.value, digits = 3)
  test = t.test(data1$Deviation_NE, data2$Deviation_NE, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][9,3] = round(test$p.value, digits = 3)
  test = t.test(data1$Deviation_Mid, data2$Deviation_Mid, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][9,5] = round(test$p.value, digits = 3)
  test = t.test(data1$Deviation_MM, data2$Deviation_MM, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][9,7] = round(test$p.value, digits = 3)
  test = t.test(data1$sd_harmonic, data2$sd_harmonic, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][9,8] = round(test$p.value, digits = 3)
  test = t.test(data1$sd_geometric, data2$sd_geometric, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  median_table[[i]][9,9] = round(test$p.value, digits = 3)
}

xtable(median_table[[1]],digits=3,caption="Distance to predictions.")
xtable(median_table[[2]],digits=3,caption="Distance to predictions.")
xtable(median_table[[3]],digits=3,caption="Distance to predictions.")

rm(data1, data2, median_table, test, mean_data, game_data)


##########Table (TBC): Mean data of time average and compare NE MM Center with loglikelihood##########
# create empty dataset
length = rep(NA, length(uniqueID))
mean_data = data.frame(session_id = length, session_round_id = length, 
                       p1_average = length, p2_average = length,
                       num_subperiods = length, pure_strategy = length, 
                       mean_matching = length, game = length, 
                       p1NEmix = length, p2NEmix = length, p1MMmix = length, p2MMmix = length, 
                       treatment = length, treatment2 = length,
                       ll_nash = length, ll_mm = length, ll_center = length, ll_mle = length)

# loop over session_id(periods)
for (i in 1:length(uniqueID)){
  round_data = subset(full_data, session_round_id == uniqueID[i])
  
  # fill in the mean_data row i
  mean_data$session_id[i] = round_data$session_code[1]
  mean_data$session_round_id[i] = round_data$session_round_id[1]
  mean_data$p1_average[i] = mean(round_data$p1_strategy)
  mean_data$p2_average[i] = mean(round_data$p2_strategy)
  mean_data$num_subperiods[i] = round_data$num_subperiods[1]
  mean_data$pure_strategy[i] = round_data$pure_strategy[1]
  mean_data$mean_matching[i] = round_data$mean_matching[1]
  mean_data$game[i] = round_data$game[1]
  mean_data$p1NEmix[i] = round_data$p1NEmix[1]
  mean_data$p2NEmix[i] = round_data$p2NEmix[1]
  mean_data$p1MMmix[i] = round_data$p1MMmix[1]
  mean_data$p2MMmix[i] = round_data$p2MMmix[1]
  mean_data$treatment[i] = round_data$treatment[1]
  mean_data$treatment2[i] = round_data$treatment2[1]
  
  # fill in the loglikelihood wrt NE
  mu1 = round_data$p1NEmix[1]
  mu2 = round_data$p2NEmix[1]
  sig1 = round_data$p1NEmix[1] * (1-round_data$p1NEmix[1])
  sig2 = round_data$p2NEmix[1] * (1-round_data$p2NEmix[1])
  x1 = mean(round_data$p1_strategy)
  x2 = mean(round_data$p2_strategy)
  pho = 0
  z = (x1-mu1)^2/(sig1^2) + (x2-mu2)^2/(sig2^2) - 2*pho*(x1-mu1)*(x2-mu2)/(sig1*sig2)
  mean_data$ll_nash[i] = -z/(2*(1-pho^2)) - log(2*pi*sig1*sig2*sqrt(1-pho^2))
  
  # fill in the loglikelihood wrt MM
  mu1 = round_data$p1MMmix[1]
  mu2 = round_data$p2MMmix[1]
  sig1 = round_data$p1MMmix[1] * (1-round_data$p1MMmix[1])
  sig2 = round_data$p2MMmix[1] * (1-round_data$p2MMmix[1])
  x1 = mean(round_data$p1_strategy)
  x2 = mean(round_data$p2_strategy)
  pho = 0
  z = (x1-mu1)^2/(sig1^2) + (x2-mu2)^2/(sig2^2) - 2*pho*(x1-mu1)*(x2-mu2)/(sig1*sig2)
  mean_data$ll_mm[i] = -z/(2*(1-pho^2)) - log(2*pi*sig1*sig2*sqrt(1-pho^2))
  
  # fill in the loglikelihood wrt center
  mu1 = 0.5
  mu2 = 0.5
  sig1 = 0.5 * (1-0.5)
  sig2 = 0.5 * (1-0.5)
  x1 = mean(round_data$p1_strategy)
  x2 = mean(round_data$p2_strategy)
  pho = 0
  z = (x1-mu1)^2/(sig1^2) + (x2-mu2)^2/(sig2^2) - 2*pho*(x1-mu1)*(x2-mu2)/(sig1*sig2)
  mean_data$ll_center[i] = -z/(2*(1-pho^2)) - log(2*pi*sig1*sig2*sqrt(1-pho^2))
  
  # fill in the loglikelihood wrt sample mean
  x1 = mean(round_data$p1_strategy)
  x2 = mean(round_data$p2_strategy)
  mu1 = x1
  mu2 = x2
  sig1 = x1 * (1-x1)
  sig2 = x2 * (1-x2)
  pho = cor(round_data$p1_strategy, round_data$p2_strategy)
  z = (x1-mu1)^2/(sig1^2) + (x2-mu2)^2/(sig2^2) - 2*pho*(x1-mu1)*(x2-mu2)/(sig1*sig2)
  mean_data$ll_mle[i] = -z/(2*(1-pho^2)) - log(2*pi*sig1*sig2*sqrt(1-pho^2))
}

mean_data = arrange(mean_data, session_round_id)

# Generate the Table
# drop IDDS
mean_data = filter(mean_data, game != 3)

# load package
library(MASS)

# create list
median_table = list()

# loop over game
for (i in 1:length(gametype)){
  game_data = subset(mean_data, game == i)
  
  median_table[[i]] = matrix(0, nrow = 6, ncol = 5)
  rownames(median_table[[i]]) = c('mm', 'rp', 'Mixed', 'Pure', 'Continuous', 'Discrete')
  colnames(median_table[[i]]) = c('ll wrt NE', 'll wrt Center', 'll wrt MM', 'll wrt mle', 'Num of Pairs')
  
  # mm vs rp
  data1 = subset(game_data, mean_matching == TRUE)
  median_table[[i]][1,1] = sum(data1$ll_nash)
  median_table[[i]][1,2] = sum(data1$ll_center)
  median_table[[i]][1,3] = sum(data1$ll_mm)
  median_table[[i]][1,4] = sum(data1$ll_mle)
  median_table[[i]][1,5] = length(data1$session_round_id)
  
  data2 = subset(game_data, mean_matching == FALSE)
  median_table[[i]][2,1] = sum(data2$ll_nash)
  median_table[[i]][2,2] = sum(data2$ll_center)
  median_table[[i]][2,3] = sum(data2$ll_mm)
  median_table[[i]][2,4] = sum(data2$ll_mle)
  median_table[[i]][2,5] = length(data2$session_round_id)
  
  # mixed vs pure
  data1 = subset(game_data, pure_strategy == FALSE)
  median_table[[i]][3,1] = sum(data1$ll_nash)
  median_table[[i]][3,2] = sum(data1$ll_center)
  median_table[[i]][3,3] = sum(data1$ll_mm)
  median_table[[i]][3,4] = sum(data1$ll_mle)
  median_table[[i]][3,5] = length(data1$session_round_id)
  
  data2 = subset(game_data, pure_strategy == TRUE)
  median_table[[i]][4,1] = sum(data2$ll_nash)
  median_table[[i]][4,2] = sum(data2$ll_center)
  median_table[[i]][4,3] = sum(data2$ll_mm)
  median_table[[i]][4,4] = sum(data2$ll_mle)
  median_table[[i]][4,5] = length(data2$session_round_id)
  
  # continuous vs discrete
  data1 = subset(game_data, num_subperiods == 0)
  median_table[[i]][5,1] = sum(data1$ll_nash)
  median_table[[i]][5,2] = sum(data1$ll_center)
  median_table[[i]][5,3] = sum(data1$ll_mm)
  median_table[[i]][5,4] = sum(data1$ll_mle)
  median_table[[i]][5,5] = length(data1$session_round_id)
  
  data2 = subset(game_data, num_subperiods != 0)
  median_table[[i]][6,1] = sum(data2$ll_nash)
  median_table[[i]][6,2] = sum(data2$ll_center)
  median_table[[i]][6,3] = sum(data2$ll_mm)
  median_table[[i]][6,4] = sum(data2$ll_mle)
  median_table[[i]][6,5] = length(data2$session_round_id)
}

xtable(head(median_table[[1]]),digits=3,caption="Distance to predictions.")
xtable(head(median_table[[2]]),digits=3,caption="Distance to predictions.")


##########Table (not used): Median data of time average by treatments##########
# create empty dataset
length = rep(NA, length(uniqueID))
mean_data = data.frame(session_round_id = length, p1_average = length, p2_average = length,
                       p1_median = length, p2_median = length, p1_q1 = length, p2_q1 = length,
                       p1_q3 = length, p2_q3 = length, sd_p1 = length, sd_p2 = length,
                       num_subperiods = length, pure_strategy = length, mean_matching = length, game = length, 
                       p1NEmix = length, p2NEmix = length, p1MMmix = length, p2MMmix = length, 
                       p1_payoff = length, p2_payoff = length, treatment = length, treatment2 = length)

# loop over session_id(periods)
for (i in 1:length(uniqueID)){
  round_data = subset(full_data, session_round_id == uniqueID[i])
  
  # fill in the mean_data row i
  mean_data$session_round_id[i] = round_data$session_round_id[1]
  mean_data$p1_average[i] = mean(round_data$p1_strategy)
  mean_data$p2_average[i] = mean(round_data$p2_strategy)
  mean_data$p1_median[i] = median(round_data$p1_strategy)
  mean_data$p2_median[i] = median(round_data$p2_strategy)
  mean_data$p1_q1[i] = quantile(round_data$p1_strategy, 0.25)
  mean_data$p1_q3[i] = quantile(round_data$p1_strategy, 0.75)
  mean_data$p2_q1[i] = quantile(round_data$p2_strategy, 0.25)
  mean_data$p2_q3[i] = quantile(round_data$p2_strategy, 0.75)
  mean_data$sd_p1[i] = sd(round_data$p1_strategy)
  mean_data$sd_p2[i] = sd(round_data$p2_strategy)
  mean_data$num_subperiods[i] = round_data$num_subperiods[1]
  mean_data$pure_strategy[i] = round_data$pure_strategy[1]
  mean_data$mean_matching[i] = round_data$mean_matching[1]
  mean_data$game[i] = round_data$game[1]
  mean_data$p1NEmix[i] = round_data$p1NEmix[1]
  mean_data$p2NEmix[i] = round_data$p2NEmix[1]
  mean_data$p1MMmix[i] = round_data$p1MMmix[1]
  mean_data$p2MMmix[i] = round_data$p2MMmix[1]
  mean_data$p1_payoff[i] = mean(round_data$p1_payoff)
  mean_data$p2_payoff[i] = mean(round_data$p2_payoff)
  mean_data$treatment[i] = round_data$treatment[1]
  mean_data$treatment2[i] = round_data$treatment2[1]
}

mean_data = arrange(mean_data, session_round_id)

# add necessary variables
mean_data = mean_data %>% mutate(Deviation_NE = sqrt((p1_average - p1NEmix)^2 + (p2_average - p2NEmix)^2))
mean_data = mean_data %>% mutate(Deviation_MM = sqrt((p1_average - p1MMmix)^2 + (p2_average - p2MMmix)^2))
mean_data = mean_data %>% mutate(Deviation_Mid = sqrt((p1_average - 0.5)^2 + (p2_average - 0.5)^2))
mean_data = mean_data %>% mutate(IQR_p1 = p1_q3 - p1_q1)
mean_data = mean_data %>% mutate(IQR_p2 = p2_q3 - p2_q1)
mean_data = mean_data %>% mutate(IQR_mean = (IQR_p1 + IQR_p2) / 2)
mean_data = mean_data %>% mutate(IQR_square = sqrt(IQR_p1^2 + IQR_p2^2))
mean_data = mean_data %>% mutate(IQR_harmonic = (IQR_p1 * IQR_p2) / (IQR_p1 + IQR_p2))
mean_data = mean_data %>% mutate(IQR_geometric = sqrt(IQR_p1 * IQR_p2))
mean_data = mean_data %>% mutate(sd_mean = (sd_p1 + sd_p2) / 2)
mean_data = mean_data %>% mutate(sd_square = sqrt(sd_p1^2 + sd_p2^2))
mean_data = mean_data %>% mutate(sd_harmonic = (sd_p1 * sd_p2) / (sd_p1 + sd_p2))
mean_data = mean_data %>% mutate(sd_geometric = sqrt(sd_p1 * sd_p2))

# read qre data from gambit
qre_8002 = read.csv("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/gambit/qre_8002.csv", header = T)
qre_3117 = read.csv("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/gambit/qre_3117.csv", header = T)

# add qre distance and optimal lambda
mean_data = mean_data %>% mutate(lambda = 0)
mean_data = mean_data %>% mutate(Deviation_QRE = 0)

for (i in 1:length(mean_data$session_round_id)){
  
  # select qre dataset based on game type
  if (mean_data$game[i] == 1){qre_data = qre_3117}
  else{qre_data = qre_8002}
  
  # find the optimal lambda
  distance_min = 100000
  for (k in 1:length(qre_data$lambda)){
    p1 = qre_data$p1_row[k]
    p2 = qre_data$p2_row[k]
    distance = sqrt((mean_data$p1_average[i] - p1)^2 + (mean_data$p2_average[i] - p2)^2)
    if (distance < distance_min){
      distance_min = distance
      lambda_min = qre_data$lambda[k]
    }
  }
  
  mean_data$lambda[i] = lambda_min
  mean_data$Deviation_QRE[i] = distance_min
}

# export dataset to stata
write.dta(mean_data, "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/stata/mp_summary.dta")


# Generate Table 3
# load package
library(MASS)

# create list
median_table = list()

# loop over game
for (i in 1:length(gametype)){
  game_data = subset(mean_data, game == i)
  
  median_table[[i]] = matrix(0, nrow = 6, ncol = 6)
  rownames(median_table[[i]]) = c('mm', 'rp', 'Mixed', 'Pure', 'Continuous', 'Discrete')
  colnames(median_table[[i]]) = c('Median Distance to NE', ' ','Median Distance to Center', ' ', 'Median Distance to MM', 'IQR harmonic')
  
  # mm vs rp
  data1 = subset(game_data, mean_matching == TRUE)
  median_table[[i]][1,1] = median(data1$Deviation_NE)
  test = wilcox.test(data1$Deviation_NE, data1$Deviation_Mid, paired=TRUE)
  median_table[[i]][1,2] = round(test$p.value, digits = 3)
  median_table[[i]][1,3] = median(data1$Deviation_Mid)
  test = wilcox.test(data1$Deviation_Mid, data1$Deviation_MM, paired=TRUE)
  median_table[[i]][1,4] = round(test$p.value, digits = 3)
  median_table[[i]][1,5] = median(data1$Deviation_MM)
  median_table[[i]][1,6] = median(data1$IQR_geometric)
  
  data2 = subset(game_data, mean_matching == FALSE)
  median_table[[i]][2,1] = median(data2$Deviation_NE)
  test = wilcox.test(data2$Deviation_NE, data2$Deviation_Mid, paired=TRUE)
  median_table[[i]][2,2] = round(test$p.value, digits = 3)
  median_table[[i]][2,3] = median(data2$Deviation_Mid)
  test = wilcox.test(data2$Deviation_Mid, data2$Deviation_MM, paired=TRUE)
  median_table[[i]][2,4] = round(test$p.value, digits = 3)
  median_table[[i]][2,5] = median(data2$Deviation_MM)
  median_table[[i]][2,6] = median(data2$IQR_geometric)
  
  
  # mixed vs pure
  data1 = subset(game_data, pure_strategy == FALSE)
  median_table[[i]][3,1] = median(data1$Deviation_NE)
  test = wilcox.test(data1$Deviation_NE, data1$Deviation_Mid, paired=TRUE)
  median_table[[i]][3,2] = round(test$p.value, digits = 3)
  median_table[[i]][3,3] = median(data1$Deviation_Mid)
  test = wilcox.test(data1$Deviation_Mid, data1$Deviation_MM, paired=TRUE)
  median_table[[i]][3,4] = round(test$p.value, digits = 3)
  median_table[[i]][3,5] = median(data1$Deviation_MM)
  median_table[[i]][3,6] = median(data1$IQR_geometric)
  
  data2 = subset(game_data, pure_strategy == TRUE)
  median_table[[i]][4,1] = median(data2$Deviation_NE)
  test = wilcox.test(data2$Deviation_NE, data2$Deviation_Mid, paired=TRUE)
  median_table[[i]][4,2] = round(test$p.value, digits = 3)
  median_table[[i]][4,3] = median(data2$Deviation_Mid)
  test = wilcox.test(data2$Deviation_Mid, data2$Deviation_MM, paired=TRUE)
  median_table[[i]][4,4] = round(test$p.value, digits = 3)
  median_table[[i]][4,5] = median(data2$Deviation_MM)
  median_table[[i]][4,6] = median(data2$IQR_geometric)
  
  
  # continuous vs discrete
  data1 = subset(game_data, num_subperiods == 0)
  median_table[[i]][5,1] = median(data1$Deviation_NE)
  test = wilcox.test(data1$Deviation_NE, data1$Deviation_Mid, paired=TRUE)
  median_table[[i]][5,2] = round(test$p.value, digits = 3)
  median_table[[i]][5,3] = median(data1$Deviation_Mid)
  test = wilcox.test(data1$Deviation_Mid, data1$Deviation_MM, paired=TRUE)
  median_table[[i]][5,4] = round(test$p.value, digits = 3)
  median_table[[i]][5,5] = median(data1$Deviation_MM)
  median_table[[i]][5,6] = median(data1$IQR_geometric)
  
  data2 = subset(game_data, num_subperiods == 15 | num_subperiods == 25)
  median_table[[i]][6,1] = median(data2$Deviation_NE)
  test = wilcox.test(data2$Deviation_NE, data2$Deviation_Mid, paired=TRUE)
  median_table[[i]][6,2] = round(test$p.value, digits = 3)
  median_table[[i]][6,3] = median(data2$Deviation_Mid)
  test = wilcox.test(data2$Deviation_Mid, data2$Deviation_MM, paired=TRUE)
  median_table[[i]][6,4] = round(test$p.value, digits = 3)
  median_table[[i]][6,5] = median(data2$Deviation_MM)
  median_table[[i]][6,6] = median(data2$IQR_geometric)
  
}

xtable(head(median_table[[1]]),digits=3,caption="Distance to predictions.")
xtable(head(median_table[[2]]),digits=3,caption="Distance to predictions.")
xtable(head(median_table[[3]]),digits=3,caption="Distance to predictions.")


# Generate Table 4 (full version of Table 3)
# create list
median_table = list()

# loop over game
for (i in 1:length(gametype)){
  game_data = subset(mean_data, game == i)
  
  median_table[[i]] = matrix(0, nrow = 9, ncol = 11)
  rownames(median_table[[i]]) = c('mm', 'rp', 'p-match', 
                                  'Mixed', 'Pure', 'p-action',
                                  'Continuous', 'Discrete', 'p-time')
  colnames(median_table[[i]]) = c('row median', 'column median',
                                  'To NE', 'p-value','To Center', 'p-value', 'To MM', 
                                  'IQR Harmonic', 'IQR Geometric', 'QRE lambda', 'To QRE')
  
  # mm vs rp
  data1 = subset(game_data, mean_matching == TRUE)
  median_table[[i]][1,1] = median(data1$p1_average)
  median_table[[i]][1,2] = median(data1$p2_average)
  median_table[[i]][1,3] = median(data1$Deviation_NE)
  test = wilcox.test(data1$Deviation_NE, data1$Deviation_Mid, paired=TRUE)
  median_table[[i]][1,4] = round(test$p.value, digits = 3)
  median_table[[i]][1,5] = median(data1$Deviation_Mid)
  test = wilcox.test(data1$Deviation_Mid, data1$Deviation_MM, paired=TRUE)
  median_table[[i]][1,6] = round(test$p.value, digits = 3)
  median_table[[i]][1,7] = median(data1$Deviation_MM)
  median_table[[i]][1,8] = median(data1$IQR_harmonic, na.rm=TRUE)
  median_table[[i]][1,9] = median(data1$IQR_geometric)
  median_table[[i]][1,10] = median(data1$lambda)
  median_table[[i]][1,11] = median(data1$Deviation_QRE)
  
  data2 = subset(game_data, mean_matching == FALSE)
  median_table[[i]][2,1] = median(data2$p1_average)
  median_table[[i]][2,2] = median(data2$p2_average)
  median_table[[i]][2,3] = median(data2$Deviation_NE)
  test = wilcox.test(data2$Deviation_NE, data2$Deviation_Mid, paired=TRUE)
  median_table[[i]][2,4] = round(test$p.value, digits = 3)
  median_table[[i]][2,5] = median(data2$Deviation_Mid)
  test = wilcox.test(data2$Deviation_Mid, data2$Deviation_MM, paired=TRUE)
  median_table[[i]][2,6] = round(test$p.value, digits = 3)
  median_table[[i]][2,7] = median(data2$Deviation_MM)
  median_table[[i]][2,8] = median(data2$IQR_harmonic, na.rm=TRUE)
  median_table[[i]][2,9] = median(data2$IQR_geometric)
  median_table[[i]][2,10] = median(data2$lambda)
  median_table[[i]][2,11] = median(data2$Deviation_QRE)
  
  test = wilcox.test(data1$p1_average, data2$p1_average)
  median_table[[i]][3,1] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$p2_average, data2$p2_average)
  median_table[[i]][3,2] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$Deviation_NE, data2$Deviation_NE)
  median_table[[i]][3,3] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$Deviation_Mid, data2$Deviation_Mid)
  median_table[[i]][3,5] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$Deviation_MM, data2$Deviation_MM)
  median_table[[i]][3,7] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$IQR_harmonic, data2$IQR_harmonic)
  median_table[[i]][3,8] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$IQR_geometric, data2$IQR_geometric)
  median_table[[i]][3,9] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$lambda, data2$lambda)
  median_table[[i]][3,10] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$Deviation_QRE, data2$Deviation_QRE)
  median_table[[i]][3,11] = round(test$p.value, digits = 3)
  
  
  # mixed vs pure
  data1 = subset(game_data, pure_strategy == FALSE)
  median_table[[i]][4,1] = median(data1$p1_average)
  median_table[[i]][4,2] = median(data1$p2_average)
  median_table[[i]][4,3] = median(data1$Deviation_NE)
  test = wilcox.test(data1$Deviation_NE, data1$Deviation_Mid, paired=TRUE)
  median_table[[i]][4,4] = round(test$p.value, digits = 3)
  median_table[[i]][4,5] = median(data1$Deviation_Mid)
  test = wilcox.test(data1$Deviation_Mid, data1$Deviation_MM, paired=TRUE)
  median_table[[i]][4,6] = round(test$p.value, digits = 3)
  median_table[[i]][4,7] = median(data1$Deviation_MM)
  median_table[[i]][4,8] = median(data1$IQR_harmonic, na.rm=TRUE)
  median_table[[i]][4,9] = median(data1$IQR_geometric)
  median_table[[i]][4,10] = median(data1$lambda)
  median_table[[i]][4,11] = median(data1$Deviation_QRE)
  
  data2 = subset(game_data, pure_strategy == TRUE)
  median_table[[i]][5,1] = median(data2$p1_average)
  median_table[[i]][5,2] = median(data2$p2_average)
  median_table[[i]][5,3] = median(data2$Deviation_NE)
  test = wilcox.test(data2$Deviation_NE, data2$Deviation_Mid, paired=TRUE)
  median_table[[i]][5,4] = round(test$p.value, digits = 3)
  median_table[[i]][5,5] = median(data2$Deviation_Mid)
  test = wilcox.test(data2$Deviation_Mid, data2$Deviation_MM, paired=TRUE)
  median_table[[i]][5,6] = round(test$p.value, digits = 3)
  median_table[[i]][5,7] = median(data2$Deviation_MM)
  median_table[[i]][5,8] = median(data2$IQR_harmonic, na.rm=TRUE)
  median_table[[i]][5,9] = median(data2$IQR_geometric)
  median_table[[i]][5,10] = median(data2$lambda)
  median_table[[i]][5,11] = median(data2$Deviation_QRE)
  
  test = wilcox.test(data1$p1_average, data2$p1_average)
  median_table[[i]][6,1] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$p2_average, data2$p2_average)
  median_table[[i]][6,2] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$Deviation_NE, data2$Deviation_NE)
  median_table[[i]][6,3] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$Deviation_Mid, data2$Deviation_Mid)
  median_table[[i]][6,5] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$Deviation_MM, data2$Deviation_MM)
  median_table[[i]][6,7] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$IQR_harmonic, data2$IQR_harmonic)
  median_table[[i]][6,8] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$IQR_geometric, data2$IQR_geometric)
  median_table[[i]][6,9] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$lambda, data2$lambda)
  median_table[[i]][6,10] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$Deviation_QRE, data2$Deviation_QRE)
  median_table[[i]][6,11] = round(test$p.value, digits = 3)
  
  # continuous vs discrete
  data1 = subset(game_data, num_subperiods == 0)
  median_table[[i]][7,1] = median(data1$p1_average)
  median_table[[i]][7,2] = median(data1$p2_average)
  median_table[[i]][7,3] = median(data1$Deviation_NE)
  test = wilcox.test(data1$Deviation_NE, data1$Deviation_Mid, paired=TRUE)
  median_table[[i]][7,4] = round(test$p.value, digits = 3)
  median_table[[i]][7,5] = median(data1$Deviation_Mid)
  test = wilcox.test(data1$Deviation_Mid, data1$Deviation_MM, paired=TRUE)
  median_table[[i]][7,6] = round(test$p.value, digits = 3)
  median_table[[i]][7,7] = median(data1$Deviation_MM)
  median_table[[i]][7,8] = median(data1$IQR_harmonic, na.rm=TRUE)
  median_table[[i]][7,9] = median(data1$IQR_geometric)
  median_table[[i]][7,10] = median(data1$lambda)
  median_table[[i]][7,11] = median(data1$Deviation_QRE)
  
  data2 = subset(game_data, num_subperiods == 15 | num_subperiods == 25)
  median_table[[i]][8,1] = median(data2$p1_average)
  median_table[[i]][8,2] = median(data2$p2_average)
  median_table[[i]][8,3] = median(data2$Deviation_NE)
  test = wilcox.test(data2$Deviation_NE, data2$Deviation_Mid, paired=TRUE)
  median_table[[i]][8,4] = round(test$p.value, digits = 3)
  median_table[[i]][8,5] = median(data2$Deviation_Mid)
  test = wilcox.test(data2$Deviation_Mid, data2$Deviation_MM, paired=TRUE)
  median_table[[i]][8,6] = round(test$p.value, digits = 3)
  median_table[[i]][8,7] = median(data2$Deviation_MM)
  median_table[[i]][8,8] = median(data2$IQR_harmonic, na.rm=TRUE)
  median_table[[i]][8,9] = median(data2$IQR_geometric)
  median_table[[i]][8,10] = median(data2$lambda)
  median_table[[i]][8,11] = median(data2$Deviation_QRE)
  
  test = wilcox.test(data1$p1_average, data2$p1_average)
  median_table[[i]][9,1] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$p2_average, data2$p2_average)
  median_table[[i]][9,2] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$Deviation_NE, data2$Deviation_NE)
  median_table[[i]][9,3] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$Deviation_Mid, data2$Deviation_Mid)
  median_table[[i]][9,5] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$Deviation_MM, data2$Deviation_MM)
  median_table[[i]][9,7] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$IQR_harmonic, data2$IQR_harmonic)
  median_table[[i]][9,8] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$IQR_geometric, data2$IQR_geometric)
  median_table[[i]][9,9] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$lambda, data2$lambda)
  median_table[[i]][9,10] = round(test$p.value, digits = 3)
  test = wilcox.test(data1$Deviation_QRE, data2$Deviation_QRE)
  median_table[[i]][9,11] = round(test$p.value, digits = 3)
  
}

xtable(median_table[[1]],digits=3,caption="Distance to predictions.")
xtable(median_table[[2]],digits=3,caption="Distance to predictions.")
xtable(median_table[[3]],digits=3,caption="Distance to predictions.")


##########Table (not used): Median of the means and compare it with median by games##########
# create empty dataset
length = rep(NA, length(uniqueID))
mean_data = data.frame(session_round_id = length, p1_average = length, p2_average = length,
                       p1_median = length, p2_median = length, p1_q1 = length, p2_q1 = length,
                       p1_q3 = length, p2_q3 = length, sd_p1 = length, sd_p2 = length,
                       num_subperiods = length, pure_strategy = length, mean_matching = length, game = length, 
                       p1NEmix = length, p2NEmix = length, p1MMmix = length, p2MMmix = length, 
                       p1_payoff = length, p2_payoff = length, treatment = length, treatment2 = length)

# loop over session_id(periods)
for (i in 1:length(uniqueID)){
  round_data = subset(full_data, session_round_id == uniqueID[i])
  
  # fill in the mean_data row i
  mean_data$session_round_id[i] = round_data$session_round_id[1]
  mean_data$p1_average[i] = mean(round_data$p1_strategy)
  mean_data$p2_average[i] = mean(round_data$p2_strategy)
  mean_data$p1_median[i] = median(round_data$p1_strategy)
  mean_data$p2_median[i] = median(round_data$p2_strategy)
  mean_data$p1_q1[i] = quantile(round_data$p1_strategy, 0.25)
  mean_data$p1_q3[i] = quantile(round_data$p1_strategy, 0.75)
  mean_data$p2_q1[i] = quantile(round_data$p2_strategy, 0.25)
  mean_data$p2_q3[i] = quantile(round_data$p2_strategy, 0.75)
  mean_data$sd_p1[i] = sd(round_data$p1_strategy)
  mean_data$sd_p2[i] = sd(round_data$p2_strategy)
  mean_data$num_subperiods[i] = round_data$num_subperiods[1]
  mean_data$pure_strategy[i] = round_data$pure_strategy[1]
  mean_data$mean_matching[i] = round_data$mean_matching[1]
  mean_data$game[i] = round_data$game[1]
  mean_data$p1NEmix[i] = round_data$p1NEmix[1]
  mean_data$p2NEmix[i] = round_data$p2NEmix[1]
  mean_data$p1MMmix[i] = round_data$p1MMmix[1]
  mean_data$p2MMmix[i] = round_data$p2MMmix[1]
  mean_data$p1_payoff[i] = mean(round_data$p1_payoff)
  mean_data$p2_payoff[i] = mean(round_data$p2_payoff)
  mean_data$treatment[i] = round_data$treatment[1]
  mean_data$treatment2[i] = round_data$treatment2[1]
}

mean_data = arrange(mean_data, session_round_id)

# add necessary variables
mean_data = mean_data %>% mutate(Deviation_NE = sqrt((p1_average - p1NEmix)^2 + (p2_average - p2NEmix)^2))
mean_data = mean_data %>% mutate(Deviation_MM = sqrt((p1_average - p1MMmix)^2 + (p2_average - p2MMmix)^2))
mean_data = mean_data %>% mutate(Deviation_Mid = sqrt((p1_average - 0.5)^2 + (p2_average - 0.5)^2))
mean_data = mean_data %>% mutate(IQR_p1 = p1_q3 - p1_q1)
mean_data = mean_data %>% mutate(IQR_p2 = p2_q3 - p2_q1)
mean_data = mean_data %>% mutate(IQR_mean = (IQR_p1 + IQR_p2) / 2)
mean_data = mean_data %>% mutate(IQR_square = sqrt(IQR_p1^2 + IQR_p2^2))
mean_data = mean_data %>% mutate(IQR_harmonic = (IQR_p1 * IQR_p2) / (IQR_p1 + IQR_p2))
mean_data = mean_data %>% mutate(IQR_geometric = sqrt(IQR_p1 * IQR_p2))
mean_data = mean_data %>% mutate(sd_mean = (sd_p1 + sd_p2) / 2)
mean_data = mean_data %>% mutate(sd_square = sqrt(sd_p1^2 + sd_p2^2))
mean_data = mean_data %>% mutate(sd_harmonic = (sd_p1 * sd_p2) / (sd_p1 + sd_p2))
mean_data = mean_data %>% mutate(sd_geometric = sqrt(sd_p1 * sd_p2))

# read qre data from gambit
qre_8002 = read.csv("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/gambit/qre_8002.csv", header = T)
qre_3117 = read.csv("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/gambit/qre_3117.csv", header = T)

# add qre distance and optimal lambda
mean_data = mean_data %>% mutate(lambda = 0)
mean_data = mean_data %>% mutate(Deviation_QRE = 0)

for (i in 1:length(mean_data$session_round_id)){
  
  # select qre dataset based on game type
  if (mean_data$game[i] == 1){qre_data = qre_3117}
  else{qre_data = qre_8002}
  
  # find the optimal lambda
  distance_min = 100000
  for (k in 1:length(qre_data$lambda)){
    p1 = qre_data$p1_row[k]
    p2 = qre_data$p2_row[k]
    distance = sqrt((mean_data$p1_average[i] - p1)^2 + (mean_data$p2_average[i] - p2)^2)
    if (distance < distance_min){
      distance_min = distance
      lambda_min = qre_data$lambda[k]
    }
  }
  
  mean_data$lambda[i] = lambda_min
  mean_data$Deviation_QRE[i] = distance_min
}

# Generate Table 5
# create list
median_table = list()

# loop over game
for (i in 1:length(gametype)){
  game_data = subset(mean_data, game == i)
  
  median_table[[i]] = matrix(0, nrow = 6, ncol = 16)
  rownames(median_table[[i]]) = c('mm', 'rp',
                                  'Mixed', 'Pure', 
                                  'Continuous', 'Discrete')
  colnames(median_table[[i]]) = c('row median', ' ', 'column median', ' ',
                                  'To NE', ' ','To Center', ' ', 'To MM', ' ',
                                  'To QRE', ' ', 'QRE lambda', ' ',
                                  'IQR Geometric', ' ')
  
  # mm vs rp
  data1 = subset(game_data, mean_matching == TRUE)
  median_table[[i]][1,1] = median(data1$p1_average)
  median_table[[i]][1,3] = median(data1$p2_average)
  median_table[[i]][1,5] = median(data1$Deviation_NE)
  median_table[[i]][1,7] = median(data1$Deviation_Mid)
  median_table[[i]][1,9] = median(data1$Deviation_MM)
  median_table[[i]][1,11] = median(data1$Deviation_QRE)
  median_table[[i]][1,13] = median(data1$lambda)
  median_table[[i]][1,15] = median(data1$IQR_geometric)
  
  data2 = subset(game_data, mean_matching == FALSE)
  median_table[[i]][2,1] = median(data2$p1_average)
  median_table[[i]][2,3] = median(data2$p2_average)
  median_table[[i]][2,5] = median(data2$Deviation_NE)
  median_table[[i]][2,7] = median(data2$Deviation_Mid)
  median_table[[i]][2,9] = median(data2$Deviation_MM)
  median_table[[i]][2,11] = median(data2$Deviation_QRE)
  median_table[[i]][2,13] = median(data2$lambda)
  median_table[[i]][2,15] = median(data2$IQR_geometric)
  
  # mixed vs pure
  data1 = subset(game_data, pure_strategy == FALSE)
  median_table[[i]][3,1] = median(data1$p1_average)
  median_table[[i]][3,3] = median(data1$p2_average)
  median_table[[i]][3,5] = median(data1$Deviation_NE)
  median_table[[i]][3,7] = median(data1$Deviation_Mid)
  median_table[[i]][3,9] = median(data1$Deviation_MM)
  median_table[[i]][3,11] = median(data1$Deviation_QRE)
  median_table[[i]][3,13] = median(data1$lambda)
  median_table[[i]][3,15] = median(data1$IQR_geometric)
  
  data2 = subset(game_data, pure_strategy == TRUE)
  median_table[[i]][4,1] = median(data2$p1_average)
  median_table[[i]][4,3] = median(data2$p2_average)
  median_table[[i]][4,5] = median(data2$Deviation_NE)
  median_table[[i]][4,7] = median(data2$Deviation_Mid)
  median_table[[i]][4,9] = median(data2$Deviation_MM)
  median_table[[i]][4,11] = median(data2$Deviation_QRE)
  median_table[[i]][4,13] = median(data2$lambda)
  median_table[[i]][4,15] = median(data2$IQR_geometric)
  
  # continuous vs discrete
  data1 = subset(game_data, num_subperiods == 0)
  median_table[[i]][5,1] = median(data1$p1_average)
  median_table[[i]][5,3] = median(data1$p2_average)
  median_table[[i]][5,5] = median(data1$Deviation_NE)
  median_table[[i]][5,7] = median(data1$Deviation_Mid)
  median_table[[i]][5,9] = median(data1$Deviation_MM)
  median_table[[i]][5,11] = median(data1$Deviation_QRE)
  median_table[[i]][5,13] = median(data1$lambda)
  median_table[[i]][5,15] = median(data1$IQR_geometric)
  
  data2 = subset(game_data, num_subperiods == 15 | num_subperiods == 25)
  median_table[[i]][6,1] = median(data2$p1_average)
  median_table[[i]][6,3] = median(data2$p2_average)
  median_table[[i]][6,5] = median(data2$Deviation_NE)
  median_table[[i]][6,7] = median(data2$Deviation_Mid)
  median_table[[i]][6,9] = median(data2$Deviation_MM)
  median_table[[i]][6,11] = median(data2$Deviation_QRE)
  median_table[[i]][6,13] = median(data2$lambda)
  median_table[[i]][6,15] = median(data2$IQR_geometric)
  
  # update the reference line in each game
  for (k in 1:6){
    median_table[[i]][k,2] = median(game_data$p1_average)
    median_table[[i]][k,4] = median(game_data$p2_average)
    median_table[[i]][k,6] = median(game_data$Deviation_NE)
    median_table[[i]][k,8] = median(game_data$Deviation_Mid)
    median_table[[i]][k,10] = median(game_data$Deviation_MM)
    median_table[[i]][k,12] = median(game_data$Deviation_QRE)
    median_table[[i]][k,14] = median(game_data$lambda)
    median_table[[i]][k,16] = median(game_data$IQR_geometric)
  }

}

xtable(median_table[[1]],digits=3,caption="Distance to predictions.")
xtable(median_table[[2]],digits=3,caption="Distance to predictions.")
xtable(median_table[[3]],digits=3,caption="Distance to predictions.")


##########Figure (not used): Transition between 8002 and 3117##########
# create empty dataset
length = rep(NA, length(uniqueID))
mean_data = data.frame(round = length, p1_average = length, p2_average = length,
                       num_subperiods = length, pure_strategy = length, mean_matching = length, game = length)

# loop over session_id(periods)
for (i in 1:length(uniqueID)){
  round_data = subset(full_data, session_round_id == uniqueID[i])
  
  # fill in the mean_data row i
  mean_data$round[i] = round_data$round[1]
  mean_data$p1_average[i] = mean(round_data$p1_strategy)
  mean_data$p2_average[i] = mean(round_data$p2_strategy)
  mean_data$num_subperiods[i] = round_data$num_subperiods[1]
  mean_data$pure_strategy[i] = round_data$pure_strategy[1]
  mean_data$mean_matching[i] = round_data$mean_matching[1]
  mean_data$game[i] = round_data$game[1]
}

mean_data = arrange(mean_data, round)

# get mean matching data and plot the 
mean_data_mm = subset(mean_data, mean_matching == TRUE & game != 3)

title = 'transition from 8002 to 3117 mean matching'
file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/supplement figures/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file)

plot(mean_data_mm$round, mean_data_mm$p1_average,
     type = 'p', col = 'blue',
     main = 'Mean over rounds', xlab = 'rounds', ylab = 'round mixture')
lines(mean_data_mm$round, mean_data_mm$p2_average, type = 'p', col = 'red')
abline(v = 4.5)
abline(v = 16.5)

dev.off()

# get random pairwise data and plot
mean_data_rp = subset(mean_data, mean_matching == FALSE & game != 3)

title = 'transition from 8002 to 3117 random pairwise'
file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/supplement figures/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file)

plot(mean_data_rp$round, mean_data_rp$p1_average,
     type = 'p', col = 'blue',
     main = 'Mean over rounds', xlab = 'rounds', ylab = 'round mixture')
lines(mean_data_rp$round, mean_data_rp$p2_average, type = 'p', col = 'red')
abline(v = 6.5)
abline(v = 24.5)

dev.off()


##########Figure (not used): Empirical switch rate##########
# calculate payoff differences
full_data = full_data %>% mutate(p1_next_payoff = payoff1Aa*p1_strategy*p2_next + payoff1Ab*p1_strategy*(1-p2_next) + payoff1Ba*(1-p1_strategy)*p2_next + payoff1Bb*(1-p1_strategy)*(1-p2_next))
full_data = full_data %>% mutate(p2_next_payoff = payoff2Aa*p1_next*p2_strategy + payoff2Ab*p1_next*(1-p2_strategy) + payoff2Ba*(1-p1_next)*p2_strategy + payoff2Bb*(1-p1_next)*(1-p2_strategy))
full_data = full_data %>% mutate(p1_diff_payoff = round(p1_next_payoff - p1_payoff, digits = 1))
full_data = full_data %>% mutate(p2_diff_payoff = round(p2_next_payoff - p2_payoff, digits = 1))

# check sign preservation
full_data = full_data %>% mutate(p1_sign_preservation = p1_diff * p1_sign)
full_data = full_data %>% mutate(p2_sign_preservation = p2_diff * p2_sign)
full_data = full_data %>% mutate(p1_sign_pre = ifelse(p1_sign_preservation>0, 1, ifelse(p1_sign_preservation == 0, 0, -1)))
full_data = full_data %>% mutate(p2_sign_pre = ifelse(p2_sign_preservation>0, 1, ifelse(p2_sign_preservation == 0, 0, -1)))

# use random pairwise mixed strategy as test data
test_data = subset(full_data, pure_strategy == FALSE & mean_matching == FALSE)

# create difference function
sr1_data = data.frame(p1_switch_rate = tapply(test_data$p1_sign_pre, test_data$p1_diff_payoff, mean),
                      p1_diff_payoff = tapply(test_data$p1_diff_payoff, test_data$p1_diff_payoff, mean))
sr2_data = data.frame(p2_switch_rate = tapply(test_data$p2_sign_pre, test_data$p2_diff_payoff, mean),
                      p2_diff_payoff = tapply(test_data$p2_diff_payoff, test_data$p2_diff_payoff, mean))

# show switch rate plots
title = 'empirical switch rate'
file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/data_summary/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 700, height = 400)

par(mfrow=c(1,2))
plot(sr1_data$p1_diff_payoff, sr1_data$p1_switch_rate, type='l', xlim = c(-100,100))
plot(sr2_data$p2_diff_payoff, sr2_data$p2_switch_rate, type='l', xlim = c(-100,100))

dev.off()


##########Figure & Table (not used): data summary chart and table by game##########
# create data container
plot_data = list()

# loop over game
for (i in 1:length(gametype)){
  game_data = subset(full_data, game_new == gametype[i])
  
  # create empty data matrix
  plot_data[[i]] = matrix(0, nrow = 8, ncol = 12)
  rownames(plot_data[[i]]) = c('pd_rp', 'pc_rp', 'md_rp', 'mc_rp',
                               'pd_mm', 'pc_mm', 'md_mm', 'mc_mm')
  colnames(plot_data[[i]]) = c('p1_median', 'p1_sd', 'pvalue_NE', 'pvalue_MM', 
                               'p2_median', 'p2_sd', 'pvalue_NE', 'pvalue_MM',
                               'p1_lower', 'p1_upper', 'p2_lower', 'p2_upper')
  
  for (j in 1:max(game_data$treatment2)){
    treatment_data = subset(game_data, treatment2 == j)
    
    # record median and sd for mean matching treatment
    if (treatment_data$mean_matching[1] == 'TRUE'){
      
      # plot_data[[i]][j,1] = median(treatment_data$p1_average)
      # plot_data[[i]][j,2] = sd(treatment_data$p1_average)
      # plot_data[[i]][j,5] = median(treatment_data$p2_average)
      # plot_data[[i]][j,6] = sd(treatment_data$p2_average)
      # plot_data[[i]][j,9] = quantile(treatment_data$p1_average, 0.25)
      # plot_data[[i]][j,10] = quantile(treatment_data$p1_average, 0.75)
      # plot_data[[i]][j,11] = quantile(treatment_data$p2_average, 0.25)
      # plot_data[[i]][j,12] = quantile(treatment_data$p2_average, 0.75)
      
      plot_data[[i]][j,1] = mean(treatment_data$p1_average)
      plot_data[[i]][j,2] = sd(treatment_data$p1_average)
      plot_data[[i]][j,5] = mean(treatment_data$p2_average)
      plot_data[[i]][j,6] = sd(treatment_data$p2_average)
      plot_data[[i]][j,9] = mean(treatment_data$p1_average) - sd(treatment_data$p1_average)
      plot_data[[i]][j,10] = mean(treatment_data$p1_average) + sd(treatment_data$p1_average)
      plot_data[[i]][j,11] = mean(treatment_data$p2_average) - sd(treatment_data$p2_average)
      plot_data[[i]][j,12] = mean(treatment_data$p2_average) + sd(treatment_data$p2_average)
      
      plot_data[[i]] = data.frame(plot_data[[i]])
      plot_data[[i]]['type'] = rownames(plot_data[[i]])
    }
    
    # record mean and sd for random pairwise treatment
    if (treatment_data$mean_matching[1] == 'FALSE'){
      mean_p1 = matrix()
      mean_p2 = matrix()
      uniquepair = unique(treatment_data$session_round_id)
      
      # loop over pairs
      for (k in 1:length(uniquepair)){
        round_data = subset(treatment_data, session_round_id == uniquepair[k])
        mean_p1_new = matrix(tapply(round_data$p1_average, round_data$tick, mean))
        mean_p2_new = matrix(tapply(round_data$p2_average, round_data$tick, mean))
        mean_p1 = rbind(mean_p1, mean_p1_new)
        mean_p2 = rbind(mean_p2, mean_p2_new)
      }
      
      # generate vectors of mean data clustered at session_round level and by tick
      mean_p1 = mean_p1[-1,]
      mean_p2 = mean_p2[-1,]
      
      # plot_data[[i]][j,1] = median(mean_p1)
      # plot_data[[i]][j,2] = sd(mean_p1)
      # plot_data[[i]][j,5] = median(mean_p2)
      # plot_data[[i]][j,6] = sd(mean_p2)
      # plot_data[[i]][j,9] = quantile(mean_p1, 0.25)
      # plot_data[[i]][j,10] = quantile(mean_p1, 0.75)
      # plot_data[[i]][j,11] = quantile(mean_p2, 0.25)
      # plot_data[[i]][j,12] = quantile(mean_p2, 0.75)
      
      plot_data[[i]][j,1] = mean(mean_p1)
      plot_data[[i]][j,2] = sd(mean_p1)
      plot_data[[i]][j,5] = mean(mean_p2)
      plot_data[[i]][j,6] = sd(mean_p2)
      plot_data[[i]][j,9] = mean(mean_p1) - sd(mean_p1)
      plot_data[[i]][j,10] = mean(mean_p1) + sd(mean_p1)
      plot_data[[i]][j,11] = mean(mean_p2) - sd(mean_p2)
      plot_data[[i]][j,12] = mean(mean_p2) + sd(mean_p2)
      
      plot_data[[i]] = data.frame(plot_data[[i]])
      plot_data[[i]]['type'] = rownames(plot_data[[i]])
    }
  }
  
  # set title
  title = paste('data summary median', as.character(game_data$game_new[1]))
  file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/data_summary/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 600, height = 600)
  
  # ggplot with rectanglers, NE and MM points.
  pic = ggplot() +
    
    geom_rect(data = plot_data[[i]], mapping = aes(xmin = p1_lower, ymin = p2_lower, 
                                                   xmax = p1_upper, ymax = p2_upper, color = type, fill = type), 
              alpha = 0.01, size = 1.5) +
    
    geom_point(data = plot_data[[i]], mapping = aes(x = p1_median, y = p2_median, color = type)) +
    
    geom_text(aes(x = game_data$p1NEmix[1], y = game_data$p2NEmix[1],
                  label = 'NE'), vjust = 0, hjust = 0) +
    
    geom_text(aes(x = game_data$p1MMmix[1], y = game_data$p2MMmix[1],
                  label = 'MM'), vjust = 0, hjust = 0) +
    
    ggtitle(title) +
    scale_x_continuous(name='row strategy', limits = c(-0.1,1.1), breaks = seq(-0.1,1.1,0.1)) +
    scale_y_continuous(name='column strategy', limits = c(-0.1,1.1), breaks = seq(-0.1,1.1,0.1)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 30),
          axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
          legend.text = element_text(size = 15))
  
  print(pic)
  dev.off()
}


##########Descriptive figure (not used): payoff heatmap and best response diagram for AMPa##########
library(lattice)
library(MASS)
library(grid)
library(ggplot2)

# Generate row player's payoff matrix in levelplot, change with payoff matrix in experiment
x = seq(from = 0, to = 1, length.out = 100)
y = seq(from = 0, to = 1, length.out = 100)
z = matrix(0, nrow=100, ncol=100)
for (i in 1:100){
  for (j in 1:100){
    z[i,j] =800*x[i]*y[j] + 0*x[i]*(1-y[j]) + 0*(1-x[i])*y[j] + 200*(1-x[i])*(1-y[j])
  }
}

# Set colors in levelplot: rainbow or greyscale
n = 10
colours = rev(rainbow(n, s = 1, v = 1, start = 0, end = 0.9, alpha = 1))
rgb.palette <- colorRampPalette(colours, space = "Lab")

# Change ticks in levelplot
x.scale <- list(at=seq(from = 0, to = 100, length.out = 6))
y.scale <- list(at=seq(from = 0, to = 100, length.out = 6))

# draw payoff heatmap
heat = levelplot(z, col.regions=rgb.palette(1000), 
                 scales=list(x=x.scale, y=y.scale),
                 main="Row Player's Payoff Table",
                 xlab="column action", 
                 ylab="row action" ,
                 cuts = 1000)

# print heatmap
title = "Heatmap for 8002 NE"
file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/data_summary/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 600, height = 600)
print(heat)
dev.off()

# define row and column mixture
row_mix = seq(from = 0, to = 1, length.out = 101)
column_mix = seq(from = 0, to = 1, length.out = 101)

# draw row player best response
row_payoff_1 = 800*column_mix
row_payoff_0 = 200*(1-column_mix)
row_BR = rep(0, length(row_mix))
for (i in 1:length(row_BR)){
  if (row_payoff_1[i] > row_payoff_0[i]){row_BR[i] = 1}
  if (row_payoff_1[i] < row_payoff_0[i]){row_BR[i] = 0}
  if (row_payoff_1[i] == row_payoff_0[i]){row_BR[i] = 0.5}
}

BRrow = ggplot() + geom_line(mapping = aes(x=column_mix, y=row_BR), show.legend = FALSE, size=1.5) +
  scale_x_continuous(name = 'column mixture', limits = c(0:1)) +
  scale_y_continuous(name = 'row best response') + theme_bw()

# draw column player best response
column_payoff_1 = 200*(1-row_mix)
column_payoff_0 = 200*row_mix
column_BR = rep(0, length(column_mix))
for (i in 1:length(column_BR)){
  if (column_payoff_1[i] > column_payoff_0[i]){column_BR[i] = 1}
  if (column_payoff_1[i] < column_payoff_0[i]){column_BR[i] = 0}
  if (column_payoff_1[i] == column_payoff_0[i]){column_BR[i] = 0.2}
}

BRcolumn = ggplot() + geom_line(mapping = aes(x=row_mix, y=column_BR), show.legend = FALSE, size=1.5) +
  scale_x_continuous(name = 'row mixture', limits = c(0:1)) +
  scale_y_continuous(name = 'column best response') + theme_bw() + coord_flip()

# print BR figures
title = "BR diagram for 8002 NE"
file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/data_summary/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 800, height = 800)

grid.newpage()
pushViewport(viewport(layout = grid.layout(4,4)))
vplayout <- function(x,y){
  viewport(layout.pos.row = x, layout.pos.col = y)
}

print(BRrow, vp = vplayout(1,1:3))
print(BRcolumn, vp = vplayout(2:4,4))

dev.off()


##########Descriptive figure (not used): payoff heatmap and MaxMin diagram for AMPa##########
# For the maxmin diagram, do it your favorite way, or if you don't have one, 
# write row player's payoff function as f(p,q), where p= col mixing prob of L 
# and q = row mixing prob of T. Then max-min diagram has q as horizontal axis 
# and graphs f(1,q) and f(0,q), and their minimum --- a piecewise linear function 
# maximized at  q = .2, where f(1,q)=f(0,q)=1.6.

# define row and column mixture
row_mix = seq(from = 0, to = 1, length.out = 101)
column_mix = seq(from = 0, to = 1, length.out = 101)

# draw row player MaxMin
row_payoff_1 = 800*row_mix
row_payoff_0 = 200*(1-row_mix)
row_MM = rep(0, length(row_mix))
for (i in 1:length(row_MM)){
  if (row_payoff_1[i] > row_payoff_0[i]){row_MM[i] = row_payoff_0[i]}
  if (row_payoff_1[i] < row_payoff_0[i]){row_MM[i] = row_payoff_1[i]}
  if (row_payoff_1[i] == row_payoff_0[i]){row_MM[i] = row_payoff_1[i]}
}

MMrow = ggplot() + geom_line(mapping = aes(x=row_mix, y=row_MM), show.legend = FALSE, size=1.5) +
  scale_x_continuous(name = 'row mixture', limits = c(0:1)) +
  scale_y_continuous(name = 'row MaxMin') + theme_bw() + coord_flip()

# draw column player MaxMin
column_payoff_1 = 200*(1-column_mix)
column_payoff_0 = 200*column_mix
column_MM = rep(0, length(column_mix))
for (i in 1:length(column_MM)){
  if (column_payoff_1[i] > column_payoff_0[i]){column_MM[i] = column_payoff_0[i]}
  if (column_payoff_1[i] < column_payoff_0[i]){column_MM[i] = column_payoff_1[i]}
  if (column_payoff_1[i] == column_payoff_0[i]){column_MM[i] = column_payoff_1[i]}
}

MMcolumn = ggplot() + geom_line(mapping = aes(x=column_mix, y=column_MM), show.legend = FALSE, size=1.5) +
  scale_x_continuous(name = 'column mixture', limits = c(0:1)) +
  scale_y_continuous(name = 'column MaxMin') + theme_bw()

# print MaxMin figures
title = "MaxMin diagram for 8002 NE"
file = paste("/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/production/data_summary/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 800, height = 800)

grid.newpage()
pushViewport(viewport(layout = grid.layout(4,4)))
vplayout <- function(x,y){
  viewport(layout.pos.row = x, layout.pos.col = y)
}

print(MMcolumn, vp = vplayout(1,1:3))
print(MMrow, vp = vplayout(2:4,4))

dev.off()

