##########Build the simulation function##########
learning_simulation = function(iteration, row_speed, column_speed, pure_indicator, step){
  
  # set simulation parameters
  p1_mix = runif(1, 0, 1)
  p2_mix = runif(1, 0, 1)
  
  if (pure_indicator == 1){
    p1_strategy = ifelse(runif(1,0,1)<=p1_mix, 1, 0)
    p2_strategy = ifelse(runif(1,0,1)<=p2_mix, 1, 0)
  }
  if (pure_indicator == 0){
    p1_strategy = p1_mix
    p2_strategy = p2_mix
  }
  
  payoff1Aa = 800 
  payoff1Ab = 0
  payoff1Ba = 0
  payoff1Bb = 200
  payoff2Aa = 0 
  payoff2Ab = 200
  payoff2Ba = 200
  payoff2Bb = 0
  
  # create data container
  simulation_data = matrix(0, nrow = iteration, ncol = 8)
  colnames(simulation_data) = c('iteration', 'row_strategy', 'column_strategy', 
                                'row_next', 'column_next', 'p1_time_average', 'p2_time_average',
                                'to_ne')
  
  size = 1
  i = 1
  while (i <= iteration){
    
    # calculate current payoff
    p1_u = payoff1Aa*p1_strategy*p2_strategy + payoff1Ab*p1_strategy*(1-p2_strategy) + payoff1Ba*(1-p1_strategy)*p2_strategy + payoff1Bb*(1-p1_strategy)*(1-p2_strategy)
    p2_u = payoff2Aa*p1_strategy*p2_strategy + payoff2Ab*p1_strategy*(1-p2_strategy) + payoff2Ba*(1-p1_strategy)*p2_strategy + payoff2Bb*(1-p1_strategy)*(1-p2_strategy)
    
    # row player belief update
    p1_uhat_a1 = payoff1Aa*p2_strategy + payoff1Ab*(1-p2_strategy)
    p1_uhat_a0 = payoff1Ba*p2_strategy + payoff1Bb*(1-p2_strategy)
    p1_ahat = ifelse(p1_uhat_a1 > p1_uhat_a0, 1, ifelse(p1_uhat_a1 < p1_uhat_a0, 0, p1_strategy))
    p1_uhat = payoff1Aa*p1_ahat*p2_strategy + payoff1Ab*p1_ahat*(1-p2_strategy) + payoff1Ba*(1-p1_ahat)*p2_strategy + payoff1Bb*(1-p1_ahat)*(1-p2_strategy)
    p1_regret = (p1_uhat - p1_u) / 800
    p1_direction = p1_ahat - p1_strategy
    p1_sign = ifelse(p1_direction > 0, 1, ifelse(p1_direction < 0, -1, 0))
    p1_regret_sign = p1_regret * p1_sign
    
    # column player belief update
    p2_uhat_a1 = payoff2Aa*p1_strategy + payoff2Ba*(1-p1_strategy)
    p2_uhat_a0 = payoff2Ab*p1_strategy + payoff2Bb*(1-p1_strategy)
    p2_ahat = ifelse(p2_uhat_a1 > p2_uhat_a0, 1, ifelse(p2_uhat_a1 < p2_uhat_a0, 0, p2_strategy))
    p2_uhat = payoff2Aa*p1_strategy*p2_ahat + payoff2Ab*p1_strategy*(1-p2_ahat) + payoff2Ba*(1-p1_strategy)*p2_ahat + payoff2Bb*(1-p1_strategy)*(1-p2_ahat)
    p2_regret = (p2_uhat - p2_u) / 200
    p2_direction = p2_ahat - p2_strategy
    p2_sign = ifelse(p2_direction > 0, 1, ifelse(p2_direction < 0, -1, 0))
    p2_regret_sign = p2_regret * p2_sign
    
    # record data
    simulation_data[i,1] = i
    simulation_data[i,2] = p1_strategy
    simulation_data[i,3] = p2_strategy
    simulation_data[i,8] = sqrt((p1_strategy - 0.5)^2 + (p2_strategy - 0.2)^2)
    
    # update player strategies and realizations
    p1_mix = p1_mix + size * row_speed * p1_regret_sign
    p1_mix = max(p1_mix, 0)
    p1_mix = min(p1_mix, 1)
    p2_mix = p2_mix + size * column_speed * p2_regret_sign
    p2_mix = max(p2_mix, 0)
    p2_mix = min(p2_mix, 1)
    
    if (pure_indicator == 1){
      p1_strategy = ifelse(runif(1,0,1)<=p1_mix, 1, 0)
      p2_strategy = ifelse(runif(1,0,1)<=p2_mix, 1, 0)
    }
    if (pure_indicator == 0){
      p1_strategy = p1_mix
      p2_strategy = p2_mix
    }
    
    # update column 4-7
    simulation_data[i,4] = p1_strategy
    simulation_data[i,5] = p2_strategy
    simulation_data[i,6] = mean(simulation_data[1:i,4])
    simulation_data[i,7] = mean(simulation_data[1:i,5])
    
    # if (i == 1){
    #   simulation_data[i,6] = simulation_data[i,4]
    #   simulation_data[i,7] = simulation_data[i,5]
    # }
    # else{
    #   simulation_data[i,6] = 0.8*simulation_data[i,4] + 0.2*simulation_data[i-1,6]
    #   simulation_data[i,7] = 0.8*simulation_data[i,5] + 0.2*simulation_data[i-1,7]
    # }
    
    i = i + 1
    size = size * step
  }
  
  # return dataset
  return(simulation_data)
}

learning_simulation2 = function(iteration, row_speed, column_speed, pure_indicator, step,
                                       p1_initial, p2_initial){
  
  # set simulation parameters
  p1_mix = p1_initial
  p2_mix = p2_initial
  
  if (pure_indicator == 1){
    p1_strategy = ifelse(runif(1,0,1)<=p1_mix, 1, 0)
    p2_strategy = ifelse(runif(1,0,1)<=p2_mix, 1, 0)
  }
  if (pure_indicator == 0){
    p1_strategy = p1_mix
    p2_strategy = p2_mix
  }
  
  payoff1Aa = 800 
  payoff1Ab = 0
  payoff1Ba = 0
  payoff1Bb = 200
  payoff2Aa = 0 
  payoff2Ab = 200
  payoff2Ba = 200
  payoff2Bb = 0
  
  # create data container
  simulation_data = matrix(0, nrow = iteration, ncol = 8)
  colnames(simulation_data) = c('iteration', 'row_strategy', 'column_strategy', 
                                'row_next', 'column_next', 'p1_time_average', 'p2_time_average',
                                'to_ne')
  
  size = 1
  i = 1
  while (i <= iteration){
    
    # calculate current payoff
    p1_u = payoff1Aa*p1_strategy*p2_strategy + payoff1Ab*p1_strategy*(1-p2_strategy) + payoff1Ba*(1-p1_strategy)*p2_strategy + payoff1Bb*(1-p1_strategy)*(1-p2_strategy)
    p2_u = payoff2Aa*p1_strategy*p2_strategy + payoff2Ab*p1_strategy*(1-p2_strategy) + payoff2Ba*(1-p1_strategy)*p2_strategy + payoff2Bb*(1-p1_strategy)*(1-p2_strategy)
    
    # row player belief update
    p1_uhat_a1 = payoff1Aa*p2_strategy + payoff1Ab*(1-p2_strategy)
    p1_uhat_a0 = payoff1Ba*p2_strategy + payoff1Bb*(1-p2_strategy)
    p1_ahat = ifelse(p1_uhat_a1 > p1_uhat_a0, 1, ifelse(p1_uhat_a1 < p1_uhat_a0, 0, p1_strategy))
    p1_uhat = payoff1Aa*p1_ahat*p2_strategy + payoff1Ab*p1_ahat*(1-p2_strategy) + payoff1Ba*(1-p1_ahat)*p2_strategy + payoff1Bb*(1-p1_ahat)*(1-p2_strategy)
    p1_regret = (p1_uhat - p1_u) / 800
    p1_direction = p1_ahat - p1_strategy
    p1_sign = ifelse(p1_direction > 0, 1, ifelse(p1_direction < 0, -1, 0))
    p1_regret_sign = p1_regret * p1_sign
    
    # column player belief update
    p2_uhat_a1 = payoff2Aa*p1_strategy + payoff2Ba*(1-p1_strategy)
    p2_uhat_a0 = payoff2Ab*p1_strategy + payoff2Bb*(1-p1_strategy)
    p2_ahat = ifelse(p2_uhat_a1 > p2_uhat_a0, 1, ifelse(p2_uhat_a1 < p2_uhat_a0, 0, p2_strategy))
    p2_uhat = payoff2Aa*p1_strategy*p2_ahat + payoff2Ab*p1_strategy*(1-p2_ahat) + payoff2Ba*(1-p1_strategy)*p2_ahat + payoff2Bb*(1-p1_strategy)*(1-p2_ahat)
    p2_regret = (p2_uhat - p2_u) / 200
    p2_direction = p2_ahat - p2_strategy
    p2_sign = ifelse(p2_direction > 0, 1, ifelse(p2_direction < 0, -1, 0))
    p2_regret_sign = p2_regret * p2_sign
    
    # record data
    simulation_data[i,1] = i
    simulation_data[i,2] = p1_strategy
    simulation_data[i,3] = p2_strategy
    simulation_data[i,8] = sqrt((p1_strategy - 0.5)^2 + (p2_strategy - 0.2)^2)
    
    # update player strategies and realizations
    p1_mix = p1_mix + size * row_speed * p1_regret_sign
    p1_mix = max(p1_mix, 0)
    p1_mix = min(p1_mix, 1)
    p2_mix = p2_mix + size * column_speed * p2_regret_sign
    p2_mix = max(p2_mix, 0)
    p2_mix = min(p2_mix, 1)
    
    if (pure_indicator == 1){
      p1_strategy = ifelse(runif(1,0,1)<=p1_mix, 1, 0)
      p2_strategy = ifelse(runif(1,0,1)<=p2_mix, 1, 0)
    }
    if (pure_indicator == 0){
      p1_strategy = p1_mix
      p2_strategy = p2_mix
    }
    
    # update column 4-7
    simulation_data[i,4] = p1_strategy
    simulation_data[i,5] = p2_strategy
    simulation_data[i,6] = mean(simulation_data[1:i,4])
    simulation_data[i,7] = mean(simulation_data[1:i,5])
    
    # if (i == 1){
    #   simulation_data[i,6] = simulation_data[i,4]
    #   simulation_data[i,7] = simulation_data[i,5]
    # }
    # else{
    #   simulation_data[i,6] = 0.8*simulation_data[i,4] + 0.2*simulation_data[i-1,6]
    #   simulation_data[i,7] = 0.8*simulation_data[i,5] + 0.2*simulation_data[i-1,7]
    # }
    
    i = i + 1
    size = size * step
  }
  
  # return dataset
  return(simulation_data)
}

learning_simulation_AMPb = function(iteration, row_speed, column_speed, pure_indicator, step){
  
  # set simulation parameters
  p1_mix = runif(1, 0, 1)
  p2_mix = runif(1, 0, 1)
  
  if (pure_indicator == 1){
    p1_strategy = ifelse(runif(1,0,1)<=p1_mix, 1, 0)
    p2_strategy = ifelse(runif(1,0,1)<=p2_mix, 1, 0)
  }
  if (pure_indicator == 0){
    p1_strategy = p1_mix
    p2_strategy = p2_mix
  }
  
  payoff1Aa = 300
  payoff1Ab = 100
  payoff1Ba = 100
  payoff1Bb = 700
  payoff2Aa = 100
  payoff2Ab = 300
  payoff2Ba = 200
  payoff2Bb = 100
  
  # create data container
  simulation_data = matrix(0, nrow = iteration, ncol = 8)
  colnames(simulation_data) = c('iteration', 'row_strategy', 'column_strategy', 
                                'row_next', 'column_next', 'p1_time_average', 'p2_time_average',
                                'to_ne')
  
  size = 1
  i = 1
  while (i <= iteration){
    
    # calculate current payoff
    p1_u = payoff1Aa*p1_strategy*p2_strategy + payoff1Ab*p1_strategy*(1-p2_strategy) + payoff1Ba*(1-p1_strategy)*p2_strategy + payoff1Bb*(1-p1_strategy)*(1-p2_strategy)
    p2_u = payoff2Aa*p1_strategy*p2_strategy + payoff2Ab*p1_strategy*(1-p2_strategy) + payoff2Ba*(1-p1_strategy)*p2_strategy + payoff2Bb*(1-p1_strategy)*(1-p2_strategy)
    
    # row player belief update
    p1_uhat_a1 = payoff1Aa*p2_strategy + payoff1Ab*(1-p2_strategy)
    p1_uhat_a0 = payoff1Ba*p2_strategy + payoff1Bb*(1-p2_strategy)
    p1_ahat = ifelse(p1_uhat_a1 > p1_uhat_a0, 1, ifelse(p1_uhat_a1 < p1_uhat_a0, 0, p1_strategy))
    p1_uhat = payoff1Aa*p1_ahat*p2_strategy + payoff1Ab*p1_ahat*(1-p2_strategy) + payoff1Ba*(1-p1_ahat)*p2_strategy + payoff1Bb*(1-p1_ahat)*(1-p2_strategy)
    p1_regret = (p1_uhat - p1_u) / 700
    p1_direction = p1_ahat - p1_strategy
    p1_sign = ifelse(p1_direction > 0, 1, ifelse(p1_direction < 0, -1, 0))
    p1_regret_sign = p1_regret * p1_sign
    
    # column player belief update
    p2_uhat_a1 = payoff2Aa*p1_strategy + payoff2Ba*(1-p1_strategy)
    p2_uhat_a0 = payoff2Ab*p1_strategy + payoff2Bb*(1-p1_strategy)
    p2_ahat = ifelse(p2_uhat_a1 > p2_uhat_a0, 1, ifelse(p2_uhat_a1 < p2_uhat_a0, 0, p2_strategy))
    p2_uhat = payoff2Aa*p1_strategy*p2_ahat + payoff2Ab*p1_strategy*(1-p2_ahat) + payoff2Ba*(1-p1_strategy)*p2_ahat + payoff2Bb*(1-p1_strategy)*(1-p2_ahat)
    p2_regret = (p2_uhat - p2_u) / 300
    p2_direction = p2_ahat - p2_strategy
    p2_sign = ifelse(p2_direction > 0, 1, ifelse(p2_direction < 0, -1, 0))
    p2_regret_sign = p2_regret * p2_sign
    
    # record data
    simulation_data[i,1] = i
    simulation_data[i,2] = p1_strategy
    simulation_data[i,3] = p2_strategy
    simulation_data[i,8] = sqrt((p1_strategy - 0.33)^2 + (p2_strategy - 0.75)^2)
    
    # update player strategies and realizations
    p1_mix = p1_mix + size * row_speed * p1_regret_sign
    p1_mix = max(p1_mix, 0)
    p1_mix = min(p1_mix, 1)
    p2_mix = p2_mix + size * column_speed * p2_regret_sign
    p2_mix = max(p2_mix, 0)
    p2_mix = min(p2_mix, 1)
    
    if (pure_indicator == 1){
      p1_strategy = ifelse(runif(1,0,1)<=p1_mix, 1, 0)
      p2_strategy = ifelse(runif(1,0,1)<=p2_mix, 1, 0)
    }
    if (pure_indicator == 0){
      p1_strategy = p1_mix
      p2_strategy = p2_mix
    }
    
    # update column 4-7
    simulation_data[i,4] = p1_strategy
    simulation_data[i,5] = p2_strategy
    simulation_data[i,6] = mean(simulation_data[1:i,4])
    simulation_data[i,7] = mean(simulation_data[1:i,5])
    
    # if (i == 1){
    #   simulation_data[i,6] = simulation_data[i,4]
    #   simulation_data[i,7] = simulation_data[i,5]
    # }
    # else{
    #   simulation_data[i,6] = 0.8*simulation_data[i,4] + 0.2*simulation_data[i-1,6]
    #   simulation_data[i,7] = 0.8*simulation_data[i,5] + 0.2*simulation_data[i-1,7]
    # }
    
    i = i + 1
    size = size * step
  }
  
  # return dataset
  return(simulation_data)
}

learning_simulation_mm = function(iteration, row_speed, column_speed, pure_indicator, group_size){
  
  # set simulation parameters
  p1_mix = rep(0, group_size)
  p2_mix = rep(0, group_size)
  p1_strategy = p1_mix
  p2_strategy = p2_mix
  
  for (j in 1:group_size){
    p1_mix[j] = runif(1, 0, 1)
    p2_mix[j] = runif(1, 0, 1)
    
    if (pure_indicator == 1){
      p1_strategy[j] = ifelse(runif(1,0,1)<=p1_mix[j], 1, 0)
      p2_strategy[j] = ifelse(runif(1,0,1)<=p2_mix[j], 1, 0)
    }
    if (pure_indicator == 0){
      p1_strategy[j] = p1_mix[j]
      p2_strategy[j] = p2_mix[j]
    }
  }
  
  p1_average = mean(p1_strategy)
  p2_average = mean(p2_strategy)
  
  payoff1Aa = 800 
  payoff1Ab = 0
  payoff1Ba = 0
  payoff1Bb = 200
  payoff2Aa = 0 
  payoff2Ab = 200
  payoff2Ba = 200
  payoff2Bb = 0
  
  # create data container
  simulation_data = matrix(0, nrow = iteration, ncol = 8)
  colnames(simulation_data) = c('iteration', 'row_strategy', 'column_strategy', 
                                'row_next', 'column_next', 'p1_time_average', 'p2_time_average',
                                'to_ne')
  
  i = 1
  while (i <= iteration){
    
    # create data vector
    p1_u = rep(0, group_size)
    p2_u = rep(0, group_size)
    p1_uhat_a1 = rep(0, group_size)
    p1_uhat_a0 = rep(0, group_size)
    p1_ahat = rep(0, group_size)
    p1_uhat = rep(0, group_size)
    p1_regret = rep(0, group_size)
    p1_direction = rep(0, group_size)
    p1_sign = rep(0, group_size)
    p1_regret_sign = rep(0, group_size)
    p2_uhat_a1 = rep(0, group_size)
    p2_uhat_a0 = rep(0, group_size)
    p2_ahat = rep(0, group_size)
    p2_uhat = rep(0, group_size)
    p2_regret = rep(0, group_size)
    p2_direction = rep(0, group_size)
    p2_sign = rep(0, group_size)
    p2_regret_sign = rep(0, group_size)
    
    # loop over players
    for (j in 1:group_size){
      
      # calculate current payoff
      p1_u[j] = payoff1Aa*p1_strategy[j]*p2_average + payoff1Ab*p1_strategy[j]*(1-p2_average) + payoff1Ba*(1-p1_strategy[j])*p2_average + payoff1Bb*(1-p1_strategy[j])*(1-p2_average)
      p2_u[j] = payoff2Aa*p1_average*p2_strategy[j] + payoff2Ab*p1_average*(1-p2_strategy[j]) + payoff2Ba*(1-p1_average)*p2_strategy[j] + payoff2Bb*(1-p1_average)*(1-p2_strategy[j])
      
      # row player belief update
      p1_uhat_a1[j] = payoff1Aa*p2_average + payoff1Ab*(1-p2_average)
      p1_uhat_a0[j] = payoff1Ba*p2_average + payoff1Bb*(1-p2_average)
      p1_ahat[j] = ifelse(p1_uhat_a1[j] > p1_uhat_a0[j], 1, ifelse(p1_uhat_a1[j] < p1_uhat_a0[j], 0, p1_strategy[j]))
      p1_uhat[j] = payoff1Aa*p1_ahat[j]*p2_average + payoff1Ab*p1_ahat[j]*(1-p2_average) + payoff1Ba*(1-p1_ahat[j])*p2_average + payoff1Bb*(1-p1_ahat[j])*(1-p2_average)
      p1_regret[j] = (p1_uhat[j] - p1_u[j]) / 800
      p1_direction[j] = p1_ahat[j] - p1_strategy[j]
      p1_sign[j] = ifelse(p1_direction[j] > 0, 1, ifelse(p1_direction[j] < 0, -1, 0))
      p1_regret_sign[j] = p1_regret[j] * p1_sign[j]
      
      # column player belief update
      p2_uhat_a1[j] = payoff2Aa*p1_average + payoff2Ba*(1-p1_average)
      p2_uhat_a0[j] = payoff2Ab*p1_average + payoff2Bb*(1-p1_average)
      p2_ahat[j] = ifelse(p2_uhat_a1[j] > p2_uhat_a0[j], 1, ifelse(p2_uhat_a1[j] < p2_uhat_a0[j], 0, p2_strategy[j]))
      p2_uhat[j] = payoff2Aa*p1_average*p2_ahat[j] + payoff2Ab*p1_average*(1-p2_ahat[j]) + payoff2Ba*(1-p1_average)*p2_ahat[j] + payoff2Bb*(1-p1_average)*(1-p2_ahat[j])
      p2_regret[j] = (p2_uhat[j] - p2_u[j]) / 200
      p2_direction[j] = p2_ahat[j] - p2_strategy[j]
      p2_sign[j] = ifelse(p2_direction[j] > 0, 1, ifelse(p2_direction[j] < 0, -1, 0))
      p2_regret_sign[j] = p2_regret[j] * p2_sign[j]
    }
    
    # record data
    simulation_data[i,1] = i
    simulation_data[i,2] = p1_average
    simulation_data[i,3] = p2_average
    simulation_data[i,8] = sqrt((p1_average - 0.5)^2 + (p2_average - 0.2)^2)
    
    # update player strategies and realizations
    for (j in 1:group_size){
      p1_mix[j] = p1_mix[j] + row_speed * p1_regret_sign[j]
      p1_mix[j] = max(p1_mix[j], 0)
      p1_mix[j] = min(p1_mix[j], 1)
      p2_mix[j] = p2_mix[j] + column_speed * p2_regret_sign[j]
      p2_mix[j] = max(p2_mix[j], 0)
      p2_mix[j] = min(p2_mix[j], 1)
      
      if (pure_indicator == 1){
        p1_strategy[j] = ifelse(runif(1,0,1)<=p1_mix[j], 1, 0)
        p2_strategy[j] = ifelse(runif(1,0,1)<=p2_mix[j], 1, 0)
      }
      if (pure_indicator == 0){
        p1_strategy[j] = p1_mix[j]
        p2_strategy[j] = p2_mix[j]
      }
    }
    
    p1_average = mean(p1_strategy)
    p2_average = mean(p2_strategy)
    
    simulation_data[i,4] = p1_average
    simulation_data[i,5] = p2_average
    simulation_data[i,6] = mean(simulation_data[1:i,4])
    simulation_data[i,7] = mean(simulation_data[1:i,5])
    
    i = i + 1
  }
  
  # return dataset
  return(simulation_data)
}


##########Figure: Fitted regret-based model simulation RP dynamics##########
# run the simulation
title = paste('Simulation3D AMPa rp P')
file = paste("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/writeup/figs/sims/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 500, height = 500)

# store time average
p1_average = c()
p2_average = c()
sims = 50
iteration = 1000
row_speed = 0.19
column_speed = 0.15
pure_indicator = 1
step = 1

for (j in 1:sims){
  simulation_data = data.frame(learning_simulation(iteration, row_speed, column_speed, pure_indicator, step))
  par(new=TRUE)

  # draw 3D plots
  if (j == 1){
    # 3D plot by NE
    plot3D::lines3D(simulation_data$row_next, simulation_data$column_next, iteration-simulation_data$iteration, col='blue',
                    xlab='row strategy', xlim=c(0:1),
                    ylab='column strategy', ylim=c(0:1),
                    zlab='time left',
                    main = 'simulation random pairwise',
                    theta=20, phi=30, r=2, d=1, bty='g')
    plot3D::lines3D(rep(0.5, iteration), rep(0.2, iteration), iteration-simulation_data$iteration, col='black', add=TRUE)
    plot3D::lines3D(rep(0.5, iteration), rep(0.5, iteration), iteration-simulation_data$iteration, col='green', add=TRUE)
    plot3D::lines3D(rep(0.2, iteration), rep(0.5, iteration), iteration-simulation_data$iteration, col='red', add=TRUE)

  }
  else{
    # 3D plot by NE
    plot3D::lines3D(simulation_data$row_next, simulation_data$column_next, iteration-simulation_data$iteration, col='blue',
                    xlab='row strategy', xlim=c(0:1),
                    ylab='column strategy', ylim=c(0:1),
                    zlab='time left',
                    theta=20, phi=30, r=2, d=1, bty='g', add=TRUE)
    plot3D::lines3D(rep(0.5, iteration), rep(0.2, iteration), iteration-simulation_data$iteration, col='black', add=TRUE)
    plot3D::lines3D(rep(0.5, iteration), rep(0.5, iteration), iteration-simulation_data$iteration, col='green', add=TRUE)
    plot3D::lines3D(rep(0.2, iteration), rep(0.5, iteration), iteration-simulation_data$iteration, col='red', add=TRUE)
  }
  
  # # try 2D plots
  # if (j == 1){
  #   plot(simulation_data$row_next, simulation_data$column_next,
  #        xlab='row strategy', xlim=c(0:1),
  #        ylab='column strategy', ylim=c(0:1),
  #        main = 'simulation random pairwise', type = 'l', cex = 1.5)
  # }
  # else{
  #   plot(simulation_data$row_next, simulation_data$column_next,
  #        xlab='row strategy', xlim=c(0:1),
  #        ylab='column strategy', ylim=c(0:1), type = 'l', cex = 1.5)
  # }
  
  # add time average
  p1_average = cbind(p1_average, simulation_data$p1_time_average[iteration])
  p2_average = cbind(p2_average, simulation_data$p2_time_average[iteration])
}

# text(0.5,0.2,'NE',cex=1,pos=3,col="blue")
# text(0.2,0.5,'MM',cex=1,pos=3,col="red")

dev.off()

print(c(mean(p1_average), mean(p2_average)))


##########Figure: Fitted regret-based model simulation MM dynamics##########
# run the simulation
title = paste('Simulation3D AMPa mm P')
file = paste("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/writeup/figs/sims/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 500, height = 500)

# store time average
p1pop_average = c()
p2pop_average = c()
sims = 1
iteration = 1000
row_speed = 0.19
column_speed = 0.15
pure_indicator = 1
group_size = 5

for (j in 1:sims){
  simulation_data = data.frame(learning_simulation_mm(iteration, row_speed, column_speed, pure_indicator, group_size))
  
  par(new=TRUE)
  
  if (j == 1){
    # 3D plot by NE
    plot3D::lines3D(simulation_data$row_next, simulation_data$column_next, iteration-simulation_data$iteration, col='blue',
                    xlab='row strategy', xlim=c(0:1),
                    ylab='column strategy', ylim=c(0:1),
                    zlab='time left',
                    main = 'simulation mean matching',
                    theta=20, phi=30, r=2, d=1, bty='g')
    plot3D::lines3D(rep(0.5, iteration), rep(0.2, iteration), iteration-simulation_data$iteration, col='black', add=TRUE)
    plot3D::lines3D(rep(0.5, iteration), rep(0.5, iteration), iteration-simulation_data$iteration, col='green', add=TRUE)
    plot3D::lines3D(rep(0.2, iteration), rep(0.5, iteration), iteration-simulation_data$iteration, col='red', add=TRUE)

  }
  else{
    # 3D plot by NE
    plot3D::lines3D(simulation_data$row_strategy, simulation_data$column_strategy, iteration-simulation_data$iteration, col='blue',
                    xlab='row strategy', xlim=c(0:1),
                    ylab='column strategy', ylim=c(0:1),
                    zlab='time left',
                    theta=20, phi=30, r=2, d=1, bty='g', add=TRUE)
    plot3D::lines3D(rep(0.5, iteration), rep(0.2, iteration), iteration-simulation_data$iteration, col='black', add=TRUE)
    plot3D::lines3D(rep(0.5, iteration), rep(0.5, iteration), iteration-simulation_data$iteration, col='green', add=TRUE)
    plot3D::lines3D(rep(0.2, iteration), rep(0.5, iteration), iteration-simulation_data$iteration, col='red', add=TRUE)
  }
  
  # # try 2D plots
  # if (j == 1){
  #   plot(simulation_data$row_next, simulation_data$column_next,
  #        xlab='row strategy', xlim=c(0:1),
  #        ylab='column strategy', ylim=c(0:1),
  #        main = 'simulation mean matching', type = 'l', cex = 1.5)
  # }
  # else{
  #   plot(simulation_data$row_next, simulation_data$column_next,
  #        xlab='row strategy', xlim=c(0:1),
  #        ylab='column strategy', ylim=c(0:1), type = 'l', cex = 1.5)
  # }
  
  # add time average
  p1pop_average = cbind(p1pop_average, simulation_data$p1_time_average[iteration])
  p2pop_average = cbind(p2pop_average, simulation_data$p2_time_average[iteration])
}

# text(0.5,0.2,'NE',cex=1,pos=3,col="blue")
# text(0.2,0.5,'MM',cex=1,pos=3,col="red")

dev.off()

print(c(mean(p1pop_average), mean(p2pop_average)))


##########Figure: Limited cycles simulation##########
# define figure title
title = paste('limit_cycle_low_beta')
file = paste("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/writeup/figs/sims/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 800, height = 400)

# set simulation parameters
iteration = 1500
row_speed = 0.1
column_speed = 0.24
pure_indicator = 0
step = 1
sims = 20

for (j in 1:sims){
  ## start outside the circle
  p1_initial = runif(1, min = 0, max = 1)
  p2_initial = runif(1, min = 0, max = 1)
  
  # draw figure of learning process
  simulation_data = data.frame(learning_simulation2(
    iteration, row_speed, column_speed, pure_indicator, step, p1_initial, p2_initial))
  tripwire_data = subset(simulation_data, column_strategy > 0.2)
  tripwire_data = subset(tripwire_data, (row_strategy - 0.5)*(row_next - 0.5) <= 0)
  
  if (j == 1){
    plot(tripwire_data$to_ne+0.001, type = 'l', col = 'blue',
         xlab = 'number of cycles', ylab = 'Deviation to NE', ylim = c(0,0.7),
         main = 'Limit Cycle with Poincare Section (a=0.5, b>0.2) and beta=0.1/0.24',
         cex.main = 1.5, cex.lab = 1.5)
  }
  else{
    lines(tripwire_data$to_ne+0.001, type = 'l', col = 'blue', ylim = c(0,0.7))
  }
  
  ## start inside the circle
  p1_initial = runif(1, min = 0.49, max = 0.51)
  p2_initial = runif(1, min = 0.19, max = 0.21)

  # draw figure of learning process
  simulation_data = data.frame(learning_simulation2(
    iteration, row_speed, column_speed, pure_indicator, step, p1_initial, p2_initial))
  tripwire_data = subset(simulation_data, column_strategy > 0.2)
  tripwire_data = subset(tripwire_data, (row_strategy - 0.5)*(row_next - 0.5) <= 0)
  
  lines(tripwire_data$to_ne, type = 'l', col = 'red', ylim = c(0,0.7))
}

dev.off()

rm(simulation_data, tripwire_data)


##########Table: Transition probability matrix for pure###########
# load packages
library(dplyr)

# set up parameters and build the simulation dataset
iteration = 1000
row_speed = 0.19
column_speed = 0.15
pure_indicator = 1
step = 1
group_size = 5
sims = 50

# set up the transition probability matrix
transition_prob = list()
tpm = matrix(0, nrow = 4, ncol = 5)
rownames(tpm) = c('(A,a) at t', '(B,a) at t', '(B,b) at t', '(A,b) at t')
colnames(tpm) = c('(A,a) at t+1', '(B,a) at t+1', '(B,b) at t+1', '(A,b) at t+1', '# of obs')

# loop over simulations
for (i in 1:sims){
  
  # select game
  #df = data.frame(learning_simulation(iteration, row_speed, column_speed, pure_indicator, step))
  df = data.frame(learning_simulation_AMPb(iteration, row_speed, column_speed, pure_indicator, step))
  
  # set up the columns for strategy profiles
  df = df %>% mutate(
    type = NA,
    type_next = NA)
  for(m in 1:iteration){
    # current profile
    if(df$row_strategy[m]==1 & df$column_strategy[m]==1){df$type[m]='(A,a)'}
    if(df$row_strategy[m]==0 & df$column_strategy[m]==0){df$type[m]='(B,b)'}
    if(df$row_strategy[m]==0 & df$column_strategy[m]==1){df$type[m]='(B,a)'}
    if(df$row_strategy[m]==1 & df$column_strategy[m]==0){df$type[m]='(A,b)'}
    # next period profile
    if(df$row_next[m]==1 & df$column_next[m]==1){df$type_next[m]='(A,a)'}
    if(df$row_next[m]==0 & df$column_next[m]==0){df$type_next[m]='(B,b)'}
    if(df$row_next[m]==0 & df$column_next[m]==1){df$type_next[m]='(B,a)'}
    if(df$row_next[m]==1 & df$column_next[m]==0){df$type_next[m]='(A,b)'}
  }
  
  # build transition probability matrix
  # create transition matrix
  transition = matrix(0, nrow = 4, ncol = 5)
  rownames(transition) = c('(A,a) at t', '(B,a) at t', '(B,b) at t', '(A,b) at t')
  colnames(transition) = c('(A,a) at t+1', '(B,a) at t+1', '(B,b) at t+1', '(A,b) at t+1', '# of obs')
  
  # loop over observations
  for (j in 1:iteration){
    if (df$type[j] == '(A,a)'){
      if (df$type_next[j] == '(A,a)'){transition[1,1] = transition[1,1] + 1}
      if (df$type_next[j] == '(B,a)'){transition[1,2] = transition[1,2] + 1}
      if (df$type_next[j] == '(B,b)'){transition[1,3] = transition[1,3] + 1}
      if (df$type_next[j] == '(A,b)'){transition[1,4] = transition[1,4] + 1}
    }
    
    if (df$type[j] == '(B,a)'){
      if (df$type_next[j] == '(A,a)'){transition[2,1] = transition[2,1] + 1}
      if (df$type_next[j] == '(B,a)'){transition[2,2] = transition[2,2] + 1}
      if (df$type_next[j] == '(B,b)'){transition[2,3] = transition[2,3] + 1}
      if (df$type_next[j] == '(A,b)'){transition[2,4] = transition[2,4] + 1}
    }
    
    if (df$type[j] == '(B,b)'){
      if (df$type_next[j] == '(A,a)'){transition[3,1] = transition[3,1] + 1}
      if (df$type_next[j] == '(B,a)'){transition[3,2] = transition[3,2] + 1}
      if (df$type_next[j] == '(B,b)'){transition[3,3] = transition[3,3] + 1}
      if (df$type_next[j] == '(A,b)'){transition[3,4] = transition[3,4] + 1}
    }
    
    if (df$type[j] == '(A,b)'){
      if (df$type_next[j] == '(A,a)'){transition[4,1] = transition[4,1] + 1}
      if (df$type_next[j] == '(B,a)'){transition[4,2] = transition[4,2] + 1}
      if (df$type_next[j] == '(B,b)'){transition[4,3] = transition[4,3] + 1}
      if (df$type_next[j] == '(A,b)'){transition[4,4] = transition[4,4] + 1}
    }
  }
  
  # calculate transition probability
  transition_prob[[i]] = transition
  for (k in 1:4){
    transition_prob[[i]][k,1] = round(transition[k,1] / sum(transition[k,1:4]), 3)
    transition_prob[[i]][k,2] = round(transition[k,2] / sum(transition[k,1:4]), 3)
    transition_prob[[i]][k,3] = round(transition[k,3] / sum(transition[k,1:4]), 3)
    transition_prob[[i]][k,4] = round(transition[k,4] / sum(transition[k,1:4]), 3)
    transition_prob[[i]][k,5] = sum(transition[k,1:4])
  }
  
  # add to the original tpm
  tpm = tpm + transition_prob[[i]]
}

tpm = tpm / sims


##########Figure: Trajectory of time average over beta##########
# set up parameters
iteration = 1500
pure_indicator = 0
step = 1
# set storage dataset
df = data.frame(beta = seq(0.1, 4, 0.01), p1_average = rep(NA, length(seq(0.1, 4, 0.01)))
                , p2_average = rep(NA, length(seq(0.1, 4, 0.01))))

for (i in 1:length(df$beta)){
  row_speed = df$beta[i]
  column_speed = df$beta[i]
  
  simulation_function = data.frame(learning_simulation(iteration, row_speed, column_speed, pure_indicator, step))
  df$p1_average[i] = round(mean(simulation_function$row_next[1000:1500]), digits = 3)
  df$p2_average[i] = round(mean(simulation_function$column_next[1000:1500]), digits = 3)
}

# draw figure
title = paste('time_average_over_beta')
file = paste("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/writeup/figs/sims/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 800, height = 400)

pic = ggplot() +
  geom_line(data = df, mapping = aes(x = beta, y = p1_average), colour = 'blue', size = 1) +
  geom_line(data = df, mapping = aes(x = beta, y = p2_average), colour = 'red', size = 1) +
  geom_smooth(data = df, mapping = aes(x = beta, y = p1_average), stat = 'smooth',
              method = loess, formula = 'y ~ x', colour = 'blue') +
  geom_smooth(data = df, mapping = aes(x = beta, y = p2_average), stat = 'smooth',
              method = loess, formula = 'y ~ x', colour = 'red') +
  geom_hline(aes(yintercept=0.5), colour = 'black', linetype = 'dotted') +
  geom_hline(aes(yintercept=0.2), colour = 'black', linetype = 'dotted') +
  ggtitle('time average over beta') +
  scale_x_continuous(name='beta', limits = c(0,4), breaks = seq(0,4,0.5)) +
  scale_y_continuous(name='time average', limits = c(0,1), breaks = seq(0,1,0.5)) +
  scale_colour_manual(values=c('blue','red', 'black'), 
                      labels=c('row time average','column time average', 'NE')) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

rm(simulation_function, pic, df)


##########Figure: Radius of cycle over beta##########
# set up parameters
iteration = 1500
pure_indicator = 0
step = 1

# set storage dataset
df = data.frame(beta = seq(0.1, 4, 0.01), to_ne = rep(NA, length(seq(0.1, 4, 0.01))))

for (i in 1:length(df$beta)){
  row_speed = df$beta[i]
  column_speed = df$beta[i]
  simulation_function = data.frame(learning_simulation(iteration, row_speed, column_speed, pure_indicator, step))
  df$to_ne[i] = mean(simulation_function$to_ne[iteration-500:iteration])
}

# draw figure
library(ggplot2)
title = paste('distance_to_NE_over_beta')
file = paste("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/writeup/figs/sims/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 800, height = 400)

pic = ggplot() +
  geom_smooth(data = df, mapping = aes(x = beta, y = to_ne), stat = 'smooth',
              method = loess, formula = 'y ~ x', se = FALSE) +
  geom_line(data = df, mapping = aes(x = beta, y = to_ne), colour = 'blue', size = 1) +
  ggtitle('distance to NE over beta') +
  scale_x_continuous(name='beta', limits = c(0,4), breaks = seq(0,4,0.5)) +
  scale_y_continuous(name='distance to NE') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

rm(df, pic, simulation_function)


##########Figure: Showcase of a single cycle given beta##########
# basic parameters and datasets
iteration = 1500
pure_indicator = 0
step = 1

# figure 1 small beta
library(dplyr)
row_speed = 0.1
column_speed = 0.24
simulation_data1 = data.frame(learning_simulation(iteration, row_speed, column_speed, pure_indicator, step))
simulation_data1 = simulation_data1 %>% mutate(
  tripwire = ifelse(column_strategy>0.2 & (row_strategy-0.5)*(row_next-0.5)<= 0, 1, 0))
simulation_data1 = filter(simulation_data1, iteration>=1400)

# figure 2 large beta
row_speed = 1
column_speed = 1
simulation_data2 = data.frame(learning_simulation(iteration, row_speed, column_speed, pure_indicator, step))
simulation_data2 = simulation_data2 %>% mutate(
  tripwire = ifelse(column_strategy>0.2 & (row_strategy-0.5)*(row_next-0.5)<= 0, 1, 0))
simulation_data2 = filter(simulation_data2, iteration>=1300)

# figure 2 extra large beta
row_speed = 4
column_speed = 4
simulation_data3 = data.frame(learning_simulation(iteration, row_speed, column_speed, pure_indicator, step))
simulation_data3 = simulation_data3 %>% mutate(
  tripwire = ifelse(column_strategy>0.2 & (row_strategy-0.5)*(row_next-0.5)<= 0, 1, 0))
simulation_data3 = filter(simulation_data3, iteration>=1300)

# set up the figure
# draw figure
library(ggplot2)
title = paste('sample_circle_over_beta')
file = paste("D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/writeup/figs/sims/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 600, height = 600)

pic = ggplot() +
  geom_point(data = simulation_data1, mapping = aes(x = column_strategy, y = row_strategy), colour = 'red', size = 2) +
  geom_point(data = simulation_data2, mapping = aes(x = column_strategy, y = row_strategy), colour = 'blue', size = 2) +
  geom_point(data = simulation_data3, mapping = aes(x = column_strategy, y = row_strategy), colour = 'green', size = 2) +
  geom_hline(aes(yintercept=0.5), colour = 'black', linetype = 'dotted') +
  geom_vline(aes(xintercept=0.2), colour = 'black', linetype = 'dotted') +
  ggtitle('sample circles beta=0.1/0.24 and beta=1') +
  scale_x_continuous(name='column strategy', limits = c(0,1)) +
  scale_y_continuous(name='row strategy', limits = c(0,1)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()