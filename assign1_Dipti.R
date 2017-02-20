# Dipti Chaudhari 

#Exercise 1

# Function Call to function craps is as follow

# craps()

craps <- function(){
  
  # do first roll
  dice_1_outcome <- sample(seq(1:6), 1)
  dice_2_outcome <- sample(seq(1:6), 1)
  
  # return if win or lose in first roll
  sum <- dice_1_outcome + dice_2_outcome
  #cat("Dice_1 ",dice_1_outcome ," Dice_2 ", dice_2_outcome)
  
  if (sum == 7 | sum == 11) {
    #cat("\nWinner on first roll as sum is ",sum)
    return (1)
  }
  if (sum == 2 | sum == 3 | sum == 12 ) {
    #cat("\nLoss on first roll as sum is ",sum)
    return (0)
  }
  
  # repeat until win (X) or lose (7)
  X <- c(4, 5, 6, 8, 9, 10)
  while (TRUE) {
    dice_1_outcome <- sample(seq(1:6), 1)
    dice_2_outcome <- sample(seq(1:6), 1)
    sum <- dice_1_outcome + dice_2_outcome
    #cat("\nDice_1 ",dice_1_outcome ," Dice_2 ", dice_2_outcome)
    
    # loss as sum is 7
    if (sum == 7) {
      #cat("\nLoss on later roll as sum is ",sum)
      return (0)
    }
    # win as sum is in X
    if (sum %in% X) {
      #cat("\nWinner on later roll as sum is in the set X ",sum)
      return (1)
    }
  }
}


#Exercise 2

# Function f takes no input and returns 0 or 1

f <- function(){
  return (sample(c(0,1),1))
}

# Estiate_bernoulli returns the probability of any function returning 1

# Function Call to function estimate_bernoulli is as follow

# estimate_bernoulli(f,0.9,0.005)

estimate_bernoulli <- function(function_name, d, e)
{
  n = 0
  while(1){
    n=n+10000
    sum = 0
    sum_of_square = 0
    for (i in 0:n){
      event = function_name()
      sum = sum + event
      sum_of_square = sum_of_square + event * event
    }
    cat("\n\nNumber of Samples: ", n)
    
    sample_mean = sum/n
    cat("\nSample Mean: ", sample_mean)
    
    sample_variance = (sum_of_square - n*(sample_mean * sample_mean))/(n-1)
    cat("\nSample Variance: ", sample_variance)
    
    standard_deviation = sqrt(sample_variance)
    cat("\nStandard Deviation: ", standard_deviation)
    
    standard_error = standard_deviation/sqrt(n)
    cat("\nStandard Error: ", standard_error)
    
    relative_error = standard_error/sample_mean
    cat("\nRelative Error: ", relative_error)
    
    inverse_cdf = qnorm((1 + d)/2)
    value = standard_error * inverse_cdf
    
    ci_left = sample_mean-d*standard_error
    ci_right = sample_mean+d*standard_error
    
    cat("\n",d,"-confidence interval: [",ci_left,",",ci_right,"]\n")
    
    if (value < e){
      cat("\nProbability of winning a game of craps")
      return(sample_mean)
    }
  }
}


# Exercise 3

# Function Call for estimating Probability of winning Craps is as follow

# estimate_bernoulli(craps,0.9,0.005)


# Exercise 4

# Function Call to function network_reliability is as follow

# network_reliability()

generate_edge_vector <- function(){
  return (runif(7))
}

assign_random_variable <- function(v,p){
  if ( (v[1] > p && v[5] > p) || 
       (v[5] > p && v[6] > p && v[7] > p) || 
       (v[1] > p && v[4] > p && v[7] > p) || 
       (v[2] > p && v[3] > p && v[5] > p) || 
       (v[3] > p && v[4] > p && v[5] > p && v[7] > p) || 
       (v[2] > p && v[4] > p && v[5] > p && v[6] > p) || 
       (v[1] > p && v[3] > p && v[6] > p && v[7] > p) ){
    return(1)
  }
  else{
    return (0)
  }
}

network_reliability <- function()
{
  n = 0
  sum = 0
  sum_of_square = 0
  
  while(1){
    n = n + 1
    v = generate_edge_vector()
    x= assign_random_variable(v,p)
    
    sum = sum + x
    sum_of_square = sum_of_square + x * x
    
    if (n %% 1000000 == 0) {
      
      cat("\n\n For number of Samples: ", n)
      cat("\n Number of faults observed: ", sum)
      
    }
    
    if (sum >= 50) {
      cat("\nFinally got 50 broken configurations on taking ", n, " samples.")
      
      sample_mean = sum/n
      cat("\nSample Mean: ", sample_mean)
      
      sample_variance = (sum_of_square - n*(sample_mean * sample_mean))/(n-1)
      cat("\nSample Variance: ", sample_variance)
      
      break
    }
  }
}



# Exercise 5

# Function Call to function network_reliability2 which reduces variance is as follow

# network_reliability2()

network_reliability2 <- function()
{
  n = 0
  count = 0
  sum = 0
  sum_of_square = 0
  # For graph in Monte Carlo lecture notes 
  # Number of Edges (m) = 7
  # min cut set (r)=2
  # Calculating q=1- (r/m)
  q = 1 - (2/7)
  while(1){
    n = n + 1
    v = generate_edge_vector()
    x= assign_random_variable(v,q)
    if(x==1){
      count = count + 1
    } 
  
    var = (0.999/(5/7))**5*(0.001/(2/7))**2
    sum = sum + var
    sum_of_square = sum_of_square + var * var
    
    if (n %% 100 == 0) {
      
      cat("\n\n For number of Samples: ", n)
      cat("\n Number of faults observed: ", sum)
      
    }
    
    if (count >= 50) {
      cat("\n\nFinally got 50 broken configurations on taking ", n, " samples.")
      
      sample_mean = sum/n
      cat("\nSample Mean: ", sample_mean)
      
      sample_variance = (sum_of_square - n*(sample_mean * sample_mean))/(n-1)
      cat("\nSample Variance: ", sample_variance)
      
      break
    }
  }
}

