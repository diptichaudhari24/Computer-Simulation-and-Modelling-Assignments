# Dipti Chaudhari 

# Global data
contract_file <- c(30,20,15,0,0,21,14,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,20,0,0,41,0,15,0,7,0,0,0,9,0,0,0,0,0,10,0,0,0,0,10,0,25,5,10,0,10,0,10,0,0,10,10,0,0,0,0,80,0,0,0,0,0,0,0,0,0,0,0,20,0,0,0,0,20,0,0,0,0,0,10,0,0,15,0,10,0,0,0,10,10,10,0,15,0,10,10,0,0,15,0,0,7,8,0,0,0,0,9,6,20,0,15,0,3,6,9,0,15,0,11,7,0,10,0,12,0,0,14,0,0,0,0,13,7,5,6,8,4,5,5,5,5,8,4,10,5,5,4,4,2,2,3,3,3,3,2,2,4,4,5,5,10,4,8,5,5,5,5,4,8,6,5,7,10,10,10,10,0,0,0,0,10,0,15,0,0,15,0,0,0,0,0,20,0,0,0,0,0,0,0,0,0,0,0,0,0,14,21,0,0,15,20,30,0,0,20,0,8,0,0,15,0,0,41,0,9,0,0,0,0,7,0,0,6,6,6,6,6,6,6,6,8,6,0,6,0,6,0,6,4,6,4,6,5,7,8,6,5,4,5,5,8,5,8,0,10,4,0,2,2,2,3,0,0,0,0,0,10,10,10,10,0,15,0,10,0,0,15,0,20,0,0,0)
Station_file<- c(1,13.60,0.16,1,12.00,0.16,1,5.60,0.14,9,0.76,0.02,6,0.65,0.01,6,1.12,0.02,1,5.53,0.17,3,5.53,0.17,2,7.35,0.12,8,0.78,0.02,11,1.09,0.02,8,0.78,0.02,18,1.45,0.02,6,7.74,0.02,4,0.95,0.01)
contract_data = matrix(rbind(contract_file),ncol=20,byrow=TRUE)
station_data = matrix(rbind(Station_file),ncol=3,byrow=TRUE)
power_demand <- c(6.04, 7.15, 9.04, 10.12, 5.80, 5.40, 6.20, 6.06, 7.97, 6.52, 9.05, 5.37, 3.99, 6.69, 5.85, 5.88, 4.86, 5.86, 7.00, 8.30)
mttf_original_conf = 0
mttf_consortium <-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

# EXERCISE 1

# This function creates the data for one day 
generate_data<-function(){
  power_supply <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  random_variable = 0
  
  for (station in 1:15){
    number_of_generators = station_data[station,1]
    # Sample for each generator
    working = runif(number_of_generators)
    
    power_generated = 0
    functional_generators = 0
    for (generator in 1:number_of_generators) {
      # If generator is functioning
      if ( working[generator] > station_data[station,3] )
      {
        functional_generators = functional_generators + 1
      }
    }
    power_generated = functional_generators * station_data[station,2]
    
    # Distribute the power generated to cities
    for (city in 1:20)
    {
      power_supply[city] = power_supply[city] + (contract_data[station,city]/100 *power_generated)
    }
  }
  return(power_supply)
}


daily_power<-function(){
  power_supply = generate_data()
  cat("\nCity\tPower_demand\tpower_supply\tpower_supply- power_demand")
  for (city in 1:20){
      cat("\n", LETTERS[city], "\t",power_demand[city],"\t\t" ,power_supply[city],"\t\t", power_supply[city] - power_demand[city])
  }
}
# FUNCTION CALL & OUTPUT
# daily_power()
# 
# City	Power_demand	power_supply	power_supply- power_demand
# A 	 6.04 		 11.1402 		 5.1002
# B 	 7.15 		 10.8831 		 3.7331
# C 	 9.04 		 12.3443 		 3.3043
# D 	 10.12 		 12.0696 		 1.9496
# E 	 5.8 		 9.1859 		 3.3859
# F 	 5.4 		 8.9171 		 3.5171
# G 	 6.2 		 9.2948 		 3.0948
# H 	 6.06 		 11.9586 		 5.8986
# I 	 7.97 		 10.5187 		 2.5487
# J 	 6.52 		 10.0392 		 3.5192
# K 	 9.05 		 9.2894 		 0.2394
# L 	 5.37 		 6.2336 		 0.8636
# M 	 3.99 		 7.2599 		 3.2699
# N 	 6.69 		 7.6027 		 0.9127
# O 	 5.85 		 6.3247 		 0.4747
# P 	 5.88 		 6.9204 		 1.0404
# Q 	 4.86 		 5.7506 		 0.8906
# R 	 5.86 		 6.8393 		 0.9793
# S 	 7 		 7.4679 		 0.4679
# T 	 8.3 		 9.2416 		 0.9416

# EXERCISE 2

failure<-function(){
  power_supply = generate_data()
  
  for (city in 1:20){
    if((power_supply[city] - power_demand[city]) < 0){
      # blackout day
      return(1)
    }
  }
  return(0)
}

estimate_failure <- function()
{
  n = 1000000
  d = 0.9
  sum = 0
  sum_of_square = 0
  for (i in 0:n){
    f = failure()
    sum = sum + f
    sum_of_square = sum_of_square + f * f
  }
  cat("\n\nNumber of Samples: ",n)
  
  sample_mean = sum/n
  cat("\nSample Mean: ", sample_mean)
  
  sample_variance = (sum_of_square - n*(sample_mean * sample_mean))/(n-1)
  cat("\nSample Variance: ", sample_variance)
  
  standard_deviation = sqrt(sample_variance)
  cat("\nStandard Deviation: ", standard_deviation)
  
  standard_error = standard_deviation/sqrt(n)
  cat("\nStandard Error: ", standard_error)
  
  inverse_cdf = qnorm((1 + d)/2)
  value = standard_error * inverse_cdf
  
  ci_left = sample_mean-value*standard_error
  ci_right = sample_mean+value*standard_error
  
  cat("\n",d,"-confidence interval: [",ci_left,",",ci_right,"]\n")
  
  
  cat("\nProbability of failure day:",sample_mean)
  
  mttf = 1/sample_mean - 1
  assign("mttf_original_conf",mttf, envir = .GlobalEnv)
  
  cat("\nMean time to Failure:",mttf_original_conf)

}

# FUNCTION CALL & OUTPUT

# > estimate_failure()
# 
# 
# Number of Samples:  1e+06
# Sample Mean:  0.61099
# Sample Variance:  0.2376815
# Standard Deviation:  0.4875259
# Standard Error:  0.0004875259
# 0.9 -confidence interval: [ 0.6109896 , 0.6109904 ]
# 
# Probability of failure day: 0.61099
# 
# Mean time to Failure: 0.636688


# EXERCISE 3

failure2<-function(generator_no,p_g){
  power_supply <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  random_variable = 0
  
  for (station in 1:15){
    number_of_generators = station_data[station,1]
    # Sample for each generator
    working = runif(number_of_generators)
    
    power_generated = 0
    functional_generators = 0
    non_fuctioning_probability = station_data[station,3]
    for (generator in 1:number_of_generators) {
      
      # Check for the generator with new probability
      if(generator == generator_no){
        non_fuctioning_probability = p_g
      }
      
      # If generator is functioning
      if ( working[generator] > non_fuctioning_probability )
      {
        functional_generators = functional_generators + 1
      }
    }
    power_generated = functional_generators * station_data[station,2]
    
    # Distribute the power generated to cities
    for (city in 1:20)
    {
      power_supply[city] = power_supply[city] + (contract_data[station,city]/100 *power_generated)
    }
  }
  for (city in 1:20){
    if((power_supply[city] - power_demand[city]) < 0){
      return(1)
    }
  }
  return(0)
}

# EXERCISE 4

consortium_report <- function (){
  local_mttf <-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  cat("\n\nConsortium Report: ")
  for (station in 1:15){
    cat("\n\nFor station: ",station)
    n = 500000
    d = 0.9
    sum = 0
    sum_of_square = 0
    for (i in 0:n){
      f = failure2(station,station_data[station,3]/2)
      sum = sum + f
      sum_of_square = sum_of_square + f * f
    }
  
    sample_mean = sum/n
    cat("\nProbability of failure day if station ",station," is upgraded: ",sample_mean)
    
    sample_variance = (sum_of_square - n*(sample_mean * sample_mean))/(n-1)
    standard_deviation = sqrt(sample_variance)
    standard_error = standard_deviation/sqrt(n)
    inverse_cdf = qnorm((1 + d)/2)
    value = standard_error * inverse_cdf
    
    ci_left = sample_mean-value*standard_error
    ci_right = sample_mean+value*standard_error
    
    cat("\n",d,"-confidence interval: [",ci_left,",",ci_right,"]\n")
    
    local_mttf[station] = 1/sample_mean - 1
    cat("\nMean time to Failure:",local_mttf[station])
  }
  assign("mttf_consortium",local_mttf, envir = .GlobalEnv)
  
}

# FUNCTION CALL & OUTPUT

# > consortium_report()
# 
# 
# Consortium Report: 
#   
#   For station:  1
# Probability of failure day if station  1  is upgraded:  0.760172
# 0.9 -confidence interval: [ 0.7601714 , 0.7601726 ]
# 
# Mean time to Failure: 0.3154918
# 
# For station:  2
# Probability of failure day if station  2  is upgraded:  0.803164
# 0.9 -confidence interval: [ 0.8031635 , 0.8031645 ]
# 
# Mean time to Failure: 0.2450757
# 
# For station:  3
# Probability of failure day if station  3  is upgraded:  0.759578
# 0.9 -confidence interval: [ 0.7595774 , 0.7595786 ]
# 
# Mean time to Failure: 0.3165205
# 
# For station:  4
# Probability of failure day if station  4  is upgraded:  0.586504
# 0.9 -confidence interval: [ 0.5865032 , 0.5865048 ]
# 
# Mean time to Failure: 0.7050182
# 
# For station:  5
# Probability of failure day if station  5  is upgraded:  0.582884
# 0.9 -confidence interval: [ 0.5828832 , 0.5828848 ]
# 
# Mean time to Failure: 0.7156072
# 
# For station:  6
# Probability of failure day if station  6  is upgraded:  0.599108
# 0.9 -confidence interval: [ 0.5991072 , 0.5991088 ]
# 
# Mean time to Failure: 0.6691481
# 
# For station:  7
# Probability of failure day if station  7  is upgraded:  0.661768
# 0.9 -confidence interval: [ 0.6617673 , 0.6617687 ]
# 
# Mean time to Failure: 0.5111036
# 
# For station:  8
# Probability of failure day if station  8  is upgraded:  0.647958
# 0.9 -confidence interval: [ 0.6479572 , 0.6479588 ]
# 
# Mean time to Failure: 0.5433099
# 
# For station:  9
# Probability of failure day if station  9  is upgraded:  0.62647
# 0.9 -confidence interval: [ 0.6264692 , 0.6264708 ]
# 
# Mean time to Failure: 0.5962456
# 
# For station:  10
# Probability of failure day if station  10  is upgraded:  0.609996
# 0.9 -confidence interval: [ 0.6099952 , 0.6099968 ]
# 
# Mean time to Failure: 0.639355
# 
# For station:  11
# Probability of failure day if station  11  is upgraded:  0.609374
# 0.9 -confidence interval: [ 0.6093732 , 0.6093748 ]
# 
# Mean time to Failure: 0.6410283
# 
# For station:  12
# Probability of failure day if station  12  is upgraded:  0.60992
# 0.9 -confidence interval: [ 0.6099192 , 0.6099208 ]
# 
# Mean time to Failure: 0.6395593
# 
# For station:  13
# Probability of failure day if station  13  is upgraded:  0.610504
# 0.9 -confidence interval: [ 0.6105032 , 0.6105048 ]
# 
# Mean time to Failure: 0.6379909
# 
# For station:  14
# Probability of failure day if station  14  is upgraded:  0.610202
# 0.9 -confidence interval: [ 0.6102012 , 0.6102028 ]
# 
# Mean time to Failure: 0.6388016
# 
# For station:  15
# Probability of failure day if station  15  is upgraded:  0.610422
# 0.9 -confidence interval: [ 0.6104212 , 0.6104228 ]
# 
# Mean time to Failure: 0.6382109



# EXERCISE 5

consortium_recommendation <- function(){
  flag = FALSE
  for (station in 1:15){
    mttf_diff = 0
    if (mttf_original_conf > mttf_consortium[station]){
      
      mttf_diff = mttf_original_conf - mttf_consortium[station]
      cat("\n\nMean time to Failure for station ",station, " is reduced by ", mttf_diff ," hence not a favorable change.")
    }
    else{
      mttf_diff = -1 * (mttf_original_conf - mttf_consortium[station])
      cat("\n\nMean time to Failure for station ",station, " is extended by ", mttf_diff)
      if(mttf_diff >4)
      {
        cat("\nWhich is greater than 4 days.")
        cat("\n\n\nConsortium is recommended.")
        flag = TRUE
      }
      else{
        cat("\nBut it is not by 4 days.")
      }

    }
  }
  if (flag == FALSE){
    cat("\n\n\nConsortium is not recommended.")
  }
}

# FUNCTION CALL & OUTPUT

# consortium_recommendation()
# 
# 
# Mean time to Failure for station  1  is reduced by  0.3211962  hence not a favorable change.
# 
# Mean time to Failure for station  2  is reduced by  0.3916123  hence not a favorable change.
# 
# Mean time to Failure for station  3  is reduced by  0.3201675  hence not a favorable change.
# 
# Mean time to Failure for station  4  is extended by  0.0683302
# But it is not by 4 days.
# 
# Mean time to Failure for station  5  is extended by  0.0789192
# But it is not by 4 days.
# 
# Mean time to Failure for station  6  is extended by  0.0324601
# But it is not by 4 days.
# 
# Mean time to Failure for station  7  is reduced by  0.1255844  hence not a favorable change.
# 
# Mean time to Failure for station  8  is reduced by  0.0933781  hence not a favorable change.
# 
# Mean time to Failure for station  9  is reduced by  0.0404424  hence not a favorable change.
# 
# Mean time to Failure for station  10  is extended by  0.002667
# But it is not by 4 days.
# 
# Mean time to Failure for station  11  is extended by  0.0043403
# But it is not by 4 days.
# 
# Mean time to Failure for station  12  is extended by  0.0028713
# But it is not by 4 days.
# 
# Mean time to Failure for station  13  is extended by  0.0013029
# But it is not by 4 days.
# 
# Mean time to Failure for station  14  is extended by  0.0021136
# But it is not by 4 days.
# 
# Mean time to Failure for station  15  is extended by  0.0015229
# But it is not by 4 days.
# 
# 
# Consortium is not recommended.