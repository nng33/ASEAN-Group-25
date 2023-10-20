## Group 25
## Frans : s2591760
## Daiki : s2547603
## Nathan : s2524152


## Contribution to this project
## Frans (40%): Handled the code for the French and British borders.
## Daiki (30%): Handled the code for cars going between French and British stations and probabilities.
## Nathan (30%): Handled the British processing side and plots. 


##################################################################################################


# This program simulates queuing system of French and British passport control
# station at a French ferry terminal. Cars first have to go through the French
# station followed by British station. Simulation runs for 2 hours (7200 seconds) 
# with 1 second step. Assume check-in closes 30 minutes before departure so that 
# car arrivals at the French station will stop after 90 minutes (5400 seconds).

# 2 simulations are run where one of them have matching arrival rate and processing
# rate while the other one is set such that there is a slight delay in the 
# British processing time.
# Plots of the average French and British queue length over time are generated
# along with the expected waiting time in the queue for a car at the start of 
# of the French queue.

# Finally, probability of at least one car missing the ferry is then estimated
# by running the simulation 100 times and considering the average French and 
# British queue length at the last second.


###############################################################################################


# The function qsim() simulates the French and British queuing system.
# qsim() takes on 8 inputs:
# (1) mf is the number of French passport control stations (default: 5)     
# (2) mb is the number of British passport control stations (default: 5)    
# (3) a.rate is the probability of a car arriving each second (default: 0.1)
# (4) trb is a constant such that the British station processing time follows 
#     Uniform(tmb, tmb+trb) (default: 40)
# (5) tmb is the minimum British handling time (default: 30)
# (6) trf is a constant such that the French station processing time follows 
#     Uniform(tmf, tmf+trf) (default: 40)
# (7) tmf is the minimum French handling time (default: 30)
# (8) maxb is the maximum British queue length per station (default: 20)

# qsim() returns a list with 3 elements:
# (1) nf: a vector of the average length of the French queues for every second
# (2) nb: a vector of the average length of the British queues for every second
# (3) eq: a vector of the average expected waiting time in the queue
#     for a car at the start of the French queue for every second

qsim <- function(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20){
  
  # Initialize state variables: these values corresponds to time t = 0
  
  # each column of french_arrivals represents number of car arriving 
  # at each French station at the beginning of time t
  french_arrivals <- rep(0,mf)

  # each column of french_queues represents the number of cars waiting 
  # at each French station's queue at the beginning of time t
  french_queues <- rep(0,mf)

  # each column of french_time represents the service time left for a customer
  # at each French station at the beginning of time t
  french_times <- rep(0,mf)

  # each column of french_busy indicates whether each 
  # French station is busy (1) or not (0) at the beginning of time t
  french_busy <- rep(0,mf)

  # each column of french_exit indicates whether the car exit (1) or not (0)
  # at each French station at the end of time t/beginning of time t+1 
  french_exit <- rep(0,mf)

  # each column of french_stuck indicates whether there's a car stuck at 
  # the corresponding station (1) or not (0) at the end of time t/
  # beginning of time t+1. 
  french_stuck <- rep(0,mf)

  # Do the same for the British queues and time
  british_arrivals <- rep(0,mb)
  
  british_queues <- rep(0,mb)
  
  british_times <- rep(0,mb)

  british_busy <- rep(0,mb)
  
  british_exit <- rep(0,mb)

  
  # each column of british_available indicates whether each British station is     
  # available (1) or not (0) at the end of time t/beginning of time t+1      
  # for containing a car arriving from the French station 
  british_available <- rep(1,mb)

  
  # Collect the index of available British stations
  available_station <- which(british_available == 1)
  
  #Initialize empty vectors 
  nf <- numeric()
  nb <- numeric()
  eq <- numeric()
  
  # iterate over the entire 2 hour (7200 seconds) period
  for (t in 1:7200) {
    # Reset number of arrivals for both British and French stations at the start of the loop    
    french_arrivals <- rep(0,mf)
    british_arrivals <- rep(0,mf)
    
    # Start from the British stations
    
    # update british_arrivals at time t
    # arrivals depends on french_exit at time t-1     
    # if a car manages to exit at time t-1, it already checked the availability of    
    # British stations
    
    # the number of arrivals at the British station is the total number of cars    
    # exiting the French station
    british_num_arrivals <- sum(french_exit)
    
    # If there are cars arriving at the British station
    if (british_num_arrivals > 0){
      
      # number of busy British stations
      num_british_busy <- sum(british_busy)
      
      # iterate over each arriving car
      for (i in 1:british_num_arrivals){
        
        # If there are any stations that are not busy
        if (num_british_busy < 5) {
          # go to the station that is not busy because
          # if station is not busy at time t-1, it is available at time t
          
          # obtain the index of the target station:
          # get the index of the station that is not busy (the station will have  
          # a minimum value of 0 as its busy state) that are available
          # then index available_station to get the targeted station
          go_here <- available_station[which.min(british_busy[available_station])]
          
          # assign arrival to the chosen station
          british_arrivals[go_here] <- 1
          
          
          # remove the chosen station from choices of available station
          # because it is busy now
          available_station <- available_station[!available_station == go_here]
          
          # the chosen station is now busy to serve the new car, so add
          # number of busy British station by one
          num_british_busy <- num_british_busy + 1
          
          # This prevents a car going to British station with 0 queue that is busy 
          # serving customer and instead, direct them to stations with 0 queue 
          # that are not busy
        }
        
        # else, when all stations are busy
        else{
          # only consider the minimum queue length at time t-1 of British stations that 
          # are available
 
          # which available station has the minimum queue?
          # first extract the British queues that are available
          # then find the index of the extracted stations with the minimum queue
          # finally index available_station to get the original index of the
          # available station with the minimum queue length to go
          go_here <- available_station[which.min(british_queues[available_station])]
          
          # enter one available station with minimum queue 
          british_arrivals[go_here] <- 1
          
          # remove the chosen station from the choices of the available station
          # because that station will not have the minimum queue anymore
          available_station <- available_station[!available_station == go_here]
        }
      }
    }
    
    # update British station busy state to values at time t
    
    # a British station is busy at time t if:
    # (1) the service time left at time t-1 is more than 1 second 
    #     (i.e, still need to serve at time t)
    # (2) or there's a customer waiting in queue at time t-1 ready to be served
    # (3) or a customer arrives exactly at time t
    for (i in 1:mb){
      if (british_times[i] > 1 || british_queues[i] > 0 || british_arrivals[i] == 1){
        british_busy[i] <- 1
      }
      # otherwise, the station is not busy
      else{
        british_busy[i] <- 0
      }
    }
    
    # update British queues length to values at time t
    for (i in 1:mb){
      # a British station's queue length changes when:
      # (1) if a station is busy at time t-1 (service time at time t-1 > 1) 
      #     then add the number of arrivals at time t to the queue length at time t-1
      # notice that these conditions allow a car who arrives exactly at time t
      # without any queue to go straight into the station without having to queue
      if (british_times[i] > 1){
        british_queues[i] <- british_queues[i] + british_arrivals[i]
      }
      
      # (2) if there's an exit recorded at time t-1 then reduce the length of the
      #     queue by the number of exit and add the arrivals at time t to the 
      #     queue length at time t-1
      else if (british_exit[i] == 1){
        
        # take the maximum between 0 and the calculated value to prevent
        # queue length going to negative (e.g., when a car exits from a station
        # with 0 queue length) 
        british_queues[i] <- max(0,british_queues[i] - british_exit[i] + british_arrivals[i])
      }
      
      # update British service time
      
      # a British station's service time changes when:
      # (1) if the service time at time t-1 > 1, then reduce that by one since
      #     time has progressed by one second after each iteration
      if (british_times[i] > 1){
        british_times[i] <- british_times[i] - 1
      }
      
      # (2) else if the station at time t is busy because of a new customer 
      #     arriving at time t and go straight to the station, then generate 
      #     a new service time following uniform(tmb, tmb+trb)
      else if (british_busy[i] == 1){
        
        # round the time to the nearest integer
        british_times[i] <- round(runif(1, tmb, tmb+trb),0)
      }
      
      # (3) otherwise, the station is not busy with no cars waiting or being served.
      #     So, assign zero.
      else{
        british_times[i] <- 0
      }
    }
    
    # update British exit state at time t
    
    for (i in 1:mb){
      # A car exit's value is recorded at time t when:
      # (1) if its service time at time t is 1 which implies that its service 
      #     will be finished at the end of time t
      if (british_times[i] == 1){
        british_exit[i] <- 1
      }
      
      # (2) in all other cases, no cars will exit
      else{
        british_exit[i] <- 0
      }
    }
    
    # update British availability at time t
    for (i in 1:mb){
      # a British station is available if:
      # (1) number of its queue at time t-1 < maximum queue length; or
      # (2) service time at time t-1 == 1 since even if queue length at time t-1 is maxb,
      #     at time t one person will exit and queue length at time t will be
      #     maxb - 1 < maxb
      
      if (british_queues[i] < maxb || british_times[i] == 1){
        british_available[i] <- 1
      }
      
      # otherwise, British station is not available
      else{
        british_available[i] <- 0
      }
    }
    
    # indices of available British station
    available_station <- which(british_available == 1)
    
    # number of available British station
    num_available_station <- sum(british_available)
    

    ## French side
    
    # Generate arrivals only within the first 90 minutes
    if (t <= 5400){  # 60s x 90mins = 5400s
      # the probability that a car arrives is a.rate
      # Let U be a random variable following Uniform(0,1)
      # Then, the Pr(U < a.rate) = a.rate
      # Therefore, the event that a number simulated from Uniform(0,1) being less than a.rate
      # (the probability of this happening is a.rate) implies that a car arrives
      # record 1 to num_arrivals if a car arrives, 0 otherwise.
      # Neglect the probability of 2 or more cars arriving in the same second
      num_arrivals <- as.integer(runif(1) < a.rate)
      
      # Update french_arrivals at time t
      
      # If there's a station that is not busy
      # choose to go to that station
      if (sum(french_busy) < 5) {
        french_arrivals[which.min(french_busy)] <- num_arrivals
      # This prevents car going to stations with 0 queue that are busy
      # and instead direct them to stations with 0 that are not busy
      }
      
      # otherwise, pick the station with the smallest queue length
      # Assume that the arriving car does not know how many seconds
      # the previous cars have left in a station
      else {
        french_arrivals[which.min(french_queues)] <- num_arrivals
      }
    }
    
    # update French station busy state to values at time t
    for (i in 1:mf) {
      # a French station is busy at time t if:
      # (1) the service time left at time t-1 is more than 1 second 
      #     (i.e, still need to serve at time t); or
      # (2) there's a customer waiting in queue at time t-1 ready to be served at time t; or
      # (3) a customer arrives exactly at time t; or
      # (4) a car is stuck inside waiting for the British queues to free up
      
      if (french_times[i] > 1 || french_queues[i] > 0 || french_arrivals[i] == 1 || french_stuck[i] == 1){
        french_busy[i] <- 1
      }
      # otherwise, the station is not busy
      else{
        french_busy[i] <- 0
      }
    }
    
    # update French queues lengths to values at time t
    for (i in 1:mf){
      # a French station's queue length changes when:
      # (1) if a station is busy at time t-1 (service time at time t-1 > 1 
      #     or a car is stuck inside) then add number of arrivals at time t
      #     to the queue length at time t-1.
      # notice that these conditions allow a car who arrives exactly at time t
      # without any queue to go straight into the station without having to queue
      if (french_times[i] > 1 || french_stuck[i] == 1){
        french_queues[i] <- french_queues[i] + french_arrivals[i]
      }
      
      # (2) if there's an exit recorded at time t-1 then reduce the length of the
      #     queue by the number of exit and add the arrivals at time t to the 
      #     queue length at time t-1
      else if (french_exit[i] == 1){
      # similar to the british_queues, taking the maximum between 0 and the value
      # ensures that queue length doesn't go to negative values
        french_queues[i] <- max(0,french_queues[i] - french_exit[i] + french_arrivals[i])
      }
    }
    
    # update French service time at time t
    for (i in 1:mf){
      # a French station's service time changes when:
      # (1) if the service time at time t-1 > 1 then reduce that by one since
      #     time has progressed by one second after each iteration
      if (french_times[i] > 1){
        french_times[i] <- french_times[i] - 1
      }
      
      # (2) else if the station at time t is busy because of a new customer 
      #     arriving at time t and go straight to the station (instead of busy
      #     because a car is stuck or because it is still serving previous customer 
      #     (first if condition)) then generate a new service time following
      #     uniform(tmf, tmf+trf)
      else if (french_busy[i] == 1 && french_stuck[i] == 0){
        french_times[i] <- round(runif(1, tmf, tmf+trf),0)
      }
      
      # (3) otherwise, the station is not busy with no cars waiting or being served.
      #     So, assign zero.
      else{
        french_times[i] <- 0
      }
    }
    
    # update French exit and stuck state at time t
    
    for (i in 1:mf){
      # (1) if its service time at time t == 1, it implies that its service 
      #     will be finished at the end of time t and there is an available 
      #     spot at the British stations, then the car can exit
      
      if (french_times[i] == 1 && num_available_station > 0){ 
        french_exit[i] <- 1
        num_available_station <- num_available_station - 1
      }
      
      # (2) else if service time at time t == 1 but there is no free space at the
      #     British station, the car doesn't exit (assign 0 to french_exit) but 
      #     it gets stuck for the first time (assign 1 to french_Stuck)
      else if (french_times[i] == 1 && num_available_station == 0){
        french_exit[i] <- 0
        french_stuck[i] <- 1
      }
      
      # (3) in all other cases, no cars will exit
      else{
        french_exit[i] <- 0
      }
    }
    
    # for each station, update stuck at time t to still stuck or manage to exit
    for (i in 1:mf){
      # if you've been assigned exit, you cannot be stuck
      if (french_exit[i] == 1){
        french_stuck[i] <- 0
      }
      # else if the car was assigned to be stuck, check whether it is still
      # stuck or it can exit
      else if (french_stuck[i] == 1){
        # if there is an available station, it can exit. Otherwise, it stays stuck.
        if (num_available_station > 0){
          french_stuck[i] <- 0
          french_exit[i] <- 1
          num_available_station <- num_available_station - 1
        }
      }
      # in other cases, no cars are stuck
      else{
        french_stuck[i] <- 0
      }
    }
    
    # Record average queue lengths 
    # nf is the average queue lengths for the French stations
    nf[t] <- mean(french_queues)
    # nb is the average queue lengths for the British stations
    nb[t] <- mean(british_queues)
    
    # eq is the expected waiting time at the start of the French queue
    eq[t] <- mean(french_queues) * mean(french_times) + mean(british_queues) * mean(british_times)
    
  }
  
  # Return a list of the vectors calculated above
  return(list(nf = nf, nb = nb, eq = eq))
}


####################################################################################################################


# simulate queue with default parameter
sim <- qsim()

# simulate queue with minimum British handling time set to 40 seconds
sim_40 <- qsim(tmb = 40)

# Set up a 2x2 grid for plots
par(mfrow = c(2, 2))  

# Plot 1: Average French and British queue lengths over time when tmb = 30
plot(1:7200, sim$nf, type = "l", xlab = "Time (seconds)", ylab = "Queue Length", ylim = c(0,20))
lines(1:7200, sim$nb, col = "red")
title("Average Queue Length When tmb = 30")
legend("topleft", legend = c("French Queue", "British Queue"), col = c("black", "red"), lty = 1, bty = "n")

# Plot 2: Expected queuing time over time when tmb = 30
plot(1:7200, sim$eq, type = "l", xlab = "Time (seconds)", ylab = "Expected Queuing Time", ylim = c(0,1000))
title("Expected Queueing Time When tmb = 30")

# Plot 3: Average French and British queue lengths over time when tmb = 40
plot(1:7200, sim_40$nf, type = "l", xlab = "Time (seconds)", ylab = "Queue Length", ylim = c(0,20))
lines(1:7200, sim_40$nb, col = "red")
title("Average Queue Length When tmb = 40")
legend("topleft", legend = c("French Queue", "British Queue"), col = c("black", "red"), lty = 1, bty = "n")

# Plot 4: Expected queuing time over time when tmb = 40
plot(1:7200, sim_40$eq, type = "l", xlab = "Time (seconds)", ylab = "Expected Queuing Time", ylim = c(0,1000))
title("Expected Queueing Time When tmb = 40")


# Estimation of the probability that at least one car miss the ferry departure 
# if there is a slight delay in the British processing time.

# Initialize a variable that counts the number of times in each simulation that 
# at least one car is still in the queue by the end of the simulation
missing <- 0

# Simulation time 
sim_t <- 7200

# run the simulation 100 times
for (i in 1:100) {
  # Assign the average French and British queue lengths at the end of the 
  # simulation to last_nf and last_nb, respectively
  sim_prob <- qsim(tmb = 40)
  last_nf <- sim_prob$nf[sim_t]
  last_nb <- sim_prob$nb[sim_t]
  
  # non-zero average of the queue lengths indicate that there are still cars
  # remaining in the queue.
  # Therefore, if the sum of the averages are not 0
  if (sum(last_nf, last_nb) != 0) {
    # there is at least one car stuck in the queue at the end of the 2-hour 
    # simulation period
    missing <- missing + 1
  }
}

# Calculate the empirical probability that at least one car miss the ferry 
# and print out the result
prob <- missing / 100
cat("The probability of at least one car missing the ferry departure is ", prob, ".", sep = "")


# Estimation of the probability if tmb = 30 (almost the same as the case of tmb = 40)
missing_2 <- 0
for (i in 1:100) {
  sim_prob2 <- qsim(tmb = 30)
  last_nf2 <- sim_prob2$nf[sim_t]
  last_nb2 <- sim_prob2$nb[sim_t]
  
  if (sum(last_nf2, last_nb2) != 0) {
    missing_2 <- missing_2 + 1
  }
}
prob2 <- missing_2 / 100
cat("The probability of at least one car missing the ferry departure is ", prob2, ".", sep = "")


# Findings:

# If the average British processing time matches the average French processing time,
# then the average queue length and expected queuing time remains relatively low. 
# However, when there is a small extra delays in British checking, 
# the expected queuing time will be visibly greater. 

# This is because a longer processing time for the British station greatly 
# increases the average British queue length.
# However, the length of the French queues will remain similar when there are 
# just slight delays in British processing time because cars are exiting the 
# French station normally as long as no cars are getting stuck in the French station.

# A notable increase in the average French queue length would be expected when 
# the British stations start reaching their maximum capacity i.e., when cars
# starts getting stuck at the French station which would impact the French queue length. 

# As a result, the probability that at least one car miss the ferry when the 
# average rate of processing British passports falls behind the arrival rate 
# into the French queues will be greater than if both rates are the same. 
