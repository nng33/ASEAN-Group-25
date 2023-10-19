## Group 25
## Frans : s2591760
## Daiki : s2547603
## Nathan : s2524152


## Contribution to this project
## Frans (%):  Daiki (%):  Nathan (%): 


#######################################################################################################################


qsim <- function(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20){
  
  # Initialize state variables: these values correspons to time t = 0
  
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

  # each column of french_exit indicates whether there's an exit (1) or not (1)
  # at each station at the end of time t/beginning of time t+1 
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

  
  
  # each column of british_available indicates whether each station is 
  # available (1) or not (0) at the end of time t/beginning of t+1 
  # for containing a car arriving from the French's exit
  british_available <- rep(1,mb)

  
  # which stations are available?
  available_station <- which(british_available == 1)
  
  #Initialize the empty vectors 
  nf <- numeric()
  nb <- numeric()
  eq <- numeric() 
  
  for (t in 1:7200) {
    # Reset number of arrivals for both British and French at the start of loop
    french_arrivals <- rep(0,mf)
    british_arrivals <- rep(0,mf)
    
    # Start from the british stations
    
    # update british arrivals at time t
    
    # arrivals depends on french_exit at t-1
    # if a car manages to exit at t-1, it already checked the availability of 
    # British stations
    ########################################################################################################
    
    # number of arrivals
    british_num_arrivals <- sum(french_exit)
    
    if (british_num_arrivals > 0){
      
      # number of busy British stations
      num_british_busy <- sum(british_busy)
      
      for (i in 1:british_num_arrivals){
        if (num_british_busy < 5) {
          # go to the station that is not busy
          # if station is not busy at t-1, it's available at t
          go_here <- available_station[which.min(british_busy[available_station])]
          british_arrivals[go_here] <- 1
          # remove the chosen station from choices of available station
          available_station <- available_station[!available_station == go_here]
          # the chosen station is now busy to serve the new car
          num_british_busy <- num_british_busy + 1
        }
        else{
          # only consider the minimum queue from stations that are available
          # minimum queue length at time t-1
          
          # which available station has the minimum queue?
          # first extract the british queues that are available
          # then find the index of the extracted stations with the minimum queue
          # finally index available_station to get the original index of the
          # available station with the minimum queue length to go
          go_here <- available_station[which.min(british_queues[available_station])]
          
          # enter one available station with minimum queue 
          british_arrivals[go_here] <- 1
          
          # remove the chosen station from the choices of available station
          available_station <- available_station[!available_station == go_here]
        }
      }
    }
    
    
    
    # update British station busy state to values at time t
    
    # a British station is busy at time t if:
    # (1) the service time left at t-1 is more than 1 second 
    #     (i.e, still need to serve at time t)
    # (2) there's a customer waiting in queue at t-1 ready to be served
    # (3) a customer arrives exactly at time t
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
      # (1) if a station is busy at time t-1 (service time at t-1 > 1) 
      #     then add number of arrivals at time t to the queue's length at time t-1.
      # notice that these conditions allow a car who arrives exactly at time t
      # without any queue to go straight into the station without having to queue
      if (british_times[i] > 1){
        british_queues[i] <- british_queues[i] + british_arrivals[i]
      }
      
      # (2) if there's an exit recorded at time t-1 then reduce the length of the
      #     queue by the number of exit and add the arrivals at time t to the 
      #     queue length at time t-1
      else if (british_exit[i] == 1){
        british_queues[i] <- max(0,british_queues[i] - british_exit[i] + british_arrivals[i])
      }
      
      # update British service time
      
      # a British station's service time changes when:
      # (1) if the service time at t-1 > 1 then reduce that by one since
      #     time has progressed by one second after each iteration
      if (british_times[i] > 1){
        british_times[i] <- british_times[i] - 1
      }
      
      # (2) else if the station at time t is busy because of a new customer 
      #     arriving at time t and go straight to the station because 
      #     it is still serving previous customer (first if condition)) 
      #     then generate a new service time following uniform(tmb, tmb+trb)
      else if (british_busy[i] == 1){
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
    
    #########################################################################################################    
    
    
    # update British availability at time t
    for (i in 1:mb){
      # British station is available if:
      # (1) number of its queue at t-1 < maximum queue length; or
      # (2) service time at t-1 == 1 since even if queue length at t-1 is maxb,
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
    
    # indices of available station
    available_station <- which(british_available == 1)
    
    # number of available station
    num_available_station <- sum(british_available)
    
    ##################################################################################################
    

    ## French side
    
    # Generate Poisson arrivals only within the first 90 minutes
    if (t <= 5400){  # 60x90mins = 5400s
      
      # the probability that a car arrives is a.rate
      # Let U be a random variable following Uniform(0,1)
      # Then, the Pr(U < a.rate) = a.rate
      # Therefore, a number simulated from Uniform(0,1) being less than a.rate
      # (the probability of this happening is a.rate) implies that a car arrives
      num_arrivals <- as.integer(runif(1) < a.rate)
      
      # Assign the num_arrivals to the shortest french queues using which.min
      # update to arrival number at time t
      # assume that arriving car does not know how many seconds left
      # the previous customer who is still being served at t-1 has in the station
      
      # Which station should be assigned for the new car
      # If there's a station that is not busy
      # choose to go to that station
      
      if (sum(french_busy) < 5) {
        french_arrivals[which.min(french_busy)] <- num_arrivals
      }
      
      # otherwise, pick the station with the smallest queue length
      else {
        french_arrivals[which.min(french_queues)] <- num_arrivals
      }
    }
    
    # For each individual station at the French border
    # Processing cars from the french station to the British station 
    for (i in 1:mf) {
      
      # update french station busy state to values at time t
      
      # a French station is busy at time t if:
      # (1) the service time left at t-1 is more than 1 second 
      #     (i.e, still need to serve at time t)
      # (2) there's a customer waiting in queue at t-1 ready to be served
      # (3) a customer arrives exactly at time t
      # (4) a car is stuck inside waiting for the British queues to free up
      
      if (french_times[i] > 1 || french_queues[i] > 0 || french_arrivals[i] == 1 || french_stuck[i] == 1){
        french_busy[i] <- 1
      }
      # otherwise, the station is not busy
      else{
        french_busy[i] <- 0
      }
    }
    
    # update french queues length to values at time t
    for (i in 1:mf){
      
      # a French station's queue length changes when:
      # (1) if a station is busy at time t-1 (service time at t-1 > 1 
      #     or a car is stuck inside) then add number of arrivals at time t
      #     to the queue's length at time t-1.
      # notice that these conditions allow a car who arrives exactly at time t
      # without any queue to go straight into the station without having to queue
      if (french_times[i] > 1 || french_stuck[i] == 1){
        french_queues[i] <- french_queues[i] + french_arrivals[i]
      }
      
      # (2) if there's an exit recorded at time t-1 then reduce the length of the
      #     queue by the number of exit and add the arrivals at time t to the 
      #     queue length at time t-1
      else if (french_exit[i] == 1){
        french_queues[i] <- max(0,french_queues[i] - french_exit[i] + french_arrivals[i])
      }
    }
    
    # update service time
    for (i in 1:mf){
      # a French station's service time changes when:
      # (1) if the service time at t-1 > 1 then reduce that by one since
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
      
      # (3) otherwise, the server is not busy with no cars waiting or being served.
      #     So, assign zero.
      else{
        french_times[i] <- 0
      }
    }
    
    # update exit and stuck state at time t
    
    for (i in 1:mf){
      # (1) if its service time at time t == 1 it implies that its service 
      #     will be finished at the end of time t and there is an available 
      #     spot at the British stations, then the car can exit
      
      if (french_times[i] == 1 && num_available_station > 0){
        french_exit[i] <- 1
        num_available_station <- num_available_station - 1
      }
      
      # (2) else if service time at t == 1 but there is no free space at the
      #     British station, the car doesn't exit (assign 0 to french_exit) but 
      #     it gets stuck for the first time (assign 1 to french_Stuck)
      else if(french_times[i] == 1 && num_available_station == 0){
        french_exit[i] <- 0
        french_stuck[i] <- 1
      }
      
      # (3) in all other cases, no cars will exit
      else{
        french_exit[i] <- 0
      }
    }
    
    ####################################################################################################################################################
    # for each station, update stuck at time t to still stuck or manage to exit
    for (i in 1:mf){
      # if you've been assigned exit, you cannot be stuck
      if (french_exit[i] == 1){
        french_stuck[i] <- 0
      }
      # else if the car was assigned to be stuck, check whether it is still
      # stuck or it can exit
      else if (french_stuck[i] == 1){
        # if there is an available station, it can exit
        if (num_available_station > 0){
          french_stuck[i] <- 0
          french_exit[i] <- 1
          num_available_station <- num_available_station - 1
        }
      }
    }

    # Record queue lengths
    nf[t] <- mean(french_queues)
    nb[t] <- mean(british_queues)
    
    # Expected waiting time at the start of the French queue
    eq[t] <- mean(french_queues) * mean(french_times) + mean(british_queues) * mean(british_times)
    
  }
  
  return(list(nf = nf, nb = nb, eq = eq))
}

####################################################################################################################


sim <- qsim()
sim

sim_40 <- qsim(tmb = 40)
sim_40

# Plot
# Set up a 2x2 grid for plots
par(mfrow = c(2, 2))  

# Plot 1: French and British queue lengths over time
plot(1:7200, sim$nf, type = "l", xlab = "Time (seconds)", ylab = "Queue Length", ylim = c(0,20))
lines(1:7200, sim$nb, col = "red")
title("Average Queue Length When tmb = 30")
legend("topleft", legend = c("French Queue", "British Queue"), col = c("black", "red"), lty = 1, bty = "n")

# Plot 2: Expected queuing time over time
plot(1:7200, sim$eq, type = "l", xlab = "Time (seconds)", ylab = "Expected Queuing Time", ylim = c(0,1000))
title("Expected Queueing Time When tmb = 30")

# Plot 3: French and British queue lengths over time
plot(1:7200, sim_40$nf, type = "l", xlab = "Time (seconds)", ylab = "Queue Length", ylim = c(0,20))
lines(1:7200, sim_40$nb, col = "red")
title("Average Queue Length When tmb = 40")
legend("topleft", legend = c("French Queue", "British Queue"), col = c("black", "red"), lty = 1, bty = "n")

# Plot 4: Expected queuing time over time
plot(1:7200, sim_40$eq, type = "l", xlab = "Time (seconds)", ylab = "Expected Queuing Time", ylim = c(0,1000))
title("Expected Queueing Time When tmb = 40")


# Estimation
# Calculate the probability that at least one car miss the ferry departure
missing <- 0
for (i in 1:100) {
  last_nf <- qsim(tmb = 40)$nf[7200]
  last_nb <- qsim(tmb = 40)$nb[7200]
  if (sum(last_nf,last_nb) != 0) {
    missing <- missing + 1
  }
}

prob <- missing/100
cat("The probability of at least one car missing the ferry departure is ", prob, ".", sep = "")


# Findings
# Comment on the impact of a longer processing time
# A longer processing time has an effect on the length of the queue. If minimum British processing time is 30 



