# update stuck at time t

# (1) if a car is stuck at the beginning of time t (indicated by 0 service time left)
#     at time t but station is still busy) and there's no space in the 
#     British stations at the end of time t, then the car is still stuck
if (french_times[i] == 0 && french_busy[i] == 1 && sum(british_available) == 0){
  french_stuck[i] <- 1
}


# (2) else if a car is stuck at the beginning of time t and a space in the British stations
#     frees up at the end of time t (that is larger than or equal to the number of cars being stuck), then the car is 
#     no longer stuck (0) and may exit (assign 1 to french_exit)
##else if (french_times[i] == 0 && french_busy[i] == 1 && sum(british_available) > 0){
#  french_stuck[i] <- 0
#  french_exit[i] <- 1
#}

else if (french_times[i] == 0 && french_busy[i] == 1 && sum(british_available) > 0 && (mb*maxb-sum(british_queues)) >= sum(french_stuck)) {
  french_stuck[i] <- 0
  french_exit[i] <- 1
}

# (3) else if a car is stuck at the beginning of time t but the number of spaces in the British stations
#     is smaller than the number of cars being stuck), then some cars is 
#     no longer stuck (0) and may exit (assign 1 to french_exit)
#     other cars that can't move to the British stations are stuck again (going back to the condition(1)) 

else if (french_times[i] == 0 && french_busy[i] == 1 && sum(british_available) > 0 && (mb*maxb-sum(british_queues)) < sum(french_stuck)) {
  if (british_queues[which.min(british_queues)] == maxb-1) {
    british_available[which.min(british_queues)] <- 0
    british_queues[which.min(british_queues)] <- british_queues[which.min(british_queues)] + 1
  }
  else {
    british_queues[which.min(british_queues)] <- british_queues[which.min(british_queues)] + 1
  }
  french_stuck[i] <- 0
  french_exit[i] <- 1
}

# in all other cases, no car is stuck
else{
  french_stuck[i] <- 0
}


missing <- 0
for (i in 1:100) {
  qsim
  if (sum(british_busy) > 0) {
    missing <- missing + 1
  }
}
prob_missing <- missing / 100





qsim <- function(mf, mb, a.rate, trb, trf, tmb, tmf, maxb) {
  
  set.seed(11)
  
  # Initialize state variables
  # Create a vector representing the number of cars waiting in each respective french queue
  french_queues <- rep(0,mf)
  # Create a vector corresponding to the processing time of the front car in each respective French queue
  # Initialize this at 0 to indicate that there is no processing time elapsed for any car in the queue
  french_times <- rep(0,mf)
  # If the person is in the station, this vector gets 1
  french_busy <- rep(0,mf)
  # Do the same for the British queues and time 
  british_queues <- rep(0,mb)
  british_times <- rep(0,mb)
  #Initialize the empty vectors 
  nf <- numeric()
  nb <- numeric()
  eq <- numeric() 
  
  for (t in 1:7200) {
    # Generate Poisson arrivals only within the first 90 minutes
    if (t <= 5400) {  # 60x90mins = 5400s
      num_arrivals <- round(rpois(1, a.rate), 0)
      # Assign the num_arrivals to the shortest french queues using which.min 
      french_queues[which.min(french_queues)] <- french_queues[which.min(french_queues)] + num_arrivals
      french_pref <- which.min(french_queues)
      # For each individual queue at the French border
      # Processing cars from the french station to the British station 
      for (i in 1:mf) {
        # We want to update the number of people in queue
        if (french_times[i] > 0) {
          french_times[i] <- french_times[i] - 1
        }
        
        if (i == french_pref) {
          french_queues[i] <- 
        }
     
        
        
        if (french_queues[i] == 0 && french_times[i] == 0) { 
          french_queues[i] = 0
          french_times[i] = 0
        }
        if (french_queues[i] > 0 && french_times[i] == 0) { 
          # Condition 
          if(sum(british_queues) >= (mb * maxb)) {
            french_times[i] <- 0
          }
          if(sum(british_queues) < (mb * maxb)) {
            # store a uniformly generated processing time for each car of the front car in each respective queue
            french_times[i] <- round(runif(1, tmf, tmf + trf), 0)
            # If condition met, a processed car in the french queue will move into the British queue
            # Need to check if the french car has actually been processed
            # Reduce the length for a selected French queue
            french_queues[i] <- french_queues[i] - 1
            # Store the indexes of British queues that are available 
            available_british <- which(british_queues < maxb)
            # Assign the processed french car to the shortest British queue
            # Do this by applying the which.min to the available British queues 
            # Finally, apply the index to obtain the shortest British queue
            target_british <- available_british[which.min(british_queues[available_british])]
            # Update the shortest British queue by 1
            british_queues[target_british] <- british_queues[target_british] + 1
          }
          
        }
        
        # Condition 1: Check whether the french queue has cars waiting and
        # Condition 2: If the processing time for the front car in the queue has elapsed 
        # If both conditions are true, the French queue is not empty and the front car has been processed
        #if (french_queues[i] > 0 && french_times[i] == 0) { 
          # store a uniformly generated processing time for each car of the front car in each respective queue
          #french_times[i] <- round(runif(1, tmf, tmf + trf), 0)
        }
      }
      
      # British station  
      # If the processing time for the front car in the British queue is less than 0 
      # Reset the processing time to 0 and remove one car from that queue  
      # Because it has already finished being processed 
      for (i in 1:mb) {
        if(british_times[i] < 0) {
          british_times[i] <- 0
          british_queues[i] <- british_queues[i]-1
        }
        
        # However, if the queue is not empty and the British time is equal to zero
        # apply a service time based on the uniform distribution 
        if (british_queues[i] > 0 && british_times[i] == 0) {
          british_times[i] <- round(runif(1,tmb, tmb + trb), 0)
        }
      }
      
      # Expected waiting time at the start of the French queue
      eq[t] <- sum(french_queues) * mean(french_times[which(french_times >= 0)])  # ??????
      
      # Record queue lengths
      nf[t] <- round(mean(french_queues),0)
      nb[t] <- round(mean(british_queues),0)
      
      
      # Update processing times
      for (i in 1:mf) {
        if (french_times[i] > 0){
          french_times[i] <- french_times[i] - 1
        }
      }
      
      for (i in 1:mb) {
        if (british_times[i] > 0){
          british_times[i] <- british_times[i] - 1
        }
      }
      t <- t + 1
    }
  }
  return(list(nf = nf, nb = nb, eq = eq))
}

sim <- qsim(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20); sim
sim_40 <- qsim(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=40,tmf=30,maxb=20); sim_40

tail(sim_40$nf, n = 1000)

# Plot
# Set up a 2x2 grid for plots
par(mfrow = c(2, 2))  

# Plot 1: French and British queue lengths over time
plot(1:7200, sim$nf, type = "l", xlab = "Time (seconds)", ylab = "French Queue Length")
lines(1:7200, sim$nb, col = "red")
# legend("topright", legend = c("French Queue", "British Queue"), col = c("black", "red"), lty = 1)

# Plot 2: Expected queuing time over time
plot(1:7200, sim$eq, type = "l", xlab = "Time (seconds)", ylab = "Expected Queuing Time")

# Plot 3: French and British queue lengths over time
plot(1:7200, sim_40$nf, type = "l", xlab = "Time (seconds)", ylab = "French Queue Length")
lines(1:7200, sim_40$nb, col = "red")
# legend("topright", legend = c("French Queue", "British Queue"), col = c("black", "red"), lty = 1)

# Plot 4: Expected queuing time over time
plot(1:7200, sim_40$eq, type = "l", xlab = "Time (seconds)", ylab = "Expected Queuing Time")





##########################################################################################################





# Simulate M/G/5
MG5 <- function(mf, a.rate, trf, tmf){
  # time period
  t.end <- 7200
  
  # Check-in period
  checkin_end <- 5400
  
  # Initialise value
  
  # Time between arrival
  tba <- 0
  
  # Arrival time
  arrival_time <- 0
  
  # all station are still empty. Pick any station
  french_number <- 0
  
  # for each station, first col is start time, second col is service time,
  # third col is end time
  # forth col is the number of people in the queue
  french_station <- list(station_1 = matrix(0,1,4),
                         station_2 = matrix(0,1,4),
                         station_3 = matrix(0,1,4),
                         station_4 = matrix(0,1,4),
                         station_5 = matrix(0,1,4)
  )
  
  # waiting time for each customer
  wait <- 0
  
  # start simulation
  i <- 1 # previous customer
  ### have to change to max(arrival times) < 1h30min
  while (max(french_station[[1]][i,3],french_station[[2]][i,3],french_station[[3]][i,3],
             french_station[[4]][i,3],french_station[[5]][i,3], na.rm=TRUE) < t.end){
    
    # inter-arrival time
    tba_entry <- max(1,round(rexp(1, rate = a.rate),0)) # deal with integer seconds only
    tba <- rbind(tba, tba_entry)
    
    # arrival item
    arrival_time_entry <- tba_entry + arrival_time[i]
    arrival_time <- rbind(arrival_time, arrival_time_entry)
    
    # choose which station to go
    shortest_qlen <- min(french_station[[1]][i,4],french_station[[2]][i,4],french_station[[3]][i,4],
                         french_station[[4]][i,4],french_station[[5]][i,4],na.rm=TRUE)
    for (j in 1:mf){
      if (length(which(french_station[[j]][i,4] == shortest_qlen) != 0)){
        french_number_entry <- j
        french_number <- rbind(french_number, french_number_entry)
        break
      }
    }
    
    # station j has the shortest queue length
    # only fill in relevant station, j
    
    # start time is as soon as you arrive or as soon as previous customer is finished
    start_time <- max(arrival_time_entry,french_station[[j]][1:i,3],na.rm=TRUE)
    service_time <- round(runif(1, min = tmf, max = tmf + trf),0)
    end_time <- start_time + service_time
    
    # number in queue for each station
    qnum <- NULL
    for (k in 1:mf){
      qnum_entry <- sum(french_station[[k]][1:nrow(french_station[[k]]),1] > arrival_time_entry, na.rm=TRUE)
      if (k == french_number_entry & start_time > arrival_time_entry){
        qnum_entry <- qnum_entry + 1
      }
      qnum <- rbind(qnum, qnum_entry)
    }
    
    # entry for current customer
    entry <- c(start_time, service_time, end_time, qnum[french_number_entry])
    french_station[[j]] <- rbind(french_station[[j]], entry)
    
    # insert blank rows for the rest of the stations
    for (k in 1:mf){
      if (k != j){
        french_station[[k]] <- rbind(french_station[[k]],c(NA,NA,NA,qnum[k]))
      }
    }
    
    wait_entry <- start_time - arrival_time_entry
    wait <- rbind(wait, wait_entry)
    
    i <- i + 1
  }
  
  #nf <- 0
  #for (p in 2:t.end){
   # nf <- rbind(nf, (french_station[[1]][p,4] + french_station[[2]][p,4] + french_station[[3]][p,4] +
    #              french_station[[4]][p,4] + french_station[[5]][p,4])/mf )
  
  #eq <- 
  return(french_station) # We wanto to return nf and eq
}

a <- MG5(mf=5, a.rate=0.1, trf=30, tmf=70)




#################################################################################################




# Function to simulate M/G/5 queue for a fixed duration

simulate_MG5 <- function(mf, a.rate, trf, tmf) {
        sim_duration <- 7200  # Duration in seconds of queue
        checkin_time <- 5400  # The time the customer need to check-in 
        station <- matrix(0, 2, mf)
        a.time <- c(0)  # Arrival time
        starting_service <- c(0)  # Time in which the customer in queue starts to be served
        wait_time <- c(0) # Time in which the customer in queue has to wait
        finishing_time <- c(0)  # Time in which the customer is finished being served
        while (tail(a.time,1) < checkin_time) {
                TBA <- rexp(1, rate = a.rate) # Time between arrival 
                a.time <- c(a.time, a.time + TBA) # Arrival time
                service_time <- runif(1, min = tmf, max = tmf + trf) # Simulate service time
                first <- which(station[1,] == min(station[1,]))[1] # Station that finishes first
                if (tail(a.time, 1) > station[1,first]) {
                  station[2,first] <- station[2,first] + tail(a.time,1) - station[1,first]
                  wait_time <- c(wait_time, 0)
                } 
                else {
                  wait_time <- c(wait_time, station[1,first] - tail(a.time,1))
                }
                finishing_time <- c(finishing_time, tail(a.time,1) + tail(wait_time,1) + service_time)
                station[1,first] <- tail(finishing_time,1)
        }
        
        # Return results
        return(list(finishing_time = finishing_time, 
                    wait_time = wait_time))
    }

# Example simulation for 7200 seconds
set.seed(10)
sim_results <- simulate_MG5(mf=5, a.rate=0.1, trf=30, tmf=70)

# Display results
cat("Arrival Times:", sim_results$starting_service, "\n")
cat("Departure Times:", sim_results$finishing_time, "\n")
cat("Response Times:", sim_results$Wq, "\n")

# Display average waiting time
average_waiting_time <- mean(sim_results$Wq, na.rm = TRUE)
cat("Average Waiting Time:", average_waiting_time, "\n")
