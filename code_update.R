qsim <- function(mf, mb, a.rate, trb, trf, tmb, tmf, maxb) {

  set.seed(11)
  
<<<<<<< HEAD
  # Initialize variables
        
  # Number of French arrivals
  french_arrivals <- rep(0,mf)
  # Create a vector representing the number of cars waiting in each respective French queue
=======
  # Initialize state variables: these values correspons to time t = 0
  
  # each column of french_arrivals represents number of car arriving 
  # at each French station at the beginning of time t
  french_arrivals <- rep(0,mf)
  
  # each column of french_queues represents the number of cars waiting 
  # at each French station's queue at the beginning of time t
>>>>>>> 9c7983843f743da0bd382e01af696fa750ad7ed2
  french_queues <- rep(0,mf)
  
  # each column of french_time represents the service time left for a customer
  # at each French station at the beginning of time t
  french_times <- rep(0,mf)
<<<<<<< HEAD
  # Is each French station processing a car? (1 if yes, 0 if no)
  french_busy <- rep(0,mf)
  # A leaving car (indicated by 1) will leave the station in the same simulation second
  french_exit <- rep(0,mf)
  # A vector that indicates if a car is unable to move forward because all British queues are full
  # 1 representing a stuck car and 0 otherwise, 1 will halt the entire processing time 
  french_stuck <- rep(0,mf)
  
  # Do the same for the British queues and British times 
  british_queues <- rep(0,mb)
  british_times <- rep(0,mb)
  # Is each British station processing a car? (1 if yes, 0 if no)
=======
  
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
>>>>>>> 9c7983843f743da0bd382e01af696fa750ad7ed2
  british_busy <- rep(0,mb)
  # Exit represents whether the a car is leaving the British station 
  # A leaving car (indicated by 1) will leave the station in the same simulation second
  british_exit <- rep(0,mb)
<<<<<<< HEAD
  # Initialize a vector indicating whether each British queue is less than maxb (1) or not (0) 
=======
  
  # each column of british_available indicates whether each station is 
  # available (1) or not (0) at the end of time t/beginning of t+1 
  # for containing a car arriving from the French's exit
>>>>>>> 9c7983843f743da0bd382e01af696fa750ad7ed2
  british_available <- rep(1,mb)
  
  #Initialize the empty vectors 
  nf <- numeric()
  nb <- numeric()
  eq <- numeric() 
  
  for (t in 1:7200) {
<<<<<<< HEAD
    # Reset number of arrivals for both British and French per simulation second
=======
    # Reset number of arrivals for both British and French at the start of loop
>>>>>>> 9c7983843f743da0bd382e01af696fa750ad7ed2
    french_arrivals <- rep(0,mf)
    british_arrivals <- rep(0,mf)
    
    # Start from the british stations
    for (i in 1:mb){
      
      # update arrivals at time t
      
      # arrivals depends on french_exit at t-1 and british_available at t-1
      #### ignoring possiblity of 2 cars exiting french at the same time
      if (sum(french_exit > 0)){
        british_arrivals[which.min(british_queues)] <- sum(french_exit)
      }
    }
      
      
      
      
      
      # update British availability at time t
      
      # British station is available if:
      # (1) number of its queue at t-1 < maximum queue length; or
      # (2) service time at t-1 == 1 since even if queue length at t-1 is maxb,
      #     at time t one person will exit and queue length at time t will be
      #     maxb - 1 < maxb
      
      if (british_queues[i] < maxb || british_times[i] == 1){
        british_available[i] <- 1
      }
      
      # otherwise, british station is not available
      else{
        british_available[i] <- 0
      }
      
      
      
  
    # Generate Poisson arrivals only within the first 90 minutes
    if (t <= 5400) {  # 60x90mins = 5400s
<<<<<<< HEAD
      # Acquire a binary variable indicating if a car has arrived (1) or not (0)
      num_arrivals <- as.integer(runif(1) < a.rate)
      # Assign the num_arrivals to the shortest French queues using which.min
      # Update to arrival number at time t
=======
      
      # the probability that a car arrives is a.rate
      # Let U be a random variable following Uniform(0,1)
      # Then, the Pr(U < a.rate) = a.rate
      # Therefore, a number simulated from Uniform(0,1) being less than a.rate
      # (the probability of this happening is a.rate) implies that a car arrives
      num_arrivals <- as.integer(runif(1) < a.rate)
      
      # Assign the num_arrivals to the shortest french queues using which.min
      # update to arrival number at time t
>>>>>>> 9c7983843f743da0bd382e01af696fa750ad7ed2
      french_arrivals[which.min(french_queues)] <- num_arrivals
      # french_queues[which.min(french_queues)] <- french_queues[which.min(french_queues)] + french_arrivals
      
    }
    
<<<<<<< HEAD
    # For each individual queue at the French border
    # Processing cars from the French station to the British station 
    for (i in 1:mf) {
      # Update busy vector 
=======
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
      
>>>>>>> 9c7983843f743da0bd382e01af696fa750ad7ed2
      if (french_times[i] > 1 || french_queues[i] > 0 || french_arrivals[i] == 1 || french_stuck[i] == 1){
        french_busy[i] <- 1
      }
      # otherwise, the station is not busy
      else{
        french_busy[i] <- 0
      }
      
<<<<<<< HEAD
      # Update French queues length
      # If either French time is greater than 1 or there is a car stuck in the queue
      # Increment the corresponding French queues by whether a French car has arrived
      if (french_times[i] > 1 || french_stuck[i] == 1) {
        french_queues[i] <- french_queues[i] + french_arrivals[i]
      }
      # Otherwise, check if a car is finished processing and is exiting 
      # If true, decrement it from the French queue plus any arrivals 
      # Else, if french_exit is < 0 (no need to specify this) don't do anything
      else if (french_exit[i] == 1) {
        french_queues[i] <- french_queues[i] + french_arrivals[i] - french_exit[i]
      }
      
      # Update service time by checking if the french_times is greater than 1 second
      # If true, decrease french_times for the cars being processed for each simulation second
      if (french_times[i] > 1) {
        french_times[i] <- french_times[i] - 1
      }
      # If french_times is equal to 1 and no cars are stuck waiting 
      # Generate a new processing time for the next car that begins processing
      else if (french_times[i] == 1 && french_stuck[i] == 0) {
        french_times[i] <- round(runif(1, tmf, tmf+trf),0)
      }
      # If both statements above are not true, reset french_times to zero because no cars are in the queue
      else {
        french_times[i] <- 0
      }
      
      # Update exit and transfer to British arrivals based on the two conditions
      # If conditions met, assign 1 to french_exit indicating that a car is leaving
      # Assign the cars that have left the French processing station to the shortest British queue
      if (french_times[i] == 1 && sum(british_available) > 0) {
=======
      # update french queues length to values at time t
      
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
        french_queues[i] <- french_queues[i] - french_exit[i] + french_arrivals[i]
      }
      
      # update service time
      
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
      
      # update exit state at time t
      
      # A car exit's value is recorded at time t when:
      # (1) if its service time at time t == 1 which implies that its service 
      #     will be finished at the end of time t and there is an available 
      #     spot at the British stations, then assign 1 to exit
           
      if (french_times[i] == 1 && sum(british_available) > 0){
>>>>>>> 9c7983843f743da0bd382e01af696fa750ad7ed2
        french_exit[i] <- 1
        
        
        # then, transfer the (transfer must be before exit time is updated)
        # british arrival +1 at time t when french exit at t-1 is 1
        # british_arrivals[which.min(british_queues)] <- french_exit[i]
      }
<<<<<<< HEAD
      else {
        french_exit[i] <- 0
      }
      
      # Update french_stuck by applying the following conditions
      # If French times are 1 or 0, the British queue capacity is reached and a car is being processed
      if (french_times[i] <= 1 && sum(british_available) == 0 && french_busy[i] == 1){
        french_stuck[i] <- 1
      }
      # Same condition as above except when the British queue capacity is not reached
      # No car will be stuck so french_stuck is zero and the car is reassigned to french_exit
      # Meaning that they will now be able to move to the shortest British queue
      else if(french_times[i] <= 1 && sum(british_available) > 0 && french_busy[i] == 1){
=======
      
      # (2) else if service time at t == 1 but there is no free space at the
      #     British station, the car doesn't exit (assign 0 to french_exit) but 
      #     it gets stuck for the first time instead (assign 1 to french_Stuck)
      else if(french_times[i] == 1 && sum(british_available) == 0){
        french_exit[i] <- 0
        french_stuck[i] <- 1
      }
      
      # (3) in all other cases, no cars will exit
      else{
        french_exit[i] <- 0
      }
      
      # update stuck at time t
      
      # (1) if a car is stuck at the beginning of time t (indicated by 0 service time left)
      #     at time t but station is still busy) and there's no space in the 
      #     British stations at the end of time t, then the car is still stuck
      if (french_times[i] == 0 && french_busy[i] == 1 && sum(british_available) == 0){
        french_stuck[i] <- 1
      }
      
      # (2) else if a car is stuck at the beginning of time t and a space in the
      #     British stations frees up at the end of time t, then the car is 
      #     no longer stuck (0) and may exit (assign 1 to french_exit)
      else if (french_times[i] == 0 && french_busy[i] == 1 && sum(british_available) > 0){
>>>>>>> 9c7983843f743da0bd382e01af696fa750ad7ed2
        french_stuck[i] <- 0
        french_exit[i] <- 1
      }
      
      # in all other cases, no car is stuck
      else{
        french_stuck <- 0
      }
    }
    
    # Now handling the cars that enter the British Station
    for (j in 1:mb) {
      # Update British busy vector
      if (british_times[j] > 1 || british_queues[j] > 0 || british_arrivals[j] == 1) {
        british_busy[j] <- 1
      }
      else {
        british_busy[j] <- 0      
      }
      
      # Update British queues            
      if (british_times[j] > 1 && british_queues[j] <= 20) {
        british_queues[j] <- british_queues[j] + british_arrivals[j]
      }
      else if (british_exit[j] == 1) {
        british_queues[j] <- british_queues[j] + british_arrivals[j] - british_exit[j]
      }
      
      # Update service times            
      if (british_times[j] > 1) {
        british_times[j] <- british_times[j] - 1
      }
      # Apply service time to the new car
      else if (british_times[j] == 1) {
        british_times[j] <- round(runif(1, tmb, tmb+trb),0)
      }
      # If both statements above are not true, reset British_times to zero because no cars are in the queue
      else {
        british_times[j] <- 0
      }      
      
      # Update whether a processing car in the British station exits         
      if (british_times[j] == 1) {
        british_exit[j] <- 1
      }
      else {
        british_exit[j] <- 0
      }
    }
      # british side
      # british_arrivals already at time t

<<<<<<< HEAD
      
###################################################################################################
      
      if (french_times[i] < 0) { # 
        french_times[i] <- 0 # Assign french time for each station to 0 
        # Condition checks if the total sum of all British queues is less than the total capacity 
        if(sum(british_queues) < (mb * maxb)) {
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
      if (french_queues[i] > 0 && french_times[i] == 0) {
        # store a uniformly generated processing time for each car of the front car in each respective queue
        french_times[i] <- round(runif(1, tmf, tmf + trf), 0)
      }
    }
    
=======
>>>>>>> 9c7983843f743da0bd382e01af696fa750ad7ed2
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
    eq[t] <- sum(french_queues) * mean(french_times[french_times >= 0])
    
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

