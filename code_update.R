qsim <- function(mf, mb, a.rate, trb, trf, tmb, tmf, maxb) {

  set.seed(11)
  
  # Initialize variables
        
  # Number of French arrivals
  french_arrivals <- rep(0,mf)
  # Create a vector representing the number of cars waiting in each respective French queue
  french_queues <- rep(0,mf)
  # Create a vector corresponding to the processing time of the front car in each respective French queue
  # Initialize this at 0 to indicate that there is no processing time elapsed for any car in the queue
  french_times <- rep(0,mf)
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
  british_busy <- rep(0,mb)
  # Exit represents whether the a car is leaving the British station 
  # A leaving car (indicated by 1) will leave the station in the same simulation second
  british_exit <- rep(0,mb)
  # Initialize a vector indicating whether each British queue is less than maxb (1) or not (0) 
  british_available <- rep(1,mb)
  
  #Initialize the empty vectors 
  nf <- numeric()
  nb <- numeric()
  eq <- numeric() 
  
  for (t in 1:7200) {
    # Reset number of arrivals for both British and French per simulation second
    french_arrivals <- rep(0,mf)
    british_arrivals <- rep(0,mf)
  
    # Generate Poisson arrivals only within the first 90 minutes
    if (t <= 5400) {  # 60x90mins = 5400s
      # Acquire a binary variable indicating if a car has arrived (1) or not (0)
      num_arrivals <- as.integer(runif(1) < a.rate)
      # Assign the num_arrivals to the shortest French queues using which.min
      # Update to arrival number at time t
      french_arrivals[which.min(french_queues)] <- num_arrivals
      # french_queues[which.min(french_queues)] <- french_queues[which.min(french_queues)] + french_arrivals
      
    }
    
    # For each individual queue at the French border
    # Processing cars from the French station to the British station 
    for (i in 1:mf) {
      # Update busy vector 
      if (french_times[i] > 1 || french_queues[i] > 0 || french_arrivals[i] == 1 || french_stuck[i] == 1){
        french_busy[i] <- 1
      }
      else{
        french_busy[i] <- 0
      }
      
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
        french_exit[i] <- 1
        british_arrivals[which.min(british_queues)] <- french_exit[i]
      }
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
        french_stuck[i] <- 0
        french_exit[i] <- 1
        british_arrivals[which.min(british_queues)] <- french_exit[i]
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

