qsim <- function(mf, mb, a.rate, trb, trf, tmb, tmf, maxb) {

  set.seed(11)
  
  # Initialize state variables
  # Create a vector representing the number of cars waiting in each respective french queue
  french_queues <- rep(0,mf)
  # Creates a vector corresponding to the processing time of the front car in each respective French queue
  # Initialize this at 0 to indicate that there is no processing time elapsed for any car in the queue
  french_times <- rep(0,mf)
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
    }
    
    # For each individual queue at the French border
    # Processing cars from the french station to the British station 
    for (i in 1:mf) {
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
    nf[t] <- mean(french_queues)
    nb[t] <- mean(british_queues)
    
    
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

