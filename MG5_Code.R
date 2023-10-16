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
