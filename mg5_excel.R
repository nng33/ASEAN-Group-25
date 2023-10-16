mg5 <- function(){
  # Simulate M/G/2
  
  # set parameters:
  
  # arrival rate per second
  a.rate <- 0.1
  
  # service rate:
  # French ~ uniform (tmf, tmf+trf) in seconds
  tmf <- 30
  trf <- 40
  
  # time period
  t.end <- 3600*2
  
  # number of french station
  mf <- 5
  
  # Initialise value
  
  # Interarrival time
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
  # change to max(arrival times) < 1h30min
  while (max(french_station[[1]][i,3],french_station[[2]][i,3],french_station[[3]][i,3],
             french_station[[4]][i,3],french_station[[5]][i,3], na.rm=TRUE) < t.end){
    
    # interarrival time
    tba_entry <- max(1,round(rexp(1, rate = a.rate),0)) # deal with integer seconds only
    tba <- rbind(tba, tba_entry)
    
    # arrival item
    arrival_time_entry <- tba_entry + arrival_time[i]
    arrival_time <- rbind(arrival_time, arrival_time_entry)
    
    # choose which server to go
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
    
    # start time is as soon as you arrive or as soon as previous customer is 
    # finished
    start_time <- max(arrival_time_entry,french_station[[j]][1:i,3],na.rm=TRUE)
    service_time <- round(runif(1, min = tmf, max = tmf + trf),0)
    end_time <- start_time + service_time
    
    # number in q for each station
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
  return(french_station)
}

