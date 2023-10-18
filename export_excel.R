set.seed(1)

qsim <- function(mf, mb, a.rate, trb, trf, tmb, tmf, maxb){
  
  # Initialize state variables: these values correspons to time t = 0
  
  # each column of french_arrivals represents number of car arriving 
  # at each French station at the beginning of time t
  french_arrivals <- rep(0,mf)
  fa <- french_arrivals
  # each column of french_queues represents the number of cars waiting 
  # at each French station's queue at the beginning of time t
  french_queues <- rep(0,mf)
  fq <- french_queues
  # each column of french_time represents the service time left for a customer
  # at each French station at the beginning of time t
  french_times <- rep(0,mf)
  ft <- french_times
  # each column of french_busy indicates whether each 
  # French station is busy (1) or not (0) at the beginning of time t
  french_busy <- rep(0,mf)
  fb <- french_busy
  # each column of french_exit indicates whether there's an exit (1) or not (1)
  # at each station at the end of time t/beginning of time t+1 
  french_exit <- rep(0,mf)
  fe <- french_exit
  # each column of french_stuck indicates whether there's a car stuck at 
  # the corresponding station (1) or not (0) at the end of time t/
  # beginning of time t+1. 
  french_stuck <- rep(0,mf)
  fs <- french_stuck
  # Do the same for the British queues and time
  british_arrivals <- rep(0,mb)
  ba <- british_arrivals
  
  british_queues <- rep(0,mb)
  bq <- british_queues
  
  british_times <- rep(0,mb)
  bt <- british_times
  
  british_busy <- rep(0,mb)
  bb <- british_busy
  
  british_exit <- rep(0,mb)
  be <- british_exit
  
  # each column of british_available indicates whether each station is 
  # available (1) or not (0) at the end of time t/beginning of t+1 
  # for containing a car arriving from the French's exit
  british_available <- rep(1,mb)
  bav <- british_available
  
  #Initialize the empty vectors 
  nf <- numeric()
  nb <- numeric()
  eq <- numeric() 
  
  for (t in 1:7200) {
    # Reset number of arrivals for both British and French at the start of loop
    french_arrivals <- rep(0,mf)
    british_arrivals <- rep(0,mf)
    
    # Start from the british stations
    for (i in 1:mb){
      
      # update British availability at time t
      
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
      
      # update british arrivals at time t
      
      # arrivals depends on french_exit at t-1
      # if a car manages to exit at t-1, it already checked availability of 
      # British stations
      #### ignoring possiblity of 2 cars exiting french at the same time
    for (i in 1:mb){ 
      british_num_arrivals <- sum(french_exit)
      if (british_num_arrivals > 0){
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
    
    ba <- rbind(ba, british_arrivals)
    bq <- rbind(bq, british_queues)
    bt <- rbind(bt, british_times)
    bb <- rbind(bb, british_busy)
    be <- rbind(be, british_exit)
    bav <- rbind(bav, british_available)
      
    ## French side
    
    # Generate Poisson arrivals only within the first 90 minutes
    if (t <= 5400) {  # 60x90mins = 5400s
      
      # the probability that a car arrives is a.rate
      # Let U be a random variable following Uniform(0,1)
      # Then, the Pr(U < a.rate) = a.rate
      # Therefore, a number simulated from Uniform(0,1) being less than a.rate
      # (the probability of this happening is a.rate) implies that a car arrives
      num_arrivals <- as.integer(runif(1) < a.rate)
      
      # Assign the num_arrivals to the shortest french queues using which.min
      # update to arrival number at time t
      french_arrivals[which.min(french_queues)] <- num_arrivals
      # french_queues[which.min(french_queues)] <- french_queues[which.min(french_queues)] + french_arrivals
      
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
      
      # update exit state at time t
      
    for (i in 1:mf){
      # A car exit's value is recorded at time t when:
      # (1) if its service time at time t == 1 which implies that its service 
      #     will be finished at the end of time t and there is an available 
      #     spot at the British stations, then assign 1 to exit
           
      if (french_times[i] == 1 && sum(british_available) > 0){
        french_exit[i] <- 1
        
        
        # then, transfer the (transfer must be before exit time is updated)
        # british arrival +1 at time t when french exit at t-1 is 1
        # british_arrivals[which.min(british_queues)] <- french_exit[i]
      }
      
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
    }
      
      # update stuck at time t
    for (i in 1:mf){
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
        french_stuck[i] <- 0
        french_exit[i] <- 1
      }
      
      # in all other cases, no car is stuck
      else{
        french_stuck[i] <- 0
      }
    }
    fa <- rbind(fa, french_arrivals)
    fq <- rbind(fq, french_queues)
    ft <- rbind(ft, french_times)
    fb <- rbind(fb, french_busy)
    fe <- rbind(fe, french_exit)
    fs <- rbind(fs, french_stuck)
      
    # Expected waiting time at the start of the French queue
    eq[t] <- mean(french_queues) * mean(french_times)
    
    # Record queue lengths
    nf[t] <- mean(french_queues)
    nb[t] <- mean(british_queues)
  }
  
  return(list(nf = nf, nb = nb, eq = eq, 
              ba = ba, bq = bq, bt = bt, bb = bb, be = be, bav = bav,
              fa = fa, fq = fq, ft = ft, fb = fb, fe = fe, fs = fs))
}

sim <- qsim(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20)


sim
sim_40 <- qsim(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=40,tmf=30,maxb=20)
sim_40

simt <- qsim(mf=5,mb=5,a.rate=0.9,trb=40,trf=5,tmb=100,tmf=5,maxb=2)

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



ba = ba, bq = bq, bt = bt, bb = bb, be = be, bav = bav,
fa = fa, fq = fq, ft = ft, fb = fb, fe = fe, fs = fs))

output <- cbind(sim$fa,sim$fq,sim$ft,sim$fb,sim$fe,sim$fs,
      sim$ba,sim$bq,sim$bt,sim$bb,sim$be,sim$bav)
df <- as.data.frame(output)
df <- data.frame(f_arrival = sim$fa, f_queue = sim$fq, f_time = sim$ft, f_busy = sim$fb, 
                  f_exit =sim$fe, f_stuck = sim$fs, b_arrival = sim$ba, b_queue = sim$bq, 
                  b_time = sim$bt, b_busy = sim$bb, b_exit = sim$be, b_available = sim$bav)

df40 <- data.frame(f_arrival = sim_40$fa, f_queue = sim_40$fq, f_time = sim_40$ft, f_busy = sim_40$fb, 
                 f_exit =sim_40$fe, f_stuck = sim_40$fs, b_arrival = sim_40$ba, b_queue = sim_40$bq, 
                 b_time = sim_40$bt, b_busy = sim_40$bb, b_exit = sim_40$be, b_available = sim_40$bav)


dft <- data.frame(f_arrival = simt$fa, f_queue = simt$fq, f_time = simt$ft, f_busy = simt$fb, 
                 f_exit =simt$fe, f_stuck = simt$fs, b_arrival = simt$ba, b_queue = simt$bq, 
                 b_time = simt$bt, b_busy = simt$bb, b_exit = simt$be, b_available = simt$bav)


library("writexl")
write_xlsx(df,"M:\\data1.xlsx")
write_xlsx(df40,"M:\\data40.xlsx")
write_xlsx(dft,"M:\\datat.xlsx")







