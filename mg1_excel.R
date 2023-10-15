# Simulate M/G/1

# set parameters:

# arrival rate per second
lambda <- 0.1


# service rate is uniform (tmf, tmf+trf) in seconds
tmf <- 30
trf <- 40

# time period
t.end <- 3600*2


## Simulate for customer 1

# inter arrival time
tba <- max(1,round(rexp(1, rate = lambda),0))

arrival_time <- tba

# service time should be at least 1 second and rounded to the nearest integer
service_time <- max(1,round(runif(1, min = tmf, max = tmf + trf),0))

# no queue for customer 1 so just go immediately to the server
time_service_begins <- arrival_time

# waiting time in queue
qtime <- 0

time_service_ends <- time_service_begins + service_time

# number in queue
qnum <- 0

## Simulate new customers until time service ends < 7200

i <- 1
while (time_service_ends[i] < t.end){
  # generate exponentially distributed interarrival time
  tba_entry <- max(1,round(rexp(1, rate = lambda),0))
  tba <- rbind(tba, tba_entry)
  
  # arrival time for currrent customer is the arrival time of previous customer
  # plus the interarrival time of customer i
  arrival_time_entry <- arrival_time[i] + tba_entry
  arrival_time <- rbind(arrival_time, arrival_time_entry)
  
  # service time is U(tmf, tmf + trf)
  service_time_entry <- max(1,round(runif(1, min = tmf, max = tmf + trf),0))
  service_time <- rbind(service_time, service_time_entry)
  
  # service begins either as soon as the previous customer is finished or
  # as soon as the current customer arrives
  time_service_begins_entry <- max(arrival_time_entry, time_service_ends[i])
  time_service_begins <- rbind(time_service_begins, time_service_begins_entry)
  
  # time in queue for current customer is the amount of time fromm their
  # arrival until their service begins
  qtime_entry <- time_service_begins_entry - arrival_time_entry
  qtime <- rbind(qtime, qtime_entry)
  
  # time service ends is the service start time + service time
  time_service_ends_entry <- time_service_begins_entry + service_time_entry
  time_service_ends <- rbind(time_service_ends, time_service_ends_entry)
  
  # number of people in queue including current customer is 
  # the number of customers whose service begins later than the current 
  # customer's arrival time
  
  # you can think of the current customer's arrival time as the current time
  # and everyone whose service starts later than current time means that 
  # they are queueing
  qnum_entry <- sum(time_service_begins[1:length(time_service_begins)] > arrival_time_entry)
  qnum <- rbind(qnum, qnum_entry)
  
  i <- i + 1
  
}

# input all data into data frame
output <- data.frame(interarrival_times = numeric(tba),
                     arrival_time = numarrival_time,
                     service_time = service_time,
                     time_service_begins = time_service_begins,
                     waiting_time_in_queue = qtime,
                     time_service_ends = time_service_ends,
                     number_in_queue = qnum)
  
output













