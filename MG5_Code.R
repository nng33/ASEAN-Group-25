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
