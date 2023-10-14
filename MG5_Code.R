# Function to simulate M/G/5 queue for a fixed duration

simulate_MG5 <- function(mf, a.rate, trf, tmf) {
        sim_duration <- 7200  # Duration in seconds of queue
        checkin_time <- 5400  #
        station <- matrix(0, 1, 5)
        counter <- 0 # Count until time 7200
        a.time <- 0  # Arrival time
        starting_service <- numeric(0)  # Time in which the customer in queue starts to be served
        finishing_time <- numeric(0)  # Time in which the customer is finished being served
        while (counter < sim_duration) {
            while (a.time < checkin_time) {
                    TBA <- rexp(1, rate = a.rate) # Time before arrival 
                    a.time <- a.time + TBA
                    # Simulate service time
                    service_time <- runif(1, min = tmf, max = tmf + trf)
                    
                    # time in which the customer is finished being served 
                    # is the sum of starting service time and actual service time
                    finishing_time <- c(finishing_time, a.time + service_time)
                    starting_service <- c(starting_service, a.time)
                    
            }
            
            # Calculate the time each customer spends waiting in line
            Wq <- finishing_time - starting_service
            
            # Return results
            return(list(starting_service = starting_service, 
                        finishing_time = finishing_time, 
                        Wq = Wq))
        }
    }

# Example simulation for 7200 seconds
set.seed(123)
sim_results <- simulate_MG5(a.rate = 0.1, service_min = 30, service_max = 70)

# Display results
cat("Arrival Times:", sim_results$starting_service, "\n")
cat("Departure Times:", sim_results$finishing_time, "\n")
cat("Response Times:", sim_results$Wq, "\n")

# Display average waiting time
average_waiting_time <- mean(sim_results$Wq, na.rm = TRUE)
cat("Average Waiting Time:", average_waiting_time, "\n")
