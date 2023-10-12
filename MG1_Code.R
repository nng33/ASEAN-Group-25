# Function to simulate M/G/1 queue for a fixed duration

simulate_MG1_fixed_duration <- function(arrival_rate, service_min, service_max) {
        simulation_duration <- 7200  # Duration in seconds of queue
        arrival_time <- 0  # Simulation clock
        starting_service <- numeric(0)  # Time in which the customer in queue starts to be served
        finishing_time <- numeric(0)  # Time in which the customer is finished being served
        
        while (arrival_time < simulation_duration) {
                TBA <- rexp(1, rate = arrival_rate) # Time before arrival 
                arrival_time <- arrival_time + TBA
                
                if (arrival_time < simulation_duration) {
                        # Simulate service time only if the arrival time is within the simulation duration
                        service_time <- runif(1, min = service_min, max = service_max)
                        
                        # time in which the customer is finished being served 
                        # is the sum of starting service time and actual service time
                        finishing_time <- c(finishing_time, arrival_time + service_time)
                        starting_service <- c(starting_service, arrival_time)
                }
        }
        
        # Calculate the time each customer spends waiting in line
        Wq <- finishing_time - starting_service
        
        # Return results
        return(list(starting_service = starting_service, 
                    finishing_time = finishing_time, 
                    Wq = Wq))
}

# Example simulation for 7200 seconds
set.seed(123)
simulation_results <- simulate_MG1_fixed_duration(arrival_rate = 0.1, service_min = 30, service_max = 70)

# Display results
cat("Arrival Times:", simulation_results$starting_service, "\n")
cat("Departure Times:", simulation_results$finishing_time, "\n")
cat("Response Times:", simulation_results$Wq, "\n")

# Display average waiting time
average_waiting_time <- mean(simulation_results$Wq, na.rm = TRUE)
cat("Average Waiting Time:", average_waiting_time, "\n")
