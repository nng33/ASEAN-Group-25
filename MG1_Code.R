# Function to simulate M/G/1 queue for a fixed duration

qsim <- function(mf = 5, arrival_rate, service_min, service_max) {
        simulation_duration <- 7200  # Duration in seconds of queue
        mf_queue <- vector("list", length = mf) # A list to store data on 5 queues
        
        for (i in 1:mf) {
                arrival_time <- 0  # Time stamp the first customer arrives 
                starting_service <- numeric(0)  # Initialize the time stamp in which the customer in queue starts to be served
                finishing_time <- numeric(0)  # Time stamp in which the customer is finished being served
        
        while (arrival_time < simulation_duration) {
                # Time before the next customer arrives
                TBA <- rexp(1, rate = arrival_rate) 
                # Time stamp for the next arriving customer (previous arrival time + previous time before arrival)
                arrival_time <- arrival_time + TBA 
                
                # Simulate service time only if the arrival time is within the simulation duration
                if (arrival_time < simulation_duration) {
                        # Time it takes to service a customer
                        service_time <- runif(1, min = service_min, max = service_max)
                        
                        # Time stamp in which the customer is finished being served 
                        # This is the sum of time stamp for starting service and the simulated service time
                        finishing_time <- c(finishing_time, arrival_time + service_time)
                        # Time stamp in which the next customer will be served 
                        starting_service <- c(starting_service, arrival_time)
                        }
                }
        
        # Calculate the time each customer spends waiting in line
        Wq <- finishing_time - starting_service
        
        # Average length of french queues for each simulation second
        
        mf_queue[[i]] <- list(starting_service = starting_service, 
                               finishing_time = finishing_time, 
                               Wq = Wq)
        }
        # Display average waiting time
        for (i in 1:mf) {
                cat("Queue", i, "Average Waiting Time:", mean(mf_queue[[i]]$Wq, na.rm = TRUE), "\n")
        }
        print(length(mf_queue[[2]]$starting_service))
        #print(mf_queue[[2]]$starting_service)
        
}

qsim(mf = 5, arrival_rate = 0.1, service_min = 30, service_max = 70)

# Display results
# cat("Arrival Times:", simulation_results$starting_service, "\n")
# cat("Departure Times:", simulation_results$finishing_time, "\n")
# cat("Response Times:", simulation_results$Wq, "\n")
