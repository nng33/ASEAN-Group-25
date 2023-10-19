## Group 25
## Frans : s2591760
## Daiki : s2547603
## Nathan : s2524152


## Contribution to this project
## Frans (%):  Daiki (%):  Nathan (%): 


#######################################################################################################################


# setwd("C:/Users/maruw/ASEAN-Group-25") ## set the working directory

# qsim function
qsim <- function(mf, mb, a.rate, trb, trf, tmb, tmf, maxb) {
  
  set.seed(11)
  
  
}

sim <- qsim(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20); sim
sim_40 <- qsim(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=40,tmf=30,maxb=20); sim_40


# Plot
# Set up a 2x2 grid for plots
par(mfrow = c(2, 2))  

# Plot 1: French and British queue lengths over time
plot(1:7200, sim$nf, type = "l", xlab = "Time (seconds)", ylab = "French Queue Length")
lines(1:7200, sim$nb, col = "red")
legend("topright", legend = c("French Queue", "British Queue"), col = c("black", "red"), lty = 1)

# Plot 2: Expected queuing time over time
plot(1:7200, sim$eq, type = "l", xlab = "Time (seconds)", ylab = "Expected Queuing Time")

# Plot 3: French and British queue lengths over time
plot(1:7200, sim_40$nf, type = "l", xlab = "Time (seconds)", ylab = "French Queue Length")
lines(1:7200, sim_40$nb, col = "red")
legend("topright", legend = c("French Queue", "British Queue"), col = c("black", "red"), lty = 1)

# Plot 4: Expected queuing time over time
plot(1:7200, sim_40$eq, type = "l", xlab = "Time (seconds)", ylab = "Expected Queuing Time")


# Estimation
# Calculate the probability that at least one car miss the ferry departure
missing <- 0
for (i in 1:100) {
  qsim(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=40,tmf=30,maxb=20)
  if (sum(busy) > 0) {
    missing <- missing + 1
  }
}


# Findings
# Comment on the impact of a longer processing time



