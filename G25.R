## Frans : s2591760
## Daiki : s2547603
## Nathan : s2524152


## Contribution to this project
## Frans (%): Handled 
## Daiki (%): Handled 
## Nathan (%): Handled 


################################################################################




################################################################################


#
netup <- function(d){
  h <- list()
  W <- list()
  b <- list()
  
  for (i in 1:length(d)){
    vec <- rep(c(1), times = length(d[i]))
    h[[i]] <- vec
  }
  
  for (j in 1:length(d)-1){
    W[[j]] <- matrix(1, length(d[j]), length(d[j+1])) + runif(min = 0, max = 0.2)
  }
  
  for (k in 1:length(d)){
    vec <- rep(c(1), times = length(d[i]))
    h[[i]] <- vec
  }
  
  return(list(h = h, W = W, b = b))
}
