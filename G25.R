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
    vec <- rep(c(0), times = d[i])
    h[[i]] <- vec
  }
  
  for (j in 1:length(d)-1){
    W[[j]] <- matrix(runif(d[j] * d[j+1], min = 0, max = 0.2), 
                     d[j], d[j+1])
  }
  
  for (k in 1:length(d)){
    vec <- rep(c(1), times = length(d[i]))
    h[[i]] <- vec
  }
  
  return(list(h = h, W = W, b = b))
}

# Test case for netup
d <- c(4,8,7,3)
netup(d)
