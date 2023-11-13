## Frans : s2591760
## Daiki : s2547603
## Nathan : s2524152


## Contribution to this project
## Frans (%): Handled 
## Daiki (%): Handled 
## Nathan (%): Handled 


################################################################################




################################################################################


# The function netup() is for initializing the network, 
# which includes the weights, biases and nodes
netup <- function(d) {

  W <- lapply(seq_along(d)[-length(d)], function(i) {
        matrix(runif(d[i] * d[i+1], 0, 0.2), d[i], d[i+1])
  })
  
  h <- lapply(seq_along(d), function(j) {
        rep(0, times = d[j])
  })
  
  b <- lapply(seq_along(d)[-length(d)], function(z) {
        runif(d[z], 0, 0.2)
  })

  nn <- list(h = h, W = W, b = b)
  return(nn)
}


# The function forward() is for computing the every node value except on the 
# first layer
forward <- function(nn, inp){
  nnh <- nn$h
  nnW <- nn$W
  nnb <- nn$b
  
  # put the input values in the node of the first layer
  for (i in 1:length(nnh[[1]])){
    nnh[[1]][i] <- inp[i]
  }
  
  # compute the remaining node values using the ReLU transform
  for (j in 1:length(nnW)){
    for (k in 1:nrow(nnW[[j]])){
      for (m in 1:ncol(nnW[[j]])){
        if ((nnW[[j]][k, m] * nnh[[j]][k] + nnb[[j]][k]) > 0){
          nnh[[j+1]][m] <- nnW[[j]][k, m] * nnh[[j]][k] + nnb[[j]][k]
        }
        else{
          nnh[[j+1]][m] <- 0
        }
      }
    }
  }
  
  nn <- list(h = nnh, W = nnW, b = nnb)
  return(nn)
}


# The function backward() is for computing the derivatives of the loss
backward <- function(nn, k){
  nnh <- nn$h
  nnW <- nn$W
  nnb <- nn$b
  
  # compute the derivative of the loss for k_i w.r.t. h^L_j
  p_k <- exp(nnh[[length(nnh)]])[k] / sum(nnh[[length(nnh)]])  #### wrong
  
  
  # compute the derivatives of the loss w.r.t. all the other h^l_j
  # back-propagation
  
  
  nn <- list(h = nnh, W = nnW, b = nnb, dh = dh, dW = dW, db = db)
  return(nn)
}



# The function train() is for training the network
train <- function(nn, inp, k, eta=.01, mb=10, nstep=10000){
  
  
  
}
