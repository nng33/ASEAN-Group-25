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
        matrix(runif(d[i] * d[i+1], 0, 0.2), d[i+1], d[i])
  })
  
  h <- lapply(seq_along(d), function(j) {
        rep(0, times = d[j])
  })
  
  b <- lapply(seq_along(d)[-1], function(z) {
        runif(d[z], 0, 0.2)
  })
  
  # b <- lapply(seq_along(d)[-length(d)], function(z) {
  #       runif(d[z], 0, 0.2)
  # })

  nn <- list(h = h, W = W, b = b)
  return(nn)
}

# h_j is the list of node values to apply the activation function to
ReLU <- function(h_j) {
      # Applied to each element in the list 
      return(max(0, h_j))
}


# The function forward() is for computing the every node value except on the 
# first layer
forward <- function(nn, inp){
  nn$h[[1]] <- inp 
  
  for(i in 2:length(nn$h)) {
        nn$h[[i]] <- nn$W[[i-1]] %*% nn$h[[i-1]] + nn$b[[i-1]]
  }
  
  # compute the remaining node values using the ReLU transform
  # for (j in 1:length(nnW)){
  #   for (k in 1:nrow(nnW[[j]])){
  #     for (m in 1:ncol(nnW[[j]])){
  #       if ((nnW[[j]][k, m] * nnh[[j]][k] + nnb[[j]][k]) > 0){
  #         nnh[[j+1]][m] <- nnW[[j]][k, m] * nnh[[j]][k] + nnb[[j]][k]
  #       }
  #       else{
  #         nnh[[j+1]][m] <- 0
  #       }
  #     }
  #   }
  # }
 
  return(nn)
}


# h represents the output value in the last layer of the respective class k
softmax <- function(h_final) {
  # h_final is a list of raw values from the output node
  return(exp(h_final)/sum(exp(h_final)))
}


# The function backward() is for computing the derivatives of the loss
backward <- function(nn, k){
  nnh <- nn$h
  nnW <- nn$W
  nnb <- nn$b
  
  # compute the derivative of the loss for k_i w.r.t. h^L_j
  loss <- c(rep(0,length(nnh)))
  for (i in 1:length(nnh)){
    if (i == k){
      loss[i] <- softmax(nnh)[[i]] - 1
    }
    else{
      loss[i] <- softmax(nnh)[[i]]
    }
  }
  
  # compute the derivatives of the loss w.r.t. all the other h^l_j
  # back-propagation
  
  
  nn <- list(h = nnh, W = nnW, b = nnb, dh = dh, dW = dW, db = db)
  return(nn)
}



# The function train() is for training the network
train <- function(nn, inp, k, eta=.01, mb=10, nstep=10000){
  
  
  
}
