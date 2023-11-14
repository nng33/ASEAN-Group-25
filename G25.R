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
      return(pmax(0, h_j))
}


# The function forward() is for computing the every node value except on the 
# first layer
forward <- function(nn, inp){
  # put the values for the first layer in each node
  nn$h[[1]] <- inp 
  
  # compute the remaining node values
  for(i in 2:length(nn$h)) {
        nn$h[[i]] <- ReLU(nn$W[[i-1]] %*% nn$h[[i-1]] + nn$b[[i-1]])
  }
 
  return(nn)
}


# h represents the output value in the last layer of the respective class k
softmax <- function(h_final) {
  # h_final is a list of raw values from the output node
  return(exp(h_final)/sum(exp(h_final)))
}


# The function backward() is for computing the derivatives of the loss
backward <- function(nn, k){
  dW <- nn$W
  dh <- nn$h
  db <- nn$b
  
  # compute the derivative of the loss for k_i w.r.t. h^L_j
  d_loss <- c(rep(0,length(nn$h)))
  for (i in 1:length(nn$h)){
    if (i == k){
          d_loss[[i]] <- softmax(nn$h)[[i]] - 1
    }
    else{
          d_loss[i] <- softmax(nn$h)[[i]]
    }
  }
  
  dh[[length(dh)]] = softmax(nn$h)
  dh[[length(dh)]][k] = dh[[length(dh)]][k] - 1
  
  
  # compute the derivatives of the loss w.r.t. all the other h^l_j
  # back-propagation
  
  for(i in length(d_loss)) {
        if(d_loss[i] < 0) {
              d_loss[i] <- 0
        }
        
  }
  
  nn <- list(h = nn$h, W = nn$W, b = nn$b, dh = dh, dW = dW, db = db)
  return(nn)
}



# The function train() is for training the network
train <- function(nn, inp, k, eta=.01, mb=10, nstep=10000){
  
  
  
}
