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
# netup() takes on 1 input:
# d: a vector giving the number of nodes in each layer of a network
# netup() returns a list with 3 elements:
# (1) h: a list of nodes for each layer
# (2) W: a list of weight matrices
# (3) b: a list of offset vectors

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


# The function ReLU() is for the ReLU transform
# This function takes on h_j as an input, which is the list of node values to
# apply the non-linear transformation
# This function returns the list to which the ReLU transform is applied

ReLU <- function(h_j) {
      # Applied the ReLU transform to each element in the list 
      return(pmax(0, h_j))
}


# The function forward() is for computing the every node value except on the 
# first layer
# forward() takes on 2 inputs:
# (1) nn: a network list as returned by netup()
# (2) inp: a vector of input values for the first layer
# forward() returns the updated version of network list

forward <- function(nn, inp){
  # put the values for the first layer in each node
  nn$h[[1]] <- inp 
  
  # compute the remaining node values
  for(i in 2:length(nn$h)) {
        nn$h[[i]] <- ReLU(nn$W[[i-1]] %*% nn$h[[i-1]] + nn$b[[i-1]])
  }
 
  return(nn)
}


# The function softmax() is for computing the derivative of the loss
# softmax() takes on two inputs:
# (1) class: the class that the softmax function is applied to
# (2) h_final: the output values in the last layer
# This function returns the value

softmax <- function(class, h_final) {
  # h_final is a list of raw values from the output node
  return(exp(class)/sum(exp(h_final)))
}


# The function backward() is for computing the derivatives of the loss
# backward() takes on 2 inputs:
# (1) nn: a network list as returned by forward()
# (2) k: output class
# backward() returns a list with 6 elements:
# (1) h: a list of nodes for each layer
# (2) W: a list of weight matrices
# (3) b: a list of offset vectors
# (4) dh: a list of the derivative w.r.t. the nodes
# (5) dW: a list of the derivative w.r.t. the weight matrices
# (6) db: a list of the derivative w.r.t. the offset vectors

# The function backward() is for computing the derivatives of the loss
backward <- function(nn, k){
      # loss wrt weights at each layer initialized at 0s
      dW <- nn$W
      dW <- lapply(nn$W, function(x) matrix(0, nrow = nrow(x), ncol = ncol(x)))
      # loss wrt node values at each layer
      dh <- nn$h
      # loss wrt biases at each layer initialized at 0s
      db <- nn$b
      db <- lapply(nn$b, function(x) rep(0, length(x)))
      
      # Compute the derivative of the loss for k_i w.r.t. h^L_j
      # Obtain the length of the last element in h of list nn
      nn_len <- length(nn$h[[length(nn$h)]])
      d_loss <- c(rep(0,nn_len))
      # Iterate through the values in the final node
      for (i in 1:nn_len){
            if (i == k){
                  d_loss[i] <- softmax(nn$h[[nn_len]][i], nn$h[[length(nn$h)]]) - 1
            }
            else{
                  d_loss[i] <- softmax(nn$h[[nn_len]][i], nn$h[[length(nn$h)]])
            }
      }
      
      # d_loss <- c(rep(0,length(nn$h)))
      # for (i in 1:length(nn$h)){
      #   if (i == k){
      #         d_loss[[i]] <- softmax(nn$h)[[i]] - 1
      #   }
      #   else{
      #         d_loss[i] <- softmax(nn$h)[[i]]
      #   }
      # }
      # 
      # dh[[length(dh)]] = softmax(nn$h)
      # dh[[length(dh)]][k] = dh[[length(dh)]][k] - 1
      
      
      # Iterate through the number of operations linking the layers together
      # E.g., if we have a 4-8-7-3, then we have 3 links or number of layers - 1
      # Backpropagate through the layers to obtain the derivatives
      layers <- length(nn$h)
      for(i in rev(seq_along(nn$h)[-length(nn$h)])) {
            # Compute the derivative of the ReLU function
            relu_der <- as.numeric(nn$h[[i]] > 0)
            # Update dh for the current layer
            dh <- d_loss * relu_der
            # Update gradients for weights and biases 
            dW[[i]] <- outer(dh, nn$h[[i+1]])
            db[[i]] <- dh
            # Compute the loss for the next iteration 
            d_loss <- t(nn$W[[i]]) %*% dh 
      }
      
      nn <- list(h = nn$h, W = nn$W, b = nn$b, dh = dh, dW = dW, db = db)
      return(nn)
}


# The function train() is for training the network
# train() takes on 6 inputs:
# (1) nn: a network list
# (2) inp: the rows of matrix of input data
# (3) k: a vector of corresponding labels
# (4) eta: the step size
# (5) mb: the number of data to randomly sample to compute the gradient
# (6) nstep: the number of optimization steps to take
# train() returns the updated version of network list

train <- function(nn, inp, k, eta=.01, mb=10, nstep=10000){
  nn <- netup(d)
  
  for (i in 1:nstep){
    nn <- forward(nn, inp)
    nn <- backward(nn, k)    #### this k should be changed
    nn$W <- nn$W - (eta * nn$dW)
    nn$b <- nn$b - (eta * nn$db)
  }
  
  nn <- forward(nn, inp)
  
  return(nn)
}


# divide the iris data into training and test data
training_data <- iris[-seq(5, nrow(iris), by = 5),]
test_data <- iris[seq(5, nrow(iris), by = 5),]


