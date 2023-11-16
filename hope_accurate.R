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
  
  nn <- list(h = h, W = W, b = b)
  return(nn)
}


# The function ReLU() is for the ReLU transform
# This function takes on h_j as an input, which is the list of node values to
# apply the non-linear transformation
# This function returns the list to which the ReLU transform is applied

ReLU <- function(x) {
      # Applied the ReLU transform to each element in the list 
      return(pmax(x, 0))
}


# The function forward() is for computing the every node value except on the 
# first layer
# forward() takes on 2 inputs:
# (1) nn: a network list as returned by netup()
# (2) inp: a vector of input values for the first layer
# forward() returns the updated version of network list

forward <- function(nn, inp){
  # put the values for the first layer in each node
  nn$h[[1]] <- as.matrix(inp)

  # compute the remaining node values
  for(i in 2:length(nn$h)) {
        nn$h[[i]] <- ReLU(nn$W[[i-1]] %*% nn$h[[i-1]] + nn$b[[i-1]])
  }
 
  return(nn)
}


# The function softmax() is for computing the derivative of the loss
# softmax() takes on one input:
# (1) h_final: the output values in the last layer
# This function returns the value

softmax <- function(h_final) {
  return(exp(h_final)/sum(exp(h_final)))
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
  # loss wrt weights
  dW <- nn$W
  
  # loss wrt node values at each layer
  dh <- nn$h # could be matrices!!!!
  
  # loss wrt biases
  db <- nn$b
  
  # derivative of the loss for k_i w.r.t. hL
  dh[[length(dh)]] <- apply(as.matrix(nn$h[[length(nn$h)]]), 2, softmax)
  
  for (i in 1:ncol(dh[[length(dh)]])){
    dh[[length(dh)]][k[i],i] <- dh[[length(dh)]][k[i],i] - 1
  }
  
  # Iterate through the number of operations linking the layers together
  # E.g., if we have a 4-8-7-3, then we have 3 links or number of layers - 1
  # Backpropagate through the layers to obtain the derivatives
  
  for(i in (length(nn$h)-1):1){
        # mask
        relu_der <- nn$h[[i+1]] > 0
        
        # Update dh for the current layer
        dl1 <- dh[[i+1]] * relu_der
        dh[[i]] <- t(nn$W[[i]]) %*% dl1
  
        # Update gradients for weights and biases 
        dW[[i]] <- dl1 %*% t(nn$h[[i]])
        db[[i]] <- dl1
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
  # inp is n x # of variables
  
  # make matrices of nodes for training 
  # rows corresponds to which node, each column correspond to one 
  # data from the mini batch
  train_h <- lapply(nn$h, function(j) {
    matrix(rep(0, length(j)*mb), length(j), mb)
  })
  
  # update nodes in network from vectors to matrices for training
  nn$h <- train_h
  
  for (step in 1:nstep){
    # make the mini batch for this step
    random_rows <- sample(nrow(inp), size = mb) # get mb random rows
    mini_batch <- t(inp[random_rows,]) # i want inputs to be in the columns
    k_mb <- k[random_rows] # get corresponding output
    
    # step 1: fill in network
    nn <- forward(nn, mini_batch)
    
    # step 2: get gradients (not yet averaged)
    nn <- backward(nn, k_mb)
    
    # step 3: average the gradients
    dW_avg <- lapply(nn$dW, function(x, mb){x/mb}, mb = mb)
    db_avg <- lapply(nn$db, function(x, mb){rowSums(x)/mb}, mb = mb)
    
    # step 4: update parameters
    for (i in 1:length(nn$W)){
      nn$W[[i]] <- nn$W[[i]] - eta*dW_avg[[i]]
      nn$b[[i]] <- nn$b[[i]] - eta*db_avg[[i]]
    }
    
    # get prediction and misclassification rate every 500 runs
    if (step %% 50 == 0){
      pred <- get_prediction(nn, t(mini_batch))
      rate <- get_mis_rate(pred, k_mb)
      print(paste("iteration:", step)) 
      print(paste("misclassification rate:", rate))
    }
  }
  
  return(nn)
}

get_prediction <- function(nn, input){
  # input is a matrix of data in rows, variables in columns
  
  # fill in net for each data
  new_net <- apply(input, 1, forward, nn = nn)
  
  # get the last layer of network for each data
  h_all <- lapply(new_net, function(x) x$h[[length(x$h)]])
  
  # transform node values to probabilities
  pred_all_prob <- lapply(h_all, softmax)
  
  # get predicted class which is the class with the highest probability
  pred_class <- sapply(pred_all_prob, which.max)
  
  # return a vector of predicted classes
  return(pred_class)
}

get_mis_rate <- function(predicted, observed){
  # misclassification rate
  rate <- sum(predicted != observed)/length(observed)
  return(rate)
}


###############################################################################
# application

# data is iris data
data <- iris

# assume output is in the very last column
# convert categorical output into integers
data[, ncol(data)] <- as.numeric(data[, ncol(data)])

# divide the data into training and test data:

# get test data
test_data <- as.matrix(data[seq(5, nrow(data), by = 5),])

# input for test data
test_data_inp <- test_data[,-ncol(test_data)]

# corresponding output of test data
test_data_out <- test_data[,ncol(test_data)]

# get training data
training_data <- as.matrix(data[-seq(5, nrow(data), by = 5),])

# k is the corresponding output of training data
k <- training_data[,ncol(training_data)]

# inp is the input of the training data
inp <- training_data[,-ncol(training_data)]

# d is a vector of the number of nodes in each layer
d <- c(4,8,7,3)

# train the network:
set.seed(2)

# step 1: set the network
nn <- netup(d)

# step 2: train the network
nn <- train(nn, inp, k)

# Test the model:

# make predictions with test data
pred_class <- get_prediction(nn, test_data_inp)

# get the misclassification rate
mis_rate <- get_mis_rate(pred_class, test_data_out)

mis_rate


