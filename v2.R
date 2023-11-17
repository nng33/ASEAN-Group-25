## Frans  : s2591760
## Daiki  : s2547603
## Nathan : s2524152

## github: https://github.com/nng33/ASEAN-Group-25

## Contribution to this project
## Frans  (33%): Handled train(), prediction and misclassification rate
## Daiki  (33%): Handled backward() and netup()
## Nathan (34%): Handled forward(), code efficiency with apply() family

###############################################################################
# This code provides 4 main functions to construct a trained simple neural 
# network: netup(), forward(), backward(), and train().

# A neural networkfor classification with flexible choice of number of layers 
# and nodes is set up with netup(). It is then trained by stochastic 
# gradient descent in train(). 

# By forward(), each node value is calculated by applying 
# the ReLU activation function to the linear combination of the values from the 
# previous layer's nodes with weights and offset values/biases.

# gradient of the loss function w.r.t to the parameters are calculated 
# by backwards propagation in backward() to obtain the direction that 
# the parameters should adjust by.

# The probability that the output variable is in a certain class is calculated
# by applying the softmax activation function on the last layer's nodes.

# The trained neural network can then be used to predict which class an 
# observation belongs to. Misclassification rate is then calculated to 
# determine the model's accuracy.
###############################################################################

ReLU <- function(x) {
  # ReLU() applies ReLU activation function to array x
  # returns an array of the transformed input
  return(pmax(x, 0))
}

softmax <- function(h){
  # softmax() returns the node values applied to 
  # the softmax() activation function
  # input:
  # h: array of node values
  return(exp(h)/sum(exp(h)))
}

get_zero_matrix <- function(mat){
  # get_zero_matrix() creates a matrix of zero entries with
  # the same dimension of matrix mat
  return(matrix(0, nrow(mat), ncol(mat)))
}

get_zero_vector <- function(v){
  # get_zero_vector() creates a vector of zero entries with
  # the same length as vector v
  return(numeric(length(v)))
}

avg_gradient <- function(x, mb){
  # avg_gradient() divides matrix x by constant mb
  return(x/mb)
}

add_list <- function(list1, list2, i){
  # add_list() adds two matrices in two different list of index [[i]]
  return(list1[[i]] + list2[[i]])
}

update_param <- function(param, grad, eta, i){
  # update_param() returns the updated parameter after stepping
  # inputs:
  # param: a list of parameter layers to be updated
  # grad: list of corresponding gradient
  # eta: constant step size
  # i: index of which parameter layer to be updated
  return(param[[i]] - (eta * grad[[i]]))
}

netup <- function(d) {
  
  # netup() is for initializing the network, 
  # which includes the weights, biases and nodes
  # input:
  # d: a vector giving the number of nodes in each layer of a network
  # returns a list with 3 elements:
  # (1) h: a list of nodes for each layer
  # (2) W: a list of weight matrices. W[[l]] links layer l to l+1
  # (3) b: a list of offset/bias vectors. b[[l]] links layer l to l+1
  
  # initialize weights with Uniform(0, 0.2) random deviates
  W <- lapply(seq_along(d)[-length(d)], function(i) {
    matrix(runif(d[i] * d[i+1], 0, 0.2), d[i+1], d[i])
  })
  
  # initialize all node values to be 0
  h <- lapply(seq_along(d), function(j) {
    rep(0, times = d[j])
  })
  
  # initialize offsets with Uniform(0, 0.2) random deviates
  b <- lapply(seq_along(d)[-1], function(z) {
    runif(d[z], 0, 0.2)
  })
  
  # return network list
  nn <- list(h = h, W = W, b = b)
  return(nn)
}

forward <- function(nn, inp){
  
  # forward() computes the node values of the network
  # inputs:
  # (1) nn: a network list as returned by netup()
  # (2) inp: a vector of input values for the first layer
  # returns the updated version of network list
  
  # put the values for the first layer
  nn$h[[1]] <- inp
  
  # compute and update the remaining node values
  # by applying the ReLU activation function to the linear combination
  # of the previous node values with weights and biases
  for(i in 2:length(nn$h)){
    nn$h[[i]] <- ReLU(nn$W[[i-1]] %*% nn$h[[i-1]] + nn$b[[i-1]])
  }
  
  # return the updated network
  return(nn)
}

backward <- function(nn, k){
  
  # backward() is for computing the derivatives of the loss
  # inputs:
  # (1) nn: a network list as returned by forward()
  # (2) k: output class
  # backward() returns a list with 6 elements:
  # (1) h: a list of nodes for each layer
  # (2) W: a list of weight matrices
  # (3) b: a list of offset vectors
  # (4) dh: a list of the derivative w.r.t. the nodes
  # (5) dW: a list of the derivative w.r.t. the weight matrices
  # (6) db: a list of the derivative w.r.t. the offset vectors
  
  # initialize lists for storing derivatives of the loss function:
  
  # derivative w.r.t. to the nodes has same dimension as h
  dh <- nn$h
  
  # derivative w.r.t. to weights has same dimension as W
  dW <- nn$W
  
  # derivative w.r.t. to biases has same dimension as b
  db <- nn$b
  
  # last layer position
  L <- length(nn$h)
  
  # derivative of the loss for k_i w.r.t. the nodes in the last layer of hL
  dh[[L]] <- softmax(nn$h[[L]])
  
  # if j == k, need to -1
  dh[[L]][k] <- dh[[L]][k] - 1
  
  # Back-propagate through the layers to obtain the other derivatives
  # start from the last layer and work backwards
  # we have length(nn$h)-1 layer of weights and biases to populate
  for(i in (L-1):1){
    
    # give logical mask for d_j^l+1 > 0 
    relu_der <- nn$h[[i+1]] > 0 
    
    # dl1_j = d_j^l+1 = dh_j^l+1 if h_j^l+1 >0, 0 otherwise.
    dl1 <- dh[[i+1]] * relu_der
    
    # update dh for the current layer
    dh[[i]] <- t(nn$W[[i]]) %*% dl1
    
    # update gradients for weights
    dW[[i]] <- dl1 %*% t(nn$h[[i]])
    
    # update gradients for biases
    db[[i]] <- dl1
  }
  
  # return updated list
  nn <- list(h = nn$h, W = nn$W, b = nn$b, dh = dh, dW = dW, db = db)
  return(nn)
}

train <- function(nn, inp, k, eta=.01, mb=10, nstep=10000){
  
  # train() trains the network and optimize the weights and biases
  # by stochastic gradient descent
  # inputs:
  # (1) nn: a network list
  # (2) inp: matrix where each row is input data
  # (3) k: a vector of integer values corresponding to output class
  # (4) eta: the step size
  # (5) mb: the number of data to randomly sample to compute the gradient
  # (6) nstep: the number of optimization steps to take
  # train() returns the updated version of network list
  
  # loop through each step
  for (i in 1:nstep){
    
    # make the mini batch for this step
    random_rows <- sample(nrow(inp), size = mb) # get mb random rows
    mini_batch <- inp[random_rows,] # randomly sample mb data from inp
    k_mb <- k[random_rows] # get corresponding output
    
    # initialize list of 0s for summing gradients
    dW_avg <- lapply(nn$W, get_zero_matrix)
    db_avg <- lapply(nn$b, get_zero_vector)
    
    # run network for each data in mini batch
    for (j in 1:mb){
      # step 1: fill in network
      nn <- forward(nn, mini_batch[j,])
      
      # step 2: get gradients
      nn <- backward(nn, k_mb[j])
      
      # step 3: aggregate gradients
      dW_avg <- lapply(seq_along(dW_avg), add_list, 
                       list1 = dW_avg, list2 = nn$dW)
      
      db_avg <- lapply(seq_along(db_avg), add_list, 
                       list1 = db_avg, list2 = nn$db)
    }
    
    # step 4: divide by mb to average the gradients
    dW_avg <- lapply(dW_avg, avg_gradient, mb = mb)
    db_avg <- lapply(db_avg, avg_gradient, mb = mb)

    # step 5: update the parameters 
    nn$W <- lapply(seq_along(nn$W), update_param, 
                   param = nn$W, grad = dW_avg, eta = eta)
    
    nn$b <- lapply(seq_along(nn$b), update_param, 
                   param = nn$b, grad = db_avg, eta = eta)
  }
  
  # return the updated list
  return(nn)
}

get_prediction <- function(nn, input){
  
  # get_prediction() predicts using the neural network
  # inputs:
  # nn: a network list
  # input: the input data matrix. one row for each data, columns are variables
  # returns the predicted output class
  
  # fill in network for each data
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
  
  # get_mis_rate() returns the misclassification rate
  # inputs:
  # predicted: a vector of predicted output class
  # observed: a vector of true output class

  rate <- sum(predicted != observed)/length(observed)
  return(rate)  
}

###############################################################################

# Training a 4-8-7-3 network on iris data set to classify irises species

# data is iris data
data <- iris

# number of data column
col_data <- ncol(data)

# vector of possible output classes
classes <- as.vector(unique(data[, col_data]))

# assume output is in the very last column
# convert categorical output into integers
data[, col_data] <- as.numeric(data[, col_data])

# divide the data into training and test data:

# test data every 5th row starting from row 5
test_data <- as.matrix(data[seq(5, nrow(data), by = 5),])

# the input for test data
test_data_inp <- test_data[,-ncol(test_data)]

# the corresponding output of test data
test_data_out <- test_data[,ncol(test_data)]

# get training data
training_data <- as.matrix(data[-seq(5, nrow(data), by = 5),])

# the input for the training data
inp <- training_data[,-ncol(training_data)]

# the corresponding output of training data
k <- training_data[,ncol(training_data)]

# a vector of the number of nodes in each layer
d <- c(4,8,7,3)

# build the network:
set.seed(2) # successful example

# step 1: set the network
nn <- netup(d)

# step 2: train the network
start <- Sys.time()
Rprof()
system.time(nn <- train(nn, inp, k))
Rprof(NULL)
summaryRprof()
period <- Sys.time() - start
period

# Test the model:

# make predictions with test data
pred_output <- get_prediction(nn, test_data_inp)

# predicted species of test data
Predicted.Species <- classes[pred_output]

# true observed species of test data
Observed.Species <- classes[test_data_out]

# put test data, predicted species, and the true observed species together
test_data_pred <- cbind(test_data_inp, Predicted.Species, 
                        Observed.Species)

# get the misclassification rate
mis_rate <- get_mis_rate(pred_output, test_data_out)
