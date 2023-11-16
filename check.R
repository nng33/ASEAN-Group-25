
ReLU <- function(h_j) {
  # Applied the ReLU transform to each element in the list 
  return(pmax(0, h_j))
}

forward <- function(nn, inp){
  # put the values for the first layer in each node
  nn$h[[1]] <- as.numeric(inp)
  
  # compute the remaining node values
  for(i in 2:length(nn$h)) {
    nn$h[[i]] <- ReLU(nn$W[[i-1]] %*% nn$h[[i-1]] + nn$b[[i-1]])
  }
  
  return(nn)
}

backward <- function(nn, k){
  # loss wrt weights
  dW <- nn$W
  
  # loss wrt node values at each layer
  dh <- nn$h
  
  # loss wrt biases
  db <- nn$b
  
  # Compute the derivative of the loss for k_i w.r.t. h^L_j
  dh[[length(dh)]] <- softmax(nn$h[[length(nn$h)]])
  dh[[length(dh)]][k] <- dh[[length(dh)]][k] - 1
  
  # Iterate through the number of operations linking the layers together
  # E.g., if we have a 4-8-7-3, then we have 3 links or number of layers - 1
  # Backpropagate through the layers to obtain the derivatives
  
  for(i in (length(nn$h)-1):1){
    # mask
    relu_der <- as.numeric(nn$h[[i+1]] > 0)
    
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

softmax <- function(h_final) {
  return(exp(h_final)/sum(exp(h_final)))
}



train <- function(nn, inp, k, eta=.01, mb=10, nstep=10000){
  # i have weight
  # fill in networks with weight 
  # >> have multiple networks with same weight and biases but different node values
  # number of networks == number of elements in the minibatch = mb
  # boo hoo these network sucks
  # do gradient descent for each network and for each output class k to adjust parameter!
  # get gradients of everything for each network
  # get average gradients
  # STEP
  # new weight and biases for class k
  # NEW WEIGHT AND BIASES FOR ALL OUTPUT CLASS :))
  # fill in ALL network with new weight and biases
  
  for (i in 1:nstep){
    # make the mini batch for this step
    random_rows <- sample(nrow(inp), size = mb)
    mini_batch <- inp[random_rows,]
    k_mb <- k[random_rows]
    
    all_nn <- rep(list(nn), mb)
    
    # for each element in the mini batch
    for (j in 1:mb){
      # fill in nodes according to weights and biases
      all_nn[[j]] <- forward(all_nn[[j]], inp = mini_batch[j,])
      
      # optimize weight and biases with stochastic gradient descent
      all_nn[[j]] <- backward(all_nn[[j]], k_mb[j])
    }
    
    #   # for each output class
    #   for (k in 1:length(unique(k))){
    #     # get gradients of all weights and biases
    #     all_nn[[j]] <- backward(all_nn[[j]], k)
    #   }
    
    
    # put all the gradients w.r.t. weight and bias into one list
    dw_all <- lapply(all_nn, function(x) x$dW)
    db_all <- lapply(all_nn, function(x) x$db)
    
    # find the average gradients
    sum_dw <- Reduce(function(x, y) Map('+', x, y), dw_all)
    sum_db <- Reduce(function(x, y) Map('+', x, y), db_all)
    step_dw <- lapply(sum_dw, function(x) x/mb * eta) # step for W
    step_db <- lapply(sum_db, function(x) x/mb * eta) # step for b
    
    # update weight and bias
    nn$W <- Map('-', nn$W, step_dw)
    nn$b <- Map('-', nn$b, step_db)
    
  }
  
  return(nn)
}

W <- list(matrix(c(-2.5, -1.5, 0.6, 0.4), nrow = 2),
          matrix(c(-0.1, 2.4, -2.2, 1.5, -5.2, 3.7), ncol = 2))

b <- list(matrix(c(1.6, 0.7), ncol = 1),
          matrix(c(0, 0, 1), ncol = 1))

h <- list(matrix(c(0, 0), ncol = 1),
          matrix(rep(0,2), ncol = 2),
          matrix(rep(0,3), ncol = 1))

nn <- list(W = W, b = b, h = h)
nn <- forward(nn1, inp = c(0.5, 0.37))
n <- backward(nn, k = 1)

dW <- nn$W
dh <- nn$h
db <- nn$b

# Compute the derivative of the loss for k_i w.r.t. h^L_j

# fill in the last dh
# k <- 1
# dh[[length(dh)]] = softmax(nn$h[[length(nn$h)]])
# dh[[length(dh)]][k] = dh[[length(dh)]][k] - 1
# 
# 
# for(i in (length(nn$h)-1):1) {
#   # Compute the derivative of the ReLU function
#   relu_der <- as.numeric(nn$h[[i+1]] > 0)
#   # Update dh for the current layer
#   dl1 <- dh[[i+1]] * relu_der
#   dh[[i]] <- t(nn$W[[i]]) %*% dl1
# 
# 
#   # Update gradients for weights and biases
#   dW[[i]] <- dl1 %*% t(nn$h[[i]])
#   db[[i]] <- dl1
#   # # Compute the loss for the next iteration
#   # d_loss <- t(nn$W[[i]]) %*% dh
# }
# 
# # fill in the rest
# for(i in (length(nn$h)-1):1) {
#   # Compute the derivative of the ReLU function
#     relu_der <- as.numeric(nn$h[[i+1]] > 0)
#     # Update dh for the current layer
#     dl1 <- dh[[i+1]] * relu_der
#     dh[[i]] <- t(nn$W[[i]]) %*% dl1
# }
# 
# i <- 2
# relu_der <- as.numeric(nn$h[[i+1]] > 0)
# # Update dh for the current layer
# dl1 <- dh[[i+1]] * relu_der
# dh[[i]] <- t(nn$W[[i]]) %*% dl1
# db[[i]] <- dl1
# dW[[i]] <- dl1 %*% t(nn$h[[i]])
# 
# 
# 
# i <- 1
# relu_der <- as.numeric(nn$h[[i+1]] > 0)
# # Update dh for the current layer
# dl1 <- dh[[i+1]] * relu_der
# dh[[i]] <- t(nn$W[[i]]) %*% dl1
# db[[i]] <- dl1
# dW[[i]] <- dl1 %*% t(nn$h[[i]])

# train <- function(nn, inp, k, eta=.01, mb=10, nstep=50)


# get data
set.seed(10)
eta <- 0.01
data <- iris[sample(nrow(iris),size = 10),c(2,4,5)]
k <- as.numeric(data[,ncol(data)])
inp <- data[,-ncol(data)]
mb <- 3


random_rows <- sample(nrow(inp), size = mb)
mini_batch <- inp[random_rows,]
k_mb <- k[random_rows]

all_nn <- rep(list(nn), mb)

j <- 1
all_nn[[j]] <- forward(all_nn[[j]], inp = mini_batch[j,])
all_nn[[j]] <- backward(all_nn[[j]], k_mb[j])


for (i in 1:nstep){
  # make the mini batch for this step
  random_rows <- sample(nrow(inp), size = mb)
  mini_batch <- inp[random_rows,]
  k_mb <- k[random_rows]
  
  all_nn <- rep(list(nn), mb)
  
  
  # for each element in the mini batch
  for (j in 1:mb){
    # fill in nodes according to weights and biases
    all_nn[[j]] <- forward(all_nn[[j]], inp = mini_batch[j,])
    
    # optimize weight and biases with stochastic gradient descent
    all_nn[[j]] <- backward(all_nn[[j]], k_mb[j])
  }
  
  #   # for each output class
  #   for (k in 1:length(unique(k))){
  #     # get gradients of all weights and biases
  #     all_nn[[j]] <- backward(all_nn[[j]], k)
  #   }
  
  
  # put all the gradients w.r.t. weight and bias into one list
  dw_all <- lapply(all_nn, function(x) x$dW)
  db_all <- lapply(all_nn, function(x) x$db)
  
  # find the average gradients
  sum_dw <- Reduce(function(x, y) Map('+', x, y), dw_all)
  sum_db <- Reduce(function(x, y) Map('+', x, y), db_all)
  step_dw <- lapply(sum_dw, function(x) x/mb * eta) # step for W
  step_db <- lapply(sum_db, function(x) x/mb * eta) # step for b
  
  # update weight and bias
  nn$W <- Map('-', nn$W, step_dw)
  nn$b <- Map('-', nn$b, step_db)
}
  

a <- c(1,2,3,3,4)
b <- c(1,4,2,3,4)

sum(a != b)



train <- function(nn, inp, k, eta=.01, mb=10, nstep=10000){
  # inp is n x # of variables
  # make nodes for training 
  train_h <- lapply(nn$h, function(j) {
    matrix(rep(0, length(j)*mb), length(j), mb)
  })
  
  nn$h <- train_h
  
  
  # for (i in 1:nstep){
    # make the mini batch for this step
    random_rows <- sample(nrow(inp), size = mb) # get mb rows
    mini_batch <- t(inp[random_rows,]) # i want inputs to be in the columns
    k_mb <- k[random_rows]
    
  # }
}


#################################################################################

# data is iris data
data <- iris

# divide the data into training and test data
# change categorical
data[, ncol(data)] <- as.numeric(data[, ncol(data)])

# get test data
test_data <- as.matrix(data[seq(5, nrow(data), by = 5),])
test_data_inp <- test_data[,-ncol(test_data)]
test_data_out <- test_data[,ncol(test_data)]

# get training data
training_data <- as.matrix(data[-seq(5, nrow(data), by = 5),])

k <- training_data[,ncol(training_data)]
inp <- training_data[,-ncol(training_data)]


d <- c(4,8,7,3)

###########################################
# check if forward and backward works
nn <- netup(d)
for_test <- forward(nn, mini_batch[1,])
back_test <- backward(for_test, k_mb[1])

# check for matrices
# update h first
train_h <- lapply(nn$h, function(j) {
  matrix(rep(0, length(j)*mb), length(j), mb)
})
nn$h <- train_h

for_test_mat <- forward(nn, t(mini_batch))
# output is non-averaged gradient
back_test_mat <- backward(for_test_mat, k_mb)

# get average gradient
dW_test <- lapply(back_test_mat$dW, function(x,mb){x/mb}, mb = mb)
db_test <- lapply(back_test_mat$db, function(x, mb){rowSums(x)/mb}, mb = mb)

# update parameter
eta <- 0.01
for (i in 1:length(back_test_mat$W)){
  back_test_mat$W[[i]] <- back_test_mat$W[[i]] - eta*dW_test[[i]]
  back_test_mat$b[[i]] <- back_test_mat$b[[i]] - eta*db_test[[i]]
}

################################################################

# Test train()
nn <- netup(d)
nn1 <- train(nn, inp, k, nstep = 10000)

# get prediction
pred_class <- get_prediction(nn1, test_data_inp)
rate <- get_mis_rate(pred_class, test_data_out)
rate
pred_class
test_data_out

#################################################################

# change for loop
a <- matrix(seq(1:12), 4,3)
k <- c(1,3,2)

# do this without for loop
  for (i in 1:ncol(a)){
    a[k[i],i] <- a[k[i],i] - 1
  }

decrement_index <- function(a, k, increment = 1) {
      # convert your vector in a matrix so each value in the vector becomes a 
      # column element in the next matrix
      mat_k <- matrix(rep(k), nrow(a), ncol(a), byrow = TRUE) 
      # convert the original matrix into a matrix of column indices
      mat_ind <- matrix(rep(1:nrow(a), ncol(a)), nrow = nrow(a))
      # match the vector turned matrix to the converted original matrix 
      # convert it into a matrix of 0s and 1s and minus with original matrix
      return(a - as.matrix(mat_k == mat_ind) * increment)
}


print(a)
