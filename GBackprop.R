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


# h_final represents the output values in the last layer
# class argument represents the class that the softmax function is applied to
softmax <- function(class, h_final) {
      # h_final is a list of raw values from the output node
      return(exp(class)/sum(exp(h_final)))
}

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
train <- function(nn, inp, k, eta=.01, mb=10, nstep=10000){
      
      
      
}
softmax <- function(class, h_final) {
  return(exp(class)/sum(exp(h_final)))
}

softmax1 <- function(h_final) {
  return(exp(h_final)/sum(exp(h_final)))
}

a <- c(1,2,3,4)
k <- 2

b <- softmax1(a)
b[k] = b[k] - 1


d_loss <- rep(0,4)
# Iterate through the values in the final node
for (i in 1:4){
  if (i == k){
    d_loss[i] <- softmax(a[i], a) - 1
  }
  else{
    d_loss[i] <- softmax(a[i], a)
  }
}

b
d_loss


list1 <- list(matrix(21:28, nrow = 4, ncol = 2),
              matrix(1:8, nrow = 4, ncol = 2),
              matrix(1:5, nrow = 5, ncol = 1),
              matrix(1:10, nrow = 10, ncol = 1))

list2 <- list(matrix(21:28, nrow = 4, ncol = 2),
              matrix(1:8, nrow = 4, ncol = 2),
              matrix(1:5, nrow = 5, ncol = 1),
              matrix(1:10, nrow = 10, ncol = 1))
all <- list(list1, list2)

################################################################################
list1 <- list(matrix(21:32, nrow = 4, ncol = 3),
              matrix(1:8, nrow = 4, ncol = 2),
              matrix(1:5, nrow = 5, ncol = 1),
              matrix(1:10, nrow = 10, ncol = 1))

list2 <- list(matrix(21:32, nrow = 4, ncol = 3),
              matrix(1:8, nrow = 4, ncol = 2),
              matrix(1:5, nrow = 5, ncol = 1),
              matrix(1:10, nrow = 10, ncol = 1))

list3 <- list(matrix(21:32, nrow = 4, ncol = 3),
              matrix(1:8, nrow = 4, ncol = 2),
              matrix(1:5, nrow = 5, ncol = 1),
              matrix(1:10, nrow = 10, ncol = 1))


all_nn <- list(list1, list2, list3)
copies <- replicate(10, nn2, simplify = FALSE)

Rprof()
result_sum <- Reduce(function(x, y) Map('+', x, y), list_of_copies)
Rprof(NULL)
summaryRprof()


Rprof()
# put all the gradients w.r.t weight and bias into one list
# dw_all <- lapply(all_nn, function(x) x[[1]])
db_all <- lapply(list_of_copies, function(x) x$db)
sum_db <- Reduce(function(x, y) Map('+', x, y), db_all)
avg_db <- lapply(sum_db, function(x) x/10)
step_db <- lapply(sum_db, function(x) x/10 * 0.01)
b <- nn2$b

new_b <- Map('-', b, step_db)



new_db <- db - 0.01 * avg_db




# find the average gradients
# dw_avg <- Reduce('+', dw_all)
db_avg <- Reduce('+', db_all)

Rprof(NULL)
summaryRprof()



Reduce('+', .list)




