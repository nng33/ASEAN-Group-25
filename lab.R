netup <- function(d, mb) {
  
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

mb <- 3
train_h <- lapply(nn$h, function(j) {
  matrix(rep(0, length(j)*mb), length(j), mb)
})

