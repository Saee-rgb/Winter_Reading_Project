
Lasso = function( p , lr , num_iter , lambda)
{
  
  # Data generation process
  n <- 100  
  X <- matrix(rnorm(n * p), ncol = p)
  eps <- rnorm(n, mean = 0, sd = 0.4)
  
  y <- -1.5 + sum(rep(2.5, p) * X[, 1:p]) + eps
  
  ## Initialization
  beta <- numeric(p + 1)
  X <- cbind(1, X)  # Add intercept column
  
  ## Gradient Descent Iterations for optimization
  for (i in 1:num_iter) 
  {
    y_pred <- X %*% beta  
    error <- y_pred - y
    
    for(j in 1:(p + 1))
    {
      beta[j] <- beta[j] - lr * sum(error * X[, j] + lambda* sign(beta[j]))
    }
  }
  
  # Print the estimated coefficients
  cat("Estimated coefficients:\n")
  for (j in 1:(p + 1)) {
    cat("beta", j - 1, ":", round(beta[j], 2), "\n")
  }
  
}


Ridge(p = 3 , lr = 0.001 , lambda = 0.01 ,num_iter = 100)




