#Data generation process 
x1 = rnorm(100)
x2 = rnorm(100)
x3 = rnorm(100)
eps = rnorm(mean = 0 , sd = 0.5 , 100)
y = -1.5 + 2.5*x1 + 2.5*x2 + 2.5*x3  + eps

####


#initialization
beta0 = 0 
beta1 = 0
beta2 = 0 
beta3 = 0

lr = 0.01
num_iter = 100
####

for(i in 1 : num_iter)
{
  y_pred = beta0 + beta1*x1 + beta2*x2 + beta3*x3 
  error = y_pred - y 
  
  beta0 = beta0 - lr*sum(error)
  beta1 = beta1 - lr*sum(error*x1)
  beta2 = beta2 - lr*sum(error*x2)
  beta3 = beta3 - lr*sum(error*x3)
   
}
print(c( round(beta0 ,2) , round(beta1,2) , round(beta2,2) , round(beta3,2)))

############ 
##Slightly improved 


MLR = function( p , lr , num_iter)
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
      beta[j] <- beta[j] - lr * sum(error * X[, j])
    }
  }
  
  # Print the estimated coefficients
  cat("Estimated coefficients:\n")
  for (j in 1:(p + 1)) {
    cat("beta", j - 1, ":", round(beta[j], 2), "\n")
  }
  
}


MLR(p = 3 , lr = 0.01 , num_iter = 100)

