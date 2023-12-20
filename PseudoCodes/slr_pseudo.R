#Simple linear regression  

#Data generation process 
x = rnorm(100)

eps = rnorm(mean = 0 , sd = 0.5 , 100)

y = -1.5 + 0.5*x + eps

data = data.frame(x , y)
 

Loss.opt = function(x,y)
{
  #initialization 
  beta0 = 0 
  beta1 = 0 
  lr = 0.01
  num_iter = 1000
  
  #optimization
  for( i in 1:num_iter) 
  {
    y_pred = beta0 + beta1*x
    error = y_pred - y
    beta0 = beta0 - lr*sum(error)
    beta1 = beta1 - lr*sum(error*x)
  
  }
  
  #return
  fit = paste(" y_i_hat = " , round(beta0 , 2), " +  " , round(beta1,2) , "x_i")
  return(fit)
}

Loss.opt(data$x , data$y)




