###########
##ISLR Ch 5 
###########

#5.3.1 The Validation Set approach 
library(ISLR2)
set.seed(1)
train = sample(392,196)
lm.fit = lm(mpg ~ horsepower , data = Auto , subset = train)


attach(Auto)
#Calculating MSE
MSE1 = mean((mpg - predict(lm.fit , Auto))[-train]^2)

#the estimated test MSE for the linear regression ft is 23.27 

#fitting quadratic 
lm.fit2 = lm(mpg ~ poly(horsepower , 2) , data = AUto , subset = train)

#Calulating MSE
MSE2 = mean((mpg - predict(lm.fit2 , Auto))[-train]^2) #18.72


#fitting cubic 
lm.fit3 = lm(mpg ~ poly(horsepower , 2) , data = Auto , subset = train)

#Calculating MSE 
MSE3 = mean((mpg - predict(lm.fit2 , Auto))[-train]^2) #18.79


##Repeating the same process one more time 

set.seed(2)

train = sample(392,196)

lm.fit = lm(mpg ~ horsepower , data = Auto , subset = train)
MSE1 = mean((mpg - predict(lm.fit , Auto))[-train]^2) # 25.72651

lm.fit2 = lm(mpg ~ poly(horsepower , 2) , data = Auto , subset = train)
MSE2 = mean((mpg - predict(lm.fit2 , Auto))[-train]^2) #20.43036

lm.fit3 = lm(mpg ~ poly(horsepower , 2) , data = Auto , subset = train)
MSE3 = mean((mpg - predict(lm.fit2 , Auto))[-train]^2) #20.43036

#Quadratic performs better than linear. Cubic does not make much difference


#5.3.2 Leave one out cross validation 

#load the boot library to use cv.glm() command of LOOCV
library(boot)
glm.fit = glm(mpg ~ horsepower , data = Auto) #works exactly like lm()
cv.err = cv.glm(Auto , glm.fit) #performance LOOCV

cv.err$delta #LOOCV error obtained 
#1st out put : row cross validation error 
#2nd out put : adjusted cross validation error 

#finding out the least MSE by LOOCV for polynomial regression!
cv.error = rep(0, 10)
for( i in 1:10)
{
  glm.fit = glm(mpg ~ poly(horsepower , i) , data = Auto)
  cv.error[i] = cv.glm(Auto , glm.fit)$delta[1]
}

cv.error
#we see sharp drop in linear and quadratic , after quadratic not much difference 


#5.3.3 : Cross Validation 
cv.error.10 = rep(0, 10)
for( i in 1:10)
{
  glm.fit = glm(mpg ~ poly(horsepower , i) , data = Auto)
  cv.error.10[i] = cv.glm(Auto , glm.fit , K = 10)$delta[1]
}
cv.error.10
#computation time is lesser than LOOCV , observation same as LOOCV


#5.3.4 : Bootstrap 
##Estimating the Accuracy of a Statistic of Interest

#Without using inbuilt function 
alpha.fn = function(data , index)
{
  X = data$X[index]
  Y = data$Y[index]
  (var(Y) - cov(X , Y))/(var(X) + var(Y) - 2*cov( X , Y))
}

alpha.fn(Portfolio , 1:100)

set.seed(7)
alpha.fn(Portfolio , sample(100 , 100, replace = T))

#Now bootstrap with inbuilt function 
boot(Portfolio , alpha.fn  , R = 1000)


###Estimating the Accuracy of a Linear Regression Model 
boot.fn = function(data , index)
  coef(lm(mpg ~ horsepower , data = data , subset = index))

boot.fn(Auto , 1:392)
#
set.seed(1)
boot.fn(Auto , sample(392 , 392 , replace = T))
boot.fn(Auto , sample(392 , 392 , replace = T))
boot(Auto, boot.fn, 1000)
#fitting simple linear reg
summary(lm(mpg ~  horsepower, data = Auto))$coef

#quadratic regression
boot.fn <- function(data, index)
  coef(lm(mpg  ~  horsepower + I(horsepower^2),data = data, subset = index))

set.seed(1)
boot(Auto, boot.fn, 1000)

summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef
#close estimates!!



