############
## ISLR Ch3
############

##3.6.1 Libraries
library(MASS)
library(ISLR2)

##3.6.2 Simple Linear Regression 
head(Boston)
?Boston
#predict mdev using rm , age , lstat 
# mdev : median value of owner-occupied homes in $1000s. 
# rm : average number of rooms per dwelling. 
# age : proportion of owner-occupied units built prior to 1940
# lstat :  lower status of the population (percent) 
#       : percent of households with low socioeconomic status

#Fitting linear regression 
lm.fit = lm(medv ~ lstat , data = Boston)


attach(Boston)
#After attach command no need to specify the data 
lm.fit = lm(medv ~ lstat)

#shows functional form and estimates of intercept
lm.fit

#testing table , R^2 value 
summary(lm.fit)

names(lm.fit)

#lm.fit$coefficients  and  coef() give same out put of estimates of coefficients
lm.fit$coefficients 
coef(lm.fit)
confint(lm.fit)

##To get confidence interval 
predict(lm.fit , data.frame(lstat = c(5 , 10 , 15)) , interval = "confidence")

#To get prediction interval 
predict(lm.fit , data.frame(lstat = c(5 , 10 , 15)) , interval = "prediction")

#
plot(lstat , medv)
abline(lm.fit , col = "blue" , lwd = 3)

#pch decides the shape of the points in scatterplot 
plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)


#Diagnostics plots 
par(mfrow = c(2,2))
plot(lm.fit) 

#residuals vs fitted
plot(predict(lm.fit) , residuals(lm.fit) , main = "residuals vs fitted")

#studentised residuals vs fitted
plot(predict(lm.fit) , rstudent(lm.fit) , main = "studentised residuals vs fitted")

#Leverage statistics for each point
hatvalues(lm.fit)  

#Plot of Leverage statistics vs Index 
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

#Pattern present in Residual Vs Fitted : Indicates problem in 
#assumption of functional model 

##3.6.3 : Multiple linear regression  
lm.fit  =  lm(medv ~  lstat + age , data = Boston)
summary(lm.fit)

#regressing on all four variables 
lm.fit = lm(medv ~ ., data = Boston)
lm.fit
summary(lm.fit)

#To access all the coponents of summary 
?summary.lm

summary(lm.fit)$r.sq #to get R^2 
summary(lm.fit)$sigma  # RSE 


#vif() function is a part of car package
library(car)
vif(lm.fit)

#Excluding a variable and then fitting 
# Here p value of age factor is very high , so we can exclude it 

lm.fit1 = lm(medv ~ .-age , data = Boston)
summary(lm.fit1)

# same can be achieved with update function 
lm.fit1 = update(lm.fit , ~.-age)

##3.6.4 Interaction terms 
# lstat * age  , is short way for lstat + age + lstat : age
# where lstat : age considers the ineraction term between lstat and age 

summary( lm(medv ~ lstat * age) , data = Boston) 


##3.6.5 : Non linear transformations of the predictors 
# I() is used to instruct R to not to perform the calculation and instead
# Just treat it as formula 

lm.fit2 = lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)

# The near 0 p value of lstat^2 indicates that it is an improved model 
# We use the anova() function to further quantify the extent to
#which the quadratic ft is superior to the linear fit 
lm.fit = lm(medv ~ lstat)
anova(lm.fit , lm.fit2)

# H0 : two models fit the data equally well 
# H1 : the full model is better i.e. the second model is better

par(mfrow = c(2,2))
plot(lm.fit2) #little pattern in  residual plots is seen 


# 
lm.fit5 = lm(medv  ~ poly(lstat , 5))
summary(lm.fit5)

lm.fit6 = lm(medv  ~ poly(lstat , 6))
summary(lm.fit6)   #The term with 6th power is not significant regressor 




#
lm.lfit = lm(medv~ log(rm) , data = Boston)
summary(lm.lfit)

##3.6.6 Qualitative Variables 

head(Carseats) 
View(Carseats) 
#Shelveloc : Qualitative variable that has  three levels 
# R automatically generates dummy variables 

lm.fit = lm(Sales ~. + Income:Advertising + Price:Age , data = Carseats)
summary(lm.fit) 

attach(Carseats)
contrasts(ShelveLoc)
#It returns the coding for dummy variables


##3.6.7 
LoadLibraries = function()
{
  library(MASS)
  library(ISLR2)
  print("The libraries have been loaded")
}
LoadLibraries()


############################################################################
