#############
###Exercises 
#############

#Q.8 
View(Auto)
#Q.8 a 
attach(Auto)
lm.fit = lm(mpg ~ horsepower) 
summary(lm.fit) 

#i. p value for F test is virtually zero , hence relationship is present 
#ii.RSE is 4.906 : around 21%  is the percentage error
#R^2 is 0.6049  : which implies that around 60% of variance in 
#mpg is explained by the predictor horsepower 

cor(mpg , horsepower)
#iii. correlation is -0.778 , negative relationship 

#iv
##To get confidence interval 
predict(lm.fit , data.frame(horsepower = c(98)) , interval = "confidence")

#To get prediction interval 
predict(lm.fit , data.frame(horsepower = c(98)) , interval = "prediction")


#Q8.(b)Plotting the regression line
plot(x = horsepower , y = mpg , pch =16)
abline(lm.fit , col = "blue" , lwd = 2)

#Q.8(c)Residual plots
par(mfrow = c(2,2))
plot(lm.fit)
#As residual vs fitted plot has pattern , it implies that,there is some 
#problem in linearity assumption 


#Q9

#Q9a. 
pairs(Auto)

#Q9b 
cor.mat = cor(Auto[ , 1 : 8])
cor.mat

#Q9.c 
Auto = Auto[ , 1 : 8] # We are excluding the 'name' variable as it is not quntitative
mlr.fit = lm(mpg ~. , data = Auto)
mlr.fit
summary(mlr.fit)

#i. As p value for F test is virtually 0, there is relationship between 
#response and predictors 
#ii. Statistically significant predictors : 
#displacement ,weight  year  , origin 
#iii. Significant predictor as p value is very less 

#Q9.d
par(mfrow = c(2,2))
plot(mlr.fit)
#Pattern in residual vs Fitted plot : problem with linearity 
#Ouotlier  points present : 14 
plot(hatvalues(mlr.fit)) 
##High liverage poiint present around 2 
#Q.9.e 
mlr.fit2 = lm(mpg~. + displacement : year , data = Auto)
summary(mlr.fit2)
#The interaction term of displacement and year 

mlr.fit3 = lm(mpg~. + displacement : year + weight:origin , data = Auto )
summary(mlr.fit3)
##High p values - cylinders and acceleration not significant

mlr.fit3 


mlr.fit4 = lm(mpg ~ displacement * year + weight * origin + horsepower , data = Auto)
mlr.fit4
summary(mlr.fit4)  

#Horsepower and weight are not significant at 0.001 level of significance. 
#We now try to fit some transformation 
par(mfrow = c(2,2))
plot(mlr.fit4)
##we try to find where the high leverage points and outliers 
plot(hatvalues(mlr.fit4))
#As we see it in the plot 
max(hatvalues(mlr.fit4))
which.max(hatvalues(mlr.fit4))
#Now we remove the leverage points
Auto = Auto[ -c(14) , ]
mlr.fit4 = lm(mpg ~ displacement * year + weight * origin + horsepower , data = Auto)
mlr.fit4
summary(mlr.fit4)  


par(mfrow = c(2,2))
plot(mlr.fit4)

#But still horsepower and weight cannot be rejected at 0.001 
#To find out transformation , we see pattern from scatterplots 
plot(y = mpg ,x = weight , pch = 16)  #It shows parabolic relationship 
plot(y = mpg , x = horsepower , pch = 16) #It shows parabolic relationship  

mlr.fit5 =  lm(mpg ~ displacement * year + weight : origin + I(horsepower^2) + I(weight^2) + origin, data = Auto)
summary(mlr.fit5) 

#not significant  

mlr.fit6 =  lm(mpg ~ displacement * year + weight : origin + I(horsepower^0.5) + I(weight^0.5) + origin, data = Auto)
summary(mlr.fit6) 
#Weight - origin interaction and origin alone are not significant 
mlr.fit7 =  lm(mpg ~ displacement * year  + I(horsepower^0.5) + I(weight^0.5), data = Auto)
summary(mlr.fit7)  

#Negligible p values , and R^2 = 0.8467

par(mfrow = c(2,2))
plot(mlr.fit7)
plot(hatvalues(mlr.fit7))


#Q10 

View(Carseats)
names(Carseats)
attach(Carseats)
#Q10(a)
mlr.fit1 = lm(Sales ~ Price + Urban + US , data = Carseats)

#Q10(b) - (c) We first see how R codes the dummy variables for qualitative variables 

contrasts(Urban)
contrasts(US) 

#R has made UrbanYes that takes value 1 , if Urban takes value Yes 
#R has made USYes that takes value 1 , if US takes value Yes 

# We assume the functional form to be : 
#   Y = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \beta_3 X_3 
# where : 
#   Y := Unit sales (in thousands) at each location 
#   X_1:= Price company charges for car seats at each site
#   X_2 := USYes : takes 1 if if store is located in US 
#   X_3 := UrbanYes : takes 1 if store is  in Urban area 
#   
#   How to interprete the coefficients?




#Q10(d)
summary(mlr.fit1)
#H0 can be rejected for Price and US , not for Urban 

#Q10(e)
mlr.fit2 = lm(Sales ~ Price + US , data = Carseats)
summary(mlr.fit2)

#Q10(f)
#Values of R^2 are 0.2393  which is very low 

#Q10(g)
#95% confidence interval for coefficients 
confint(mlr.fit2) 

#Q10(h)
par(mfrow = c(2,2))
plot(mlr.fit2)
plot(hatvalues(mlr.fit2) , ylab = "Levrage") 
max(hatvalues(mlr.fit2))
which.max(hatvalues(mlr.fit2)) 
##There is something really wrong with the plot 
#what is it? 


#Q11. 

