
################
#### Exercises
################
#8.a
college = read.csv("College.csv")
names(college)
View(college)
#8.b
rownames(college) = college[ ,1]
View(college) 
college = college[ ,-1]
View(college) 
#8.c.i
summary(college)
#8.c.ii
pairs(college[ , 2:11]) 
#8.c.iii
attach(college)
Private = as.factor(Private)
plot(y = Outstate , x = Private , varwidth = T , 
     xlab = "Private" , ylab = "Outstate" , main = "Boxplot") 
#8.c.iv : binning 
Elite = rep("No" , nrow(college))
Elite[college$Top10perc >50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college , Elite)


summary(college) 

plot(y = Outstate , x = Elite , varwidth = T ,horizontal = T,
     xlab = "Outstate" , ylab = "Private" , main = "Boxplot")

par(mfrow =c(2,2))
hist(college$Apps ,xlim = range(college$Apps), xlab = "Apps" , main = " ")
hist(college$Accept  , col = "pink" , xlab = " Accept" , main = " ")
hist(college$Outstate , breaks = 30 , col= "lightgreen" , xlab = "Outstate" , main = " ")
hist(college$Outstate , breaks = 20 , col = "violet" , xlab = "Outstate" , main = " ")


#9.a 
Auto = read.csv("Auto.csv")
View(Auto)
names(Auto)

#qualitative : name 
#quantitative : remaining all 
Auto$horsepower = as.numeric(Auto$horsepower)
Auto = na.omit(Auto)
Auto = Auto[  , 1: 8]

for( i in 1:8)
{
  print(range(Auto[ , i]))
  print(mean(Auto[ ,i]))
  print(sd(Auto[ , i]))
}


Auto = Auto[ -c( 10: 85) , ]
for( i in 1:8)
{
  print(range(Auto[ , i]))
  print(mean(Auto[ ,i]))
  print(sd(Auto[ , i]))
}

attach(Auto)
plot(x = displacement , y = mpg ,pch = 16 , col = cylinders , main = cor(displacement , mpg))
plot(x = acceleration , y = mpg ,pch = 16 , col = cylinders , main = cor(acceleration , mpg))
plot(x = horsepower , y = mpg ,pch = 16 , col = cylinders , main = cor( horsepower, mpg))
plot(x = weight , y = mpg ,pch = 16 , col = cylinders , main = cor( weight, mpg))



#10. 
#10.a 
library(ISLR2)
Boston
?Boston 
dim(Boston) #506 rows : total no of observations , 13 columns : total no of features 

#10.b
pairs(Boston)


