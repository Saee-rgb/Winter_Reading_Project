#############
##Chapter 4 
#############

#4.7.1 : Smarket Data
#loading the library
library(ISLR2)
#all the variables under consideration
names(Smarket)

#summary of data 
summary(Smarket)

#plot all possible scaterplots 
pairs(Smarket)

#to get correlation matrix 

cor(Smarket[ , -9])
#there appears to be little
#correlation between today’s returns and previous days’ returns. The only
#substantial correlation is between Year and Volume. 

attach(Smarket)
plot(Volume)

#Volume is incrasing over time 



#4.7.2 : Logistic regression 
glm.fits = glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
  data = Smarket , family = binomial
)

summary(glm.fits)

coef(glm.fits)

summary(glm.fits)$coef[ ,4]

#get the predicted value : 
glm.probs = predict(glm.fits , type = "response")
glm.probs[1:10]

#to see the encoding for dummy variable 
contrasts(Direction)

#to classify into 0 or 1 (Down or Up)
glm.pred = rep("Down" , 1250)
glm.pred[glm.probs  > 0.5] = "Up"


#to get the confusion matrix 
table(glm.pred , Direction)

#to get fraction of total correct prediction 
mean(glm.pred == Direction ) 

#Dividing data into  train and test for model evaluation : 

train = (Year < 2005)
Smarket.2005 = Smarket[!train, ]

dim(Smarket.2005)

Direction.2005 = Direction[!train]


#training and testing the model on two separate datasets 
glm.fits = glm(
  Direction  ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
  data = Smarket , family = binomial , subset = train
  
)

glm.probs = predict(glm.fits , Smarket.2005 , type = 'response')

glm.pred = rep("Down" , 252)
glm.pred[glm.probs > 0.5] = "Up"

#to get confusion matrix
table(glm.pred , Direction.2005)

#fraction of total correct predictions
mean(glm.pred == Direction.2005)

#test error : fractionn of total wrong predictions
mean(glm.pred != Direction.2005)



#refitting logistic regression with only significant variables
glm.fits = glm(Direction ~ Lag1 + Lag2 , data = Smarket , family = binomial , subset = train)

glm.probs = predict(glm.fits , Smarket.2005 , type = "response")

glm.pred = rep("Down" , 252)
glm.pred[glm.probs >0.5] = "Up"
#confusion matrix
table(glm.pred , Direction.2005)

#fraction of total correct prediction
mean(glm.pred == Direction.2005)

#fraction of correct predictions for "Up" 
106/(106 + 76)

#This suggests apossible trading strategy of buying on days when the model 
#predicts an increasing market, and avoiding trades on days when a decrease
#is predicted

#predicting for individual data points 

predict(glm.fits , 
        newdata =  data.frame(Lag1 = c(1.2 , 1.5) , Lag2 = c(1.1 , -0.8)) , 
        type = "response"
        )

##4.7.3 : Linear Discriminant Analysis 
library(MASS)
lda.fit = lda(Direction ~ Lag1 + Lag2 , data = Smarket , subset = train)
lda.fit 
plot(lda.fit)

##To interprete the output

#Prior probabilitues : Pi1 , Pi2 
#Group means : mu1 , mu2 
#Coefficients of LDA : 
# these are the multipliers of the elements of X = x in (4.24). If
#−0.642 × Lag1 − 0.514 × Lag2 is large, then the LDA classifer will predict
#a market increase, and if it is small, then the LDA classifer will predict a
#market decline. 
#This large or small , depends on cutoff point 

#plot : plots values of linear combination of coefficients for two classes separately 

lda.pred  =  predict(lda.fit , Smarket.2005)
names(lda.pred) 
#class : prediction of class  
#posterior : posterior probability
#x : value of linear discriminant

lda.class = lda.pred$class 

#get the confusion  matrix 
table(lda.class , Direction.2005)
#fraction of total correct classifications
mean(lda.class == Direction.2005)

#Making cutoff 0.5 
sum(lda.pred$posterior[, 1] >= .5)
sum(lda.pred$posterior[, 1] < .5)

lda.pred$posterior[1:20, 1]
lda.class[1:20]


#changing the cutoff point 
sum(lda.pred$posterior[, 1] > .9)

## 4.7.4 Quadratic discriminant analysis

qda.fit =  qda(Direction ~ Lag1 + Lag2 , data = Smarket , subset = train)
qda.fit


qda.class = predict(qda.fit , Smarket.2005)$class
#to get the confusion matrix 
table(qda.class , Direction.2005)

#to get fraction of correct prediction 
mean(qda.class == Direction.2005)
################################################################

##4.7.5 : Naive Bayes 
library(e1071)
nb.fit = naiveBayes( Direction ~ Lag1 + Lag2 , data = Smarket , subset = train)
nb.fit

#A-priori pobabilities : Pi1, Pi2 estimates
#Conditional probabilities : 
#1st column is mean , second is variance according to each class

#verification : 

mean(Lag1[train][Direction[train]== "Down"])
sd(Lag1[train][Direction[train] == "Down"])

#prediction, confusion matrix and test error

nb.class = predict(nb.fit , Smarket.2005)
table(nb.class , Direction.2005)
mean(nb.class == Direction.2005)
#59% correct predictions : slightly worse than QDA, but better than LDA

#estimating probability for going into each class
nb.preds <- predict(nb.fit, Smarket.2005, type = "raw")
nb.preds[1:5, ]

#4.7.6 : K Nearest Neighbors 
library(class)

#arguments of knn 
#train.X : A matrix containing the predictors associated with the training data,
#          labeled below.
#test.X : A matrix containing the predictors associated with the data for which
#         we wish to make predictions
#train.Direction : A vector containing the class labels for the training observations
# A value for K, the number of nearest neighbors to be used by the
#classifer.


train.X = cbind(Lag1 , Lag2)[train , ]
test.X = cbind(Lag1 , Lag2)[!train , ]
train.Direction = Direction[train]

#we have to set the seed , in order to preserve the tie breaking done randomly by R 
set.seed(1)
knn.pred = knn(train.X , test.X , train.Direction , k = 1)

table(knn.pred , Direction.2005)

mean(knn.pred == Direction.2005)
#not very good as only 50% of observations are correctly classified 

#we repeat with k = 3 
knn.pred = knn(train.X , test.X , train.Direction , k = 3)

table(knn.pred , Direction.2005)

mean(knn.pred == Direction.2005)
#slightly better 

knn.pred = knn(train.X , test.X , train.Direction , k = 4)

table(knn.pred , Direction.2005)

mean(knn.pred == Direction.2005)
#not large change 


###Applying knn on different dataset 
dim(Caravan)
attach(Caravan)
summary(Purchase)

standardized.X <- scale(Caravan[, -86]) #standardizing data , 86th column in qualitative
var(Caravan[ , 1])
var(Caravan[ ,2])

var(standardized.X[ ,1])
var(standardized.X[ ,2])   #mean 0 and  variance 1 for alquantitative columns 


#train test . test set of first 1000 observation
test = 1:1000
train.X = standardized.X[ -test , ]
test.X = standardized.X[test, ]
train.Y = Purchase[-test]
test.Y = Purchase[test]


set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)
mean(test.Y != "No")


#=====
# #Making train and test randomly 
# test = sample(1:5822 , size = 1000 , replace = "FALSE")
# train.X = standardized.X[ -test , ]
# test.X = standardized.X[test, ]
# train.Y = Purchase[-test]
# test.Y = Purchase[test]
# 
# 
# set.seed(1)
# knn.pred = knn(train.X, test.X, train.Y, k = 1)
# mean(test.Y != knn.pred)  #0.102
# mean(test.Y != "No") 
#==================================================================
table(knn.pred , test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)

#improved ! 

##Trying with logistics regression 
glm.fits = glm(Purchase ~. , data = Caravan , 
               family = binomial ,subset = -test)

glm.probs = predict( glm.fits , Caravan[test , ], type = "response") 
glm.pred = rep("No" , 1000)
glm.pred[glm.probs>0.5] = "Yes"
table(glm.pred , test.Y)
#problem because of 0

#decreasing the cutoff
glm.pred = rep("No" , 1000)
glm.pred[glm.probs > 0.25] = "Yes"
table(glm.pred , test.Y)
##33 people will purchase insure , we are 33% sure 


#4.7.7 : Poisson regression  on Bikeshare data
attach(Bikeshare)
dim(Bikeshare)
names(Bikeshare)


#least square 
mod.lm = lm(
  bikers ~ mnth + hr + workingday + temp + weathersit , 
  data = Bikeshare
  )

summary(mod.lm)


##Another way of coding 
contrasts(Bikeshare$hr) = contr.sum(24)
contrasts(Bikeshare$mnth) = contr.sum(12)
mod.lm2 <- lm(
  bikers ~ mnth + hr + workingday + temp + weathersit,
  data = Bikeshare
)
summary(mod.lm2)

#2, the coeffcient estimate for the last level of month 
# equals the negative of the sum of the coefficient estimates for
#all of the other levels


#BUt predictions are same 
sum((predict(mod.lm) - predict(mod.lm2))^2)
all.equal(predict(mod.lm), predict(mod.lm2))


#coefficients of the last month are deliberately calculated 
coef.months <- c(coef(mod.lm2)[2:12],
                 -sum(coef(mod.lm2)[2:12]))


##Making the plots 
plot(coef.months, xlab = "Month", ylab = "Coefficient",
     xaxt = "n", col = "blue", pch = 19, type = "o")

axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A",
                                       "M", "J", "J", "A", "S", "O", "N", "D"))
coef.hours <- c(coef(mod.lm2)[13:35],
                -sum(coef(mod.lm2)[13:35]))


plot(coef.hours, xlab = "Hour", ylab = "Coefficient",
       col = "blue", pch = 19, type = "o")

#fittimg poisson regression
mod.pois <- glm(
  bikers ~  mnth + hr + workingday + temp + weathersit,
  data = Bikeshare, family = poisson
)

 
summary(mod.pois)



coef.mnth <- c(coef(mod.pois)[2:12],
               -sum(coef(mod.pois)[2:12]))
plot(coef.mnth, xlab = "Month", ylab = "Coefficient",
       xaxt = "n", col = "blue", pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J"
                                       , "J", "A", "S", "O", "N", "D"))
coef.hours <- c(coef(mod.pois)[13:35],
                  -sum(coef(mod.pois)[13:35]))

plot(coef.hours, xlab = "Hour", ylab = "Coefficient",
       col = "blue", pch = 19, type = "o")

plot(predict(mod.lm2), predict(mod.pois, type = "response"))
abline(0, 1, col = 2, lwd = 3)