##########
##11.8.1 : Brain cancer data 
#########
library(ISLR2)

names(BrainCancer)
dim(BrainCancer)

##Briefly examine the data 
attach(BrainCancer)
table(sex)
table(diagnosis)
table(status)
##How is status variable encoded ? 
#status = 1 : uncensored observation 
#status = 0 : censored observation 

##Recreate Kaplan Meiersurvival curve 
#time corrospons to y_i, the time to ith event(either censored or death)

library(survival)
fit.surv = survfit(Surv(time , status) ~ 1 )
plot(fit.surv , xlab = "Months" , ylab = "Estimates Probably Of Survival")

##Recreate Kaplan Meier survival curve startified by sex
fit.sex = survfit(Surv(time , status) ~ sex)
plot(fit.sex , xlab = "Months" , ylab = "Estimates Probably Of Survival" , col = c(2,4))

legend("bottomleft" , levels(sex) , col = c(2,4) , lty = 1)

##Perform logrank test 

logrank.test = survdiff(Surv(time , status) ~ sex)
logrank.test
#no evidence of a diference in survival between the two sexes.

##fit Cox proportional hazards model , with sex as only predictor 

fit.cox = coxph(Surv(time , status) ~ sex)
summary(fit.cox)
#to get values which are not rounded off 

summary(fit.cox)$logtest[1]
summary(fit.cox)$waldtest[1]
summary(fit.cox)$sctest[1]
logrank.test$chisq

#not enough difference to show that there is enough difference between survival of females 
#males 

#the score test from the Cox model is exactly equal to the log rank test statistic! 


##fit the model that uses all the predictors 

fit.all = coxph(Surv(time , status)~ sex + diagnosis + loc + ki + gtv + stereo)

fit.all

#baseline corresponds to meningioma

#quantitative variables : mean 
#qualitative variables : modal value 


# 
modaldata <- data.frame(
  diagnosis = levels(diagnosis),
  sex = rep("Female", 4),
  loc = rep("Supratentorial", 4),
  ki = rep(mean(ki), 4),
  gtv = rep(mean(gtv), 4),
  stereo = rep("SRT", 4)
)

survplots = survfit(fit.all , newdata = modaldata)

plot(survplots , xlab ="Months" , ylab = "Survival Probability" , col = 2:5)

legend("bottomleft" , levels(diagnosis) , col = 2:5 , lty = 1)

############
###11.8.2 Publication Data
############

##Recreating Kaplan Meier curve startified on posres variable 
fit.posres = survfit(
   
Surv(time, status)~posres , data = Publication
)

plot(fit.posres , xlab = "Months" , ylab = "Probability of Not being Published " , col = 3:4)

legend("topright", c("Negative Result", "Positive Result"),
       col = 3:4, lty = 1) 

##Fitting the model 
fit.pub = coxph(Surv(time , status) ~ posres , data = Publication)
fit.pub

#providing no evidenceof a diference in time-to-publication between studies with positive versus
#negative results

#same oucome from logrank test 
logrank.test = survdiff(Surv(time , status) ~ posres , data = Publication)
logrank.test

##results change dramatically when we include other predictors 

fit.pub2 = coxph(Surv(time , status) ~ .-mech , data = Publication)
fit.pub2
#clinend, posres , impact variables included 


#############
###11.8.3 Call Centre data 
#############
#Simulating survival data 
# observed wait times (in seconds) for 2,000 customers who have
#phoned a call center
#censoring occurs if a customer hangs up before his or her call is answered.
# Operators : the number of call center operators available at the time of the call, which can range from 5 to 15),
# Center : (either A, B, or C), 
# Time :  of day (Morning, Afternoon, or Evening)

set.seed(4)
N = 2000 
N <- 2000
Operators <- sample(5:15, N, replace = T)
Center <- sample(c("A", "B", "C"), N, replace = T)
Time <- sample(c("Morn.", "After.", "Even."), N, replace = T)
X <- model.matrix( ~ Operators + Center + Time)[, -1]

#design matrix
X[1:5 , ]
#specifying coefficients and hazard functions 
true.beta = c(0.04 , -0.3 ,0 , 0.2 , -0.2)
h.fn = function(x) return(0.00001*x)
 

#simulating the data
library(coxed)
queuing =  sim.survdata(N = N, T = 1000, X = X,
                        beta = true.beta, hazard.fun = h.fn)

names(queuing)

#observed data 
# y : event time 
#  failed : indicator , TRUE if call was answered and FAILED if customer hung up 

mean(queuing$data$failed)
# 

head(queuing$data)


#Plotting Kaplan Meier curves stratified by Centre 
par(mfrow = c(1, 2))

fit.Center <- survfit(Surv(y, failed) ~ Center,
                        data = queuing$data)

plot(fit.Center, xlab = "Seconds",
       ylab = "Probability of Still Being on Hold",
       col = c(2, 4, 5) , main = "Stratified by Centre")

legend("topright",
         c("A", "B", "C"),
         col = c(2, 4, 5), lty = 1)

#Plotting Kaplan Meier curves stratified by Time 
fit.Time <- survfit(Surv(y, failed)~ Time,
                    data = queuing$data)
plot(fit.Time, xlab = "Seconds",
       ylab = "Probability of Still Being on Hold",
       col = c(2, 4, 5) , main = "Stratified by time")
legend("topright", c("Morning", "Afternoon", "Evening"),
         col = c(5, 2, 4), lty = 1)


#logrank test for centres 
survdiff(Surv(y, failed) ~ Center, data = queuing$data)
#highly significant difference in centres 

#logrank test for time 
survdiff(Surv(y, failed) ~ Time, data = queuing$data)
#highly significant difference in Time zones 

#fitting Cox's proportional model to data 
fit.queuing <- coxph(Surv(y, failed) ~ .,data = queuing$data)
fit.queuing


#The p-values for Center = B, Time = Even. and Time = Morn. are very small.
#It is also clear that the hazard — that is, the instantaneous risk that a call
#will be answered — increases with the number of operators.