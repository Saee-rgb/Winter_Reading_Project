##############
#ISLR Chapter2
##############

#### Lab session

##2.1 Basic commands 
x  <- c(1 , 6, 2) # c() : concatenate  , <- to assign the value into variable
x 

y = c( 1 , 4, 3) # = can also be used 

length(x)
length(y)
x + y   # the length of x and y should be same 

ls() # Lists out all the objects in environment 
rm( x , y ) # removes the specified objects from enviroment

rm(list = ls())  #removes all the specified objects from environment 

?matrix    # go the help page of matrix() , to know more about matrix function
x = matrix(c(1 ,2 , 3, 4) , 2 , 2)  #populate matrix by columns 

x = matrix(c(1 ,2 , 3, 4) , 2 , 2 , byrow = TRUE) 
sqrt(x)
x^2 


x = rnorm(50)  #sample from standard normal of size 50 
y = x + rnorm(50 , mean = 50 , sd = 0.1)
cor(x , y)  # to get correlation between x and y 


set.seed(1234)  #to get the same random sample every time . The numerical 
                #argument given is arbitary 
rnorm(50)



set.seed(3)
y = rnorm(100)
mean(y)
var(y)
sqrt(var(y))   #calculating standard deviation from variance 
sd(y)          #standard deviation by using inbuilt R fucntion 


##2.2 Graphics 
x = rnorm(100)
y = rnorm(100)
plot(x,y)
plot( x, y , xlab = " This is the x axis" , 
      ylab = " This is the y axis" , 
      main = "Plot of X vs Y ")


pdf("Figure.pdf")
plot( x , y , col = "green")
dev.off()   #indicates to R that we are done creating plot 


jpeg("Figure_copy.jpeg")
plot(x , y , col = "violet" , main = "Saved as jpeg")
dev.off()  

x = seq(1 , 10)
x 

x = 1: 10 
x

x = seq(-pi , pi , length = 50)
x

x = -pi : pi
y = x 
f = outer( x , y , function(x , y ) cos(y)/(1 + x^2))
contour(x , y ,f)  
#Error : incraesing 'x' and 'y' values expected 
contour(x, y, f, nlevels = 45, add = T) 
fa =  (f - t(f)) / 2 
contour(x, y, fa, nlevels = 15)


#to produce a heatmap 
image(x, y, fa) 

#persp : to produce a 3D plot 
persp(x, y, fa)

#theta , phi : to control the angles at which the plot is viewed
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 20)
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 30, phi = 40)

## 2.3 Indexing the data 
A = matrix(1:16 , 4, 4)
A 

A[2 ,3]
A[c(1 ,3) , c(2,4)]
A[1 : 3 , 2:4]
A[1: 2 , ]
A[ , 3:4]
A[1 ,]
A[ ,3]

A[ -c(1 ,3) , ]
A[ -c(1 ,3) , -c(1 , 3, 4)]

B = matrix(1 : 16 , 8, 2)
B

dim(B)  
nrow = dim(B)[1]
ncol = dim(B)[2]

##2.4 Loading the dataset 
#read.table()
#write.table()
library(ISLR2)
head(Auto)
View(Auto)
# 
# Auto = read.csv("Auto.csv")
# 
# 
# 
# # Get the current working directory
# current_directory <- getwd()
# 
# # Specify the file name
# file_name <- "Auto.data"
# 
# # Get the full path to the file relative to the current working directory
# file_path <- file.path(current_directory, file_name)
# 
# # Output the file path
# print(file_path)
# 
# 
#DOUBT IN read.table() and read.csv() : Issue with working directory. 
#Not being able to load , even after working directory is same

dim(Auto)  
Auto = na.omit(Auto) 
dim(Auto)   ##No row had misisng values 
names(Auto) 



##2.5 : Additional Graphical and numerical summaries 
plot(Auto$cylinders  , Auto$mpg)
attach(Auto)
plot(cylinders , mpg)
cylinders = as.factor(cylinders)

plot(cylinders , mpg)  # AUtomatically generates 
plot(cylinders , mpg , col = "red" , varwidth = T)
plot(cylinders , mpg , col = "red" , varwidth = T , horizontal = T) 
##
#varwidth: This argument, when set to TRUE, adjusts the width of the boxes in a boxplot based on the sample sizes of the groups. 
#Larger groups will have wider boxes, allowing for a visual representation of the distribution and spread of data within different categories or groups.
#If set to FALSE, all boxes will have the same width 

#horizontal : Makes boxplot horizontal if set to TRUE


plot(cylinders , mpg, col = "red", varwidth = T,
    xlab = "cylinders", ylab = "MPG") 

hist(mpg)
hist(mpg , col = 2 )
hist(mpg , col= 2 , breaks = 15)

pairs(Auto)  #pairs of all the possible scatterplots  

pairs( ~ mpg + displacement + horsepower + weight + acceleration,
      data = Auto)  #product scatterplots for subset of variables 

plot(horsepower , mpg)
identify(horsepower , mpg , name) ##doubt : how does identify function really works?

summary(Auto)
summary(mpg)


#savehistory() : Save all the previous histroy 
#loadhistory() : load the  history 
#q()

