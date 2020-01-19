library("ISLR")
library ("gpairs")
library("corrplot")
#library("dlookr")
names(Carseats)
head(Carseats)

#### Question 1
str(Carseats)
summary(Carseats)


#### Question 2
# Omit the categorical variables
Carseats1<-Carseats[,c(1,2,3,4,5,6,8,9)]
summary(Carseats1)
# Pairwise Correlation Matrix
gpairs(Carseats1)

# Create correlation table after dropping the Advertising variable
cor(Carseats1[,-4])

#Pairwise COrrelation Matrix with log transformation

Carseats1$logdave<-log(Carseats1$Advertising)
gpairs(Carseats1)

#### Question 3-5
# Fit the multiple regression model
q3fit1<-lm(Sales~Price, data=Carseats)
q3fit2<-lm(Sales~Population, data=Carseats)
q3fit3<-lm(Sales~Income, data=Carseats)
q3fit4<-lm(Sales~US, data=Carseats)
# Print output
summary(q3fit1)
summary(q3fit2)
summary(q3fit3)
summary(q3fit4)
# Examine residual plot and normal Q-Q plot
# par(mar=c(1,1,1,1)) # If you got error of the margin size
par(mfrow=c(1,1))
plot(q3fit1)
plot(q3fit2)
plot(q3fit3)
plot(q3fit4)

# Copy the lines from 24-28 and paste here.
# Modify the copy-pasting code
# Chance the name of predictor and model name

#### Question 6
confint(q3fit1) # Reject H0 if the confidence interval contains 0
confint(q3fit2)
confint(q3fit3)
confint(q3fit4)

#### Question 7
plot(q3fit1)
plot(q3fit2)
plot(q3fit3)
plot(q3fit4)

