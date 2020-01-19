library(ISLR)
library("leaps")

# Interactions
sm<-lm(Sales~TV+Radio, data=Advertising)
summary(sm)
par(mfrow=c(2,2))
plot(sm)

mm1<-lm(Sales~TV+Radio+TV*Radio, data=Advertising)
summary(mm1)
plot(mm1)

null=lm(Balance~1, data=Credit)
full=lm(Balance~.-ID, data=Credit)
step(full, data=Credit, direction="backward")

mm2<-lm(Balance ~ Income + Limit + Rating + Cards + Age + Student, data = Credit)
summary(mm2)
plot(mm2)

mm3<-lm(Balance~Income+Student+Income*Student, data=Credit)
summary(mm3)
plot(mm3)

mm4<-lm(Balance~Limit + Rating + Cards + Age+ Income+Student+Income*Student, data=Credit)
summary(mm4)
plot(mm4)

# Auto Data set
names(Auto)
summary(Auto)
plot(Auto)
gpairs(Auto)

plot(Auto$mpg~Auto$cylinders)
Cylinders1<-as.factor(Auto$cylinders)
Year1<-as.factor(Auto$year)
Origin1<-as.factor(Auto$origin)

summary(Cylinders1)
plot(Auto$mpg~Cylinders1)

fit1<-lm(Auto$mpg~Cylinders1)
summary(fit1)

summary(Year1)
plot(Auto$mpg~Year1)

fit2<-lm(Auto$mpg~Year1)
summary(fit2)

plot(Auto$mpg~Auto$horsepower)
horsepower2<-as.numeric(Auto$horsepower)
plot(Auto$mpg~horsepower2)
mm2_1<-lm(Auto$mpg~horsepower2)
plot(mm2_1)
mm2_2<-lm(Auto$mpg~horsepower2+I(horsepower2^2))
summary (mm2_1)
summary (mm2_2)


mm2_3<-lm(Auto$mpg~horsepower2+I(horsepower2^2)+I(horsepower2^3)+I(horsepower2^4)+I(horsepower2^5))
summary(mm2_3)
plot(mm2_3)

AIC(mm2_1, mm2_2, mm2_3)
BIC(mm2_1, mm2_2, mm2_3)

