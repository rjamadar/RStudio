##################
### QUESTION 1 ###
##################
library(gpairs)
library(ggplot2)
# Summary of the data set
summary(movie)
# additional descriptive statistics
# first-year box office receipts/millions
mean(movie$y) 
median(movie$y)
sd(movie$y)

# total production costs/millions
mean(movie$x1)
median(movie$x1)
sd(movie$x1)
# total promotional costs/millions
mean(movie$x2)
median(movie$x2)
sd(movie$x2)
# total book sales/millions
mean(movie$x3)
median(movie$x3)
sd(movie$x3)

#--------------------------
q3fit1<-lm(y~x1, data=movie)
q3fit2<-lm(y~x1+x2, data=movie)
q3fit3<-lm(y~x1+x2+x3, data=movie)


par(mfrow=c(1,1))
plot(q3fit1)

plot(q3fit2)
plot(q3fit3)

# Part 2
gpairs(movie)
# log transformation----------
movie$logmovie<-log(movie$y)
gpairs(movie)
#--------------

#logY<-log(movie$y)
#logx1<-log(movie$x1)
#logx2<-log(movie$x2)
#logx3<-log(movie$x3)
#logmovie<- data.frame (logY, logx1, logx2, logx3)
#gpairs(logmovie)
#plot(logmovie)
#summary(logmovie)

# Part 3
cor(movie)

# Part 4 & 5
m1<-lm(y~x1, data=movie)
summary(m1)

m2<-lm(y~x1+x2, data=movie)
summary(m2)

m3<-lm(y~x1+x2+x3, data=movie)
summary(m3)
# Part 6
confint(m1)
confint(m2)
confint(m3)
# Part 8
par(mfrow=c(2,2))
plot(m1)
summary(influence.measures(m1))

par(mfrow=c(2,2))
plot(m2)
summary(influence.measures(m2))

par(mfrow=c(2,2))
plot(m3)
summary(influence.measures(m3))


##################
### QUESTION 2 ###
##################
library(gpairs)

# summary of expense and csat columns, all rows
summary(states)

# Take subset for the Questions
sub.state <- subset(states, select = c("region", "house", "senate", "percent", "income", "expense", "csat"))
summary(sub.state )


#---------------

#sub.state$logstate<-log(sub.state)
# Part 1
plot(sub.state[4:7])

# Part 2
cor(sub.state[4:7])

# Part 3
m2<-lm(csat~expense, sub.state)
summary(m2)

m3<-lm(csat~income, sub.state)
summary(m3)

m4<-lm(csat~percent, sub.state)
summary(m4)





# Part 4
par(mfrow=c(2,2))
plot(m2)

summary(influence.measures(m2))

# Part 5
sub.state$region<-as.factor(sub.state$region)
m3<-lm(csat~region, sub.state)
m3_1 <- update(m3, data=na.omit(sub.state))
summary(m3_1)

##################
### QUESTION 3 ###
##################
install.packages("fpp")
install.packages("SciViews")

library(fpp)
library(leaps)
library(SciViews)

# Part 1
summary(credit)
pairs(credit[,-(4:5)], diag.panel=panel.hist, data=credit) # The printed numbers and words on the histogram are values (Please ignore them)

# Part 2
null<-lm(score~1, data=credit) # Null model
full<-lm(scorecredit$logtemp<-log(credit$time.employed)
gpairs(credit)~.-fte -single, data=credit) # Full model
step(null, scope=list(lower=null, upper=full), direction="forward", data=credit)
step(full, data=credit, direction="backward")

bs=regsubsets(score~.-fte -single, data=credit)
par(mfrow=c(2,2))
plot(bs, scale="bic")
plot(bs, scale="Cp")
plot(bs, scale="adjr2")
plot(bs, scale="r2")

## Please replace "xx" to variables based on your analysis results.
best1<-lm(score~savings + income + time.employed + time.address, credit)
best2<-lm(score~savings + income + time.employed, credit)
summary(best1)
summary(best2)

anova(best1, best2)

best1_Q2<-lm(score~savings + income + time.employed + time.address, credit)
summary(best1_Q2)

#Part 3
plot(best1_Q2)

#Part 4
ncredit <- data.frame(score=credit$score, 
                      log.savings=log(credit$savings+1), 
                      log.income=log(credit$income+1), 
                      log.address=log(credit$time.address+1),
                      log.employed=log(credit$time.employed+1), 
                      fte=credit$fte, single=credit$single)

pairs(ncredit [,1:5],diag.panel=panel.hist)

##log-model
logm1<-lm(ncredit$score~ncredit$log.savings+ncredit$log.income+ncredit$log.address+ncredit$log.employed)
summary(logm1)
plot(logm1)

##################
### QUESTION 4 ###
##################

# Part 1
sub.prestige<-Prestige[,c(1:4)]
summary(sub.prestige)

# Part 2 and 3
gpairs(sub.prestige)

sub.prestige$logpres<-log(Prestige$prestige)
gpairs(sub.prestige)



# Question 4
# Stepwise
null=lm(prestige~1, data=sub.prestige) # original subset
null
summary(null)

full=lm(prestige~., data=sub.prestige)
full
summary(full)

step(null, scope=list(lower=null, upper=full), data=sub.prestige, direction="forward")
step(full, data=sub.prestige, direction="backward")
step(null, scope = list(upper=full), data=sub.prestige, direction="both")

# Best Subset
bs1=regsubsets(prestige~., data=sub.prestige)

par(mar=c(1,1,1,1))
dev.off()
plot(bs1, scale="bic")
plot(bs1, scale="Cp")
plot(bs1, scale="adjr2")
plot(bs1, scale="r2")
summary(bs1)
# Part 6
plot(best)
