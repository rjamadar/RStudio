######################### Quesiton 3 ############################
# Install Packages
install.packages('tidyverse')
install.packages('modelr')
install.packages('broom') 

library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)      # helps to tidy up model outputs

####### Load data & Exploring data
(default <- as_tibble(ISLR::Default))
summary(default)
pairs(default)

install.packages('cowplot')
library(cowplot)
mosaicplot(student~default, data=default, color=TRUE)
ggplot(default, aes(x = default, y = balance)) +
  geom_boxplot() + theme_bw()
ggplot(default, aes(x = default, y = income)) +
  geom_boxplot() + theme_bw()

####### Part 2: Split the whole sample into a training set(60%) and testing set(40%)
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(default), replace = T, prob = c(0.6,0.4))
train <- default[sample, ]
test <- default[!sample, ]

####### Part 3: SLR and SLR
# Simple linear regresion 
model0 <- lm(default ~ balance, data = train)
summary(model0)

# Simple logistic regression
model1 <- glm(default ~ balance, family = "binomial", data = train)
summary(model1)

#oods ratio for default

exp(coef(model1))

# confidence intervals
exp(confint(model1))


# graph of the logistic model fit
default %>%
  mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
  ggplot(aes(balance, prob)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Balance") +
  ylab("Probability of Default")

####### Part 4:Assession coefficients
tidy(model1)
exp(coef(model1))

####### Part 5: Making prediction
predict(model1, data.frame(balance = c(1000, 2000)), type = "response")


####### Part 6: Add categorical variable
model2<- glm(default ~ balance+student, family = "binomial", data = train)
summary(model2)
tidy(model2)

####### Part 6: Full logistic regression
model3<- glm(default ~ balance + income + student, family = "binomial", data = train)
summary(model3)
tidy(model3)

####### Part 7: Prediction
new.df <- tibble(balance = 1500, income = 40, student = c("Yes", "No"))
predict(model3, new.df, type = "response")


####### Part 7: Model evaluation
## Likelihood ratio test (Goodness of fit test) using Chi-square on the ANOVA
anova(model1, model2, model3, test = "Chisq")

## R-square: Larger is better

library(pscl)
mr<-list(model1 = pscl::pR2(model1)["McFadden"],
         model2 = pscl::pR2(model2)["McFadden"],
         model3 = pscl::pR2(model3)["McFadden"])
mr

## Residual
# Model 1 (duplicate codes below for different model)
model1_data <- augment(model2) %>% 
  mutate(index = 1:n())

ggplot(model1_data, aes(index, .std.resid, color = default)) + 
  geom_point(alpha = .5) +
  geom_ref_line(h = 3)

model1_data %>% 
  filter(abs(.std.resid) > 3)

# Cook's distance
# Model 1 (duplicate codes below for different model)
plot(model3, which = 4, id.n = 5)
model1_data %>% 
  top_n(5, .cooksd)

# Validation of predicted values (test set error)
test.predicted.m1 <- predict(model1, newdata = test, type = "response")
test.predicted.m2 <- predict(model2, newdata = test, type = "response")
test.predicted.m3 <- predict(model3, newdata = test, type = "response")


list(
  model1 = table(test$default, test.predicted.m1 > 0.5) %>% prop.table() %>% round(3),
  model2 = table(test$default, test.predicted.m2 > 0.5) %>% prop.table() %>% round(3),
  model3 = table(test$default, test.predicted.m3 > 0.5) %>% prop.table() %>% round(3)
)

test %>%
  mutate(model1.pred = ifelse(test.predicted.m1 > 0.5, "Yes", "No"),
         model2.pred = ifelse(test.predicted.m2 > 0.5, "Yes", "No"),
         model3.pred = ifelse(test.predicted.m3 > 0.5, "Yes", "No")
         ) %>%
  summarise(m1.error = mean(default != model1.pred),
            m2.error = mean(default != model2.pred),
            m3.error = mean(default != model3.pred),
            )
####################### End of Quesiton 3 #########################



######################### Quesiton 4 ############################
install.packages("readr")
install.packages("forcats")
install.packages("multcomp")

library(readr)
library(forcats)
library(ggplot2)
library(dplyr)
library(knitr)
library(multcomp)

str(c.crime)

###### Part 1: Plotting the outcome variable, number of violent crimes(nv)
ggplot(c.crime, aes(x=nv)) + 
  geom_histogram(bins = 15) +
  xlab("Number of violent crimes")

###### Part 3: Checking assumptions

### Before checking the assumptions, let's clean up the data
# Combining the southern colleges and universities
c.crime <- c.crime %>%
  mutate(region, region = fct_recode(region, "S" = "SW", "S"="SE"))

# Removing Outlier
c.crime <- c.crime %>% 
  filter(nvrate<5)

# Checking mean and variance 
table1 <- c.crime %>%
  group_by(region, type) %>%
  dplyr::summarize(MeanCount = mean(nv, na.rm=TRUE),
                   VarCount = var(nv, na.rm=TRUE),
                   MeanRate = mean(nvrate, na.rm=TRUE),
                   VarRate = var(nvrate, na.rm=TRUE),
                   n = n())
kable(table1, booktabs=T, caption = 'The mean and variance of the violent crime rate by region and type of institution')

###### Part 4:  Boxplot
ggplot(c.crime, aes(x = region, y = nvrate, fill = type)) +
  geom_boxplot() +
  ylab("Violent crimes per 1000 students")

###### Part 5:  Model
model3 = glm(nv~type+region,family=poisson, offset=log(enroll1000),data=c.crime)
tmp <- capture.output(summary(model3))
cat(tmp[c(3:5, 10:18, 21:25)], sep='\n')

#oods ratio for default

exp(coef(model4))

# confidence intervals
exp(confint(model4))

###### Part 6:Multiple Comparisons
tmp <- capture.output(summary(glht(model3, mcp(region="Tukey"))))
cat(tmp[c(4:5, 10:22, 24)], sep='\n')

###### Part 7: interaction model
model4 = glm(nv ~ type + region + region:type, family = poisson, 
             offset = log(enroll1000), data = c.crime)
tmp <- capture.output(summary(model4))
cat(tmp[c(3:5, 10:22, 25:29)], sep='\n')

###### Part 8: ANOVA
anova(model3,model4,test="Chisq") # addition of intxn terms sign

##### Part 10: Quasipoisson
model5<-glm(formula = nv ~ type + region + region:type, family = quasipoisson, 
    data = c.crime, offset = log(enroll1000))
summary(model5)

#### Part 11: Compare models after adjusting for overdispersion
phi <- sum(resid(model4, type='pearson')^2) / model4$df.residual
drop.in.dev <- model3$deviance - model4$deviance
diff.in.df <- model3$df.residual - model4$df.residual
Fstat <- drop.in.dev / summary(model5)$dispersion
Fstat
1-pf(Fstat, diff.in.df, model4$df.residual)

####################### End of Quesiton 4 #########################