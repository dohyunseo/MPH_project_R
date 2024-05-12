# Libraries:
library(ggplot2)
library(sandwich)
library(msm)
library(lmtest)
library(questionr)
library(haven)
library(epitools)
library(DescTools)
library(epiDisplay)
library(tidyverse)
library(dplyr)
library(gtsummary)
library(sjPlot)

montana <-read_dta("montana.dta")
View(montana)

## Question 1.1: Poisson regression

montana$arsenic = as.factor(montana$arsenic)
model1 <- glm(formula=rescadth ~ arsenic + offset(log(pyrs)), family=poisson(link="log"), data=montana)
summary(model1)
tbl_regression (model1, exponentiate=TRUE)
table1 <-tab_model(model1)
table1
logLik(model1)

exp(coef(model1))
exp(confint.default(model1))

## Question 1.2:  
montana$agegrp = as.factor(montana$agegrp)
model1_conf <- glm(formula=rescadth ~ arsenic + agegrp + offset(lpyrs), family=poisson(link="log"), data=montana)

tbl_regression (model1_conf, exponentiate=TRUE)
table1 <-tab_model(model1_conf)
summary(model1_conf)
((0.80-0.73)/0.80)
((0.60-0.71)/0.60)
((1.00-1.41)/1.00)
logLik(model1_conf)

## Question 1.3: 
model2 <- glm(formula=rescadth ~ arsenic + offset(lpyrs) + agegrp + hire + arsenic*hire, family=poisson(link="log"), data=montana)
summary(model2)
tbl_regression (model2, exponentiate=TRUE)
table1 <-tab_model(model2)
logLik(model2)

#to get the -2 log likelihood
(-178.30)*2
model3 <- glm(formula=rescadth ~ arsenic + offset(lpyrs) + agegrp + hire, family=poisson(link="log"), data=montana)
summary(model3)
tbl_regression (model3, exponentiate=TRUE)
table1 <-tab_model(model3)
logLik(model3)
#to get the -2 log likelihood
(-178.93)*2

#comparing both models
lrtest(model2,model3)

library(oddsratio)
library (logbin)

adult_bmi<- read_dta("adult_bmi.dta")

## Question 1: 

adult_bmi$binbmi = adult_bmi$bmi
adult_bmi$binbmi [adult_bmi$bmi < 25] = "<25"
adult_bmi$binbmi [adult_bmi$bmi >= 25] = ">=25"

adult_bmi$binbmi = as.factor(adult_bmi$binbmi)
str(adult_bmi)

# Logistic regression :
model1 <- glm(binbmi ~ prepregbmi, family=binomial, data = adult_bmi)
summary(model1)
exp(coef(model1))
exp(confint.default(model1))

# Question 2: OR with maternal pre pregnancy change of 10 units:
or_glm(data=adult_bmi, model=model1, incr=list(prepregbmi = 10))

## or manually
(0.13116*10)
exp((0.13116*10))

# Log-binomial analysis -> Relative Risk Regression:
model2 <- logbin(binbmi ~ prepregbmi, data = adult_bmi)
summary(model2)
exp(coef(model2))
exp(confint.default(model2))

#another way to obtain RR -> poisson regression with robust variance
adult_bmi$binbmi2 = adult_bmi$bmi
adult_bmi$binbmi2 [adult_bmi$bmi < 25] = 0
adult_bmi$binbmi2 [adult_bmi$bmi >= 25] = 1
adult_bmi$binbmi = as.factor(adult_bmi$binbmi)

model2poisson <- glm(binbmi2 ~ prepregbmi, family = poisson(link = "log"), data = adult_bmi)
summary(model2poisson)
exp(coef(model2poisson))
exp(confint.default(model2poisson))

# RR with maternal pre-pregnancy change of 10 units:
exp(0.10969*10)



