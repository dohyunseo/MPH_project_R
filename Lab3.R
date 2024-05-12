rm(list = ls()) #clearing working directory to get a fresh start.


#Libraries:
library(questionr)
library(haven)
library(epitools)
library(DescTools)
library(epiDisplay)
library(tidyverse)
library(dplyr)
library(nnet)
library(reshape2)
library(MASS)
library(Hmisc)
library(stargazer)
library(stats)


diabetes <- read_dta("diabetes.dta")
View(diabetes)

diabetes$group <- factor(diabetes$group,
                    levels = c(1,2,3),
                    labels = c("no diabetes", "chemical diabetes", "overt diabetes"))


#question 1: Unordered polytomous regression model:
library(mlogit) #### NOTE: "nnet" package USES A DIFFERENT SEARCH FUNCTION AND WILL PRODUCE DIFFERENT BETAS. 
### "mlogit" PACKAGE WILL PRODUCE MAXIMUM LIKELIHOOD ESTIMATES AND IS THE MORE ACCURATE PACKAGE FOR MOST EPI ANALYSES.

diabetes_df <- as.data.frame(diabetes) #changes from tibble to data frame format to use the mlogit package.
diabetes2 <- dfidx(data=diabetes_df, choice="group", shape="wide") #reshape the data to make mlogit run correctly 
head(diabetes2) #just checking things run through correctly. 

##coefficients:
modelx = mlogit(group ~ 1 | relwt, data = diabetes2, reflevel="no diabetes")
summary(modelx)
stargazer(modelx, type="text") #makes output prettier 

modelxx = mlogit(group ~ 1 | glutest, data = diabetes2, reflevel="no diabetes")
summary(modelxx)
stargazer(modelxx, type="text") #makes output prettier 

modelxxx = mlogit(group ~ 1 | sspg, data = diabetes2, reflevel="no diabetes")
summary(modelxxx)
stargazer(modelxxx, type="text") #makes output prettier 

##ORs:
exp(coef(modelx))
exp(coef(modelxx))
exp(coef(modelxxx))

#question 4: 

##a. if ordered polytomous regression model:
diabetes$group = as.factor(diabetes$group)
modelord = polr(group ~ sspg, data = diabetes)
summary(modelord)
coef(summary(modelord))

modelord2 = polr(group ~ sspg + relwt, data = diabetes)
summary(modelord2)
((0.0260-0.0276)/0.0260)

##b. if un-ordered polytomous regression model:

summary(modelxxx) ## modelxx from q1 is the unadjusted, unordered model. 

model_unord_adj = mlogit(group ~ 1 | sspg + relwt, data = diabetes2, reflevel="no diabetes")
summary(model_unord_adj)


((0.0215741-0.0192871)/0.0215741) 
#&:
((0.0421901-0.0428916)/0.0421901) 

