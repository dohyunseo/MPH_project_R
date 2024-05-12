library(epiR)
library(mStats)

esoph_cancer <- read_dta("esoph_cancer.dta")
View(esoph_cancer)

#categorical variables from previous labs
esoph_cancer$agegrp [esoph_cancer$age>= 21 & esoph_cancer$age <= 59] ="<60 years"
esoph_cancer$agegrp [esoph_cancer$age > 59 & esoph_cancer$age <= 91] =">=60 years"

esoph_cancer$alcgrp [esoph_cancer$alcohol <= 39] ="0-39 g/day"
esoph_cancer$alcgrp [esoph_cancer$alcohol > 39] ="40+ g/day"

esoph_cancer$tobgrp [esoph_cancer$tobacco <3] ="<10 g/day"
esoph_cancer$tobgrp [esoph_cancer$tobacco >=3 & esoph_cancer$tobacco <8] =">=10 g/day"

#question 1: model with only alcohol
model1 <- glm(casecont ~ alcgrp, data = esoph_cancer, family = binomial)
summary(model1)
exp(coefficients(model1))
exp(confint.default(model1))

#question 2: model with alcohol and adjusting for age
model2 <- glm(casecont ~ alcgrp + agegrp, data = esoph_cancer, family = binomial)
summary(model2)
exp(coefficients(model2))
exp(confint.default(model2))

#question 3: model with alcohol and adjusting for tobacco
model3 <- glm(casecont ~ alcgrp + tobgrp, data = esoph_cancer, family = binomial)
summary(model3)
exp(coefficients(model3))
exp(confint.default(model3))

#question 4: model with alcohol, tobacco, age, and the cross-product term between tobacco and alcohol
model4 <- glm(casecont ~ alcgrp + tobgrp + agegrp + tobgrp*alcgrp , data = esoph_cancer, family = binomial)
summary(model4)
exp(coefficients(model4))
exp(confint.default(model4))

##to create a new variable with interaction: using dummy variables 

esoph_cancer$newvar1 <- esoph_cancer$tobgrp
esoph_cancer$newvar1[esoph_cancer$alcgrp == "0-39 g/day" & esoph_cancer$tobgrp == "<10 g/day"] <- "1"
esoph_cancer$newvar1[esoph_cancer$alcgrp == "0-39 g/day" & esoph_cancer$tobgrp == ">=10 g/day"] <- "0"
esoph_cancer$newvar1[esoph_cancer$alcgrp == "40+ g/day" & esoph_cancer$tobgrp == "<10 g/day"] <- "0"
esoph_cancer$newvar1[esoph_cancer$alcgrp == "40+ g/day" & esoph_cancer$tobgrp == ">=10 g/day"] <- "0"

esoph_cancer$newvar2 <- esoph_cancer$tobgrp
esoph_cancer$newvar2[esoph_cancer$alcgrp == "0-39 g/day" & esoph_cancer$tobgrp == "<10 g/day"] <- "0"
esoph_cancer$newvar2[esoph_cancer$alcgrp == "0-39 g/day" & esoph_cancer$tobgrp == ">=10 g/day"] <- "1"
esoph_cancer$newvar2[esoph_cancer$alcgrp == "40+ g/day" & esoph_cancer$tobgrp == "<10 g/day"] <- "0"
esoph_cancer$newvar2[esoph_cancer$alcgrp == "40+ g/day" & esoph_cancer$tobgrp == ">=10 g/day"] <- "0"

esoph_cancer$newvar3 <- esoph_cancer$tobgrp
esoph_cancer$newvar3[esoph_cancer$alcgrp == "0-39 g/day" & esoph_cancer$tobgrp == "<10 g/day"] <- "0"
esoph_cancer$newvar3[esoph_cancer$alcgrp == "0-39 g/day" & esoph_cancer$tobgrp == ">=10 g/day"] <- "0"
esoph_cancer$newvar3[esoph_cancer$alcgrp == "40+ g/day" & esoph_cancer$tobgrp == "<10 g/day"] <- "1"
esoph_cancer$newvar3[esoph_cancer$alcgrp == "40+ g/day" & esoph_cancer$tobgrp == ">=10 g/day"] <- "0"

esoph_cancer$newvar4 <- esoph_cancer$tobgrp
esoph_cancer$newvar4[esoph_cancer$alcgrp == "0-39 g/day" & esoph_cancer$tobgrp == "<10 g/day"] <- "0"
esoph_cancer$newvar4[esoph_cancer$alcgrp == "0-39 g/day" & esoph_cancer$tobgrp == ">=10 g/day"] <- "0"
esoph_cancer$newvar4[esoph_cancer$alcgrp == "40+ g/day" & esoph_cancer$tobgrp == "<10 g/day"] <- "0"
esoph_cancer$newvar4[esoph_cancer$alcgrp == "40+ g/day" & esoph_cancer$tobgrp == ">=10 g/day"] <- "1"

#An alternative way of creating the dummy variable:
# alctob1: light drinkers, heaver smokers
esoph_ca$alctob1 <- ifelse(is.na(esoph_ca$alcgrp) | is.na(esoph_ca$tobgrp), NA,
                           ifelse(esoph_ca$alcgrp == "0-39 g/day" & esoph_ca$tobgrp == ">=10 g/day", 1,0))
esoph_ca$alctob1 <- factor(esoph_ca$alctob1)
table(esoph_ca$alctob1, useNA = "always")

# alctob2: heavy drinkers, light smokers
esoph_ca$alctob2<- ifelse(is.na(esoph_ca$alcgrp) | is.na(esoph_ca$tobgrp), NA,
                          ifelse(esoph_ca$alcgrp == "40+ g/day" & esoph_ca$tobgrp == "<10 g/day", 1, 0))
esoph_ca$alctob2 <- factor(esoph_ca$alctob2)
table(esoph_ca$alctob2, useNA = "always")

# alctob3: heavy drinkers, heavy smokers
esoph_ca$alctob3<- ifelse(is.na(esoph_ca$alcgrp) | is.na(esoph_ca$tobgrp), NA,
                          ifelse(esoph_ca$alcgrp == "40+ g/day" & esoph_ca$tobgrp == ">=10 g/day", 1, 0))
esoph_ca$alctob3 <- factor(esoph_ca$alctob3)
table(esoph_ca$alctob3, useNA = "always")

## alctob4: light drinkers, light smokers
esoph_ca$alctob4 <- ifelse(is.na(esoph_ca$alcgrp) | is.na(esoph_ca$tobgrp), NA,
                           ifelse(esoph_ca$alcgrp == "0-39 g/day" & esoph_ca$tobgrp == "<10 g/day", 1,0))
esoph_ca$alctob4 <- factor(esoph_ca$alctob4)
table(esoph_ca$alctob4, useNA = "always")

#Multivariable model using the newvar* variables
model5 <- glm(casecont ~ agegrp + newvar2 + newvar3 + newvar4, data = esoph_cancer, family = binomial)
summary(model5)
exp(cbind(OR = coef(model5), confint.default(model5)))
vcov(model5)

#RERI calculation
epi.interaction(model5, param="dummy", coef=c(3, 4, 5), conf.level =0.95)

##here's a way to hand-check the output from the epi.interaction code:
model5$coefficients

#Store the OR values (order of variables in coefficient list may change if you ordered variables differently in your model)
OR_01 <- exp(model5$coefficients[3])  #OR where exposure =0 and z=1 (coef of newvar2, listed second)
OR_10 <- exp(model5$coefficients[4])  #OR where exposure =1 and z=0 (coef of newvar3, listed third)
OR_11 <- exp(model5$coefficients[5])  #the joint OR of exposure=1 and z=1 (coef of newvar5, listed fourth)

#another way to do this is just copying and pasting values
OR01 <- 4.17
OR10 <- 10.30
OR11 <- 16.74

# Calculate RERI
RERI = OR_11 - OR_10 - OR_01 + 1
RERI

#another calculation of RERI, just using the hard-coded numbers. Same result.
RERI2 <- OR11 - OR10 - OR01 + 1
RERI2
