library(haven)
crp <- read_dta("CRP_angina.dta")


# Look at the data
library(tidyverse)
str(crp)
View(crp)

# Question 4 - ratio of cases to controls:
crp$casecont = factor(crp$casecont, levels= c(1,2), labels=c("cases","controls"))
library(janitor)
crp %>% tabyl(casecont)

# Question 5 - evaluating if matching worked:

# evaluating age by case status
crp %>%
  group_by(casecont) %>%
  summarise(mean=mean(age_yr))
# one-way anova test
one.way = aov(age_yr ~ casecont, data=crp )
summary(one.way)

t.test(age_yr ~ casecont, data = crp)

# evaluating smoking by case status
crp %>% tabyl(casecont, smoke) %>%
  adorn_percentages(denominator = "col") %>%  #convert to proportions
  adorn_pct_formatting() %>%                  #convert to percents
  adorn_ns(position="front") %>%
  adorn_title(row_name = "casecont", col_name = "smoke")
# chi-squared test
chisq.test(crp$casecont, crp$smoke)


# Question 6 - conditional logistic regression for continuous hsCRP in association with angina

# There are at least two ways to run a conditional logistic regression in R:

# First way:
library(survival)
model_q6 = clogit(angina ~ hscrp + strata(group_id), data=crp)
model_q6
exp(confint.default(model_q6))
# this method allows you to extract the log-likelihood with the logLik function
logLik(model_q6)

# Second way:
library(Epi)
model_q6_v2 = clogistic(angina ~ hscrp, strata=group_id, data=crp)
model_q6_v2
exp(confint.default(model_q6_v2))
# This way does not allow you to use the logLik function to get the log-likelihood 
# Recommend using the first way (survival package) to allow calculation of likelihood ratio test


# Question 7 - conditional logistic regression for dichotomous hsCRP in association with angina

# creating dichotomous crp_grp variable
crp <- mutate(crp, crp_grp = case_when(
  hscrp <= 1 ~ "0",
  hscrp > 1 ~ "1"
))

# conditional logistic regression model
model_q7 = clogit(angina ~ crp_grp + strata(group_id), data=crp)
model_q7
exp(confint.default(model_q7))


# Question 8 - assessing gender and alcohol use as potential confounders

# assessing gender
model_q8_gend = clogit(angina ~ crp_grp + gender + strata(group_id), data=crp)
model_q8_gend
exp(confint.default(model_q8_gend))

# assessing alcohol use
model_q8_alc = clogit(angina ~ crp_grp + alc_high + strata(group_id), data=crp)
model_q8_alc
exp(confint.default(model_q8_alc))


# Quesion 9 - evaluate alcohol variable more closely
crp %>%
tabyl(alc_high)
# a lot of missing data (43.6% missing)!


# Question 10 - assessing if gender independent predictor of angina

# unadjusted model
model_q10_unadj = clogit(angina ~ gender + strata(group_id), data=crp)
model_q10_unadj
exp(confint.default(model_q10_unadj))

# adjusted for crp_grp
model_q10_adj = clogit(angina ~ gender + crp_grp + strata(group_id), data=crp)
model_q10_adj
exp(confint.default(model_q10_adj))

# gender associated with angina in both unadjusted and adjusted models


# Question 11 - assessing obesity as a potential effect modifier 

# creating obesity variable
crp$bmi = (crp$wt_kg/(crp$ht_cm/100)^2)

crp <- mutate(crp, bmi_30 = case_when(
  bmi <= 30 ~ "0",
  bmi > 30 ~ "1"
))

# checking new dichotomous variable 
crp %>%
  group_by(bmi_30) %>%
  summarise(n=n(), min=min(bmi), max=max(bmi)) 


# confirm obesity associated with angina, controlling for crp_grp
model_q11 = clogit(angina ~ bmi_30 + crp_grp + strata(group_id), data=crp)
model_q11
exp(confint.default(model_q11))


# testing for multiplicative interaction with cross-product term
model_q11_crossprod = clogit(angina ~ bmi_30*crp_grp + strata(group_id), data=crp)
model_q11_crossprod
exp(confint.default(model_q11_crossprod))

# testing for multiplicative interaction with log-likelihood ratio test
library(lmtest)
lrtest(model_q11, model_q11_crossprod)


# testing for additive interaction with indicator/dummy variables

# creating dummy variables
crp$crpbmi1 <- crp$crp_grp
crp$crpbmi1[crp$bmi_30 == "0" & crp$crp_grp == "0"] <- "0"
crp$crpbmi1[crp$bmi_30 == "1" & crp$crp_grp == "0"] <- "1"
crp$crpbmi1[crp$bmi_30 == "0" & crp$crp_grp == "1"] <- "0"
crp$crpbmi1[crp$bmi_30 == "1" & crp$crp_grp == "1"] <- "0"
crp$crpbmi1[is.na(crp$bmi_30) | is.na(crp$crp_grp)] <- NA

crp$crpbmi2 <- crp$crp_grp
crp$crpbmi2[crp$bmi_30 == "0" & crp$crp_grp == "0"] <- "0"
crp$crpbmi2[crp$bmi_30 == "1" & crp$crp_grp == "0"] <- "0"
crp$crpbmi2[crp$bmi_30 == "0" & crp$crp_grp == "1"] <- "1"
crp$crpbmi2[crp$bmi_30 == "1" & crp$crp_grp == "1"] <- "0"
crp$crpbmi2[is.na(crp$bmi_30) | is.na(crp$crp_grp)] <- NA

crp$crpbmi3 <- crp$crp_grp
crp$crpbmi3[crp$bmi_30 == "0" & crp$crp_grp == "0"] <- "0"
crp$crpbmi3[crp$bmi_30 == "1" & crp$crp_grp == "0"] <- "0"
crp$crpbmi3[crp$bmi_30 == "0" & crp$crp_grp == "1"] <- "0"
crp$crpbmi3[crp$bmi_30 == "1" & crp$crp_grp == "1"] <- "1"
crp$crpbmi3[is.na(crp$bmi_30) | is.na(crp$crp_grp)] <- NA

model_q11_dummy = clogit(angina ~ crpbmi1 + crpbmi2 + crpbmi3 + strata(group_id), data=crp)
model_q11_dummy
exp(confint.default(model_q11_dummy))


# Question 12 - run unconditional logistic regression model adjusted for matching variables
model_q12 <- glm(angina ~ crp_grp + age_yr + smoke, data = crp, family = binomial)
summary(model_q12)
exp(coefficients(model_q12))
exp(confint.default(model_q12))

