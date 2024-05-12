library(haven)
library(survival)
library(tidyverse)
library(skimr)
library(janitor)
library(survminer)

stanford <- read_dta("stanford.dta")

names(stanford)

# Q1 - how many total obs in the dataset
str(stanford)

# Q2 - how many patients received heart transplant
tabyl(stanford$transplant)

# Q3 - how many patients received heart transplant within 30 days
stanford <- mutate(stanford, wait30 = case_when(
  transplant == 1  & wait <= 30 ~ 1,
  transplant == 1  & wait > 30 ~ 0))
tabyl(stanford$wait30)

#ADDITIONAL QUESTIONS that were not in the lab but for your information:
#What was the mean age of patients who received a heart transplant? 
#What was the mean age of patient who did not receive a heart transplant?
# Q4 - mean age of patients by transplant status
stanford %>%
  group_by(transplant) %>%
  summarise(mean(age))

# Q4 - mean wait time of patients with transplant
stanford %>%
  group_by(transplant) %>%
  summarise(mean(wait))
stanford %>%
  group_by(transplant) %>%
  summarise(min(wait))
stanford %>%
  group_by(transplant) %>%
  summarise(max(wait))


#####################################################################################################################
#####################################################################################################################

## Fixed variable 

# Q6 - KM curve by Transplant
km = Surv(time = stanford$stime, event=stanford$died)
km_curve_transplant = survfit(formula=km ~ stanford$transplant, data=stanford, type="kaplan-meier", conf.type="log")
# median survival time
km_curve_transplant
# plot
ggsurvplot(km_curve_transplant, conf.int='True')

## log rank:
ggsurvplot(km_curve_transplant, conf.int='False', pval = TRUE, pval.method = TRUE, surv.median.line="hv")
logrank_pval <- survdiff(Surv(stime, died) ~ transplant, data = stanford, rho=0)
logrank_pval

## Wilcoxon:
ggsurvplot(km_curve_transplant, conf.int='False', pval = TRUE, pval.method = TRUE, log.rank.weights="n", surv.median.line="hv")
wilcoxon_pval <- survdiff(Surv(stime, died) ~ transplant, data = stanford, rho=1)
wilcoxon_pval 

## Fixed variable cox regression model

# Q8 - Bivariate model
cox_fixed_unadj = coxph(Surv(stime, died) ~ transplant, data=stanford) 
summary(cox_fixed_unadj)

# Q9 - Adjusted model
cox_fixed_adj = coxph(Surv(stime, died) ~ transplant + age + surgery + year, data=stanford) 
summary(cox_fixed_adj)

#####################################################################################################################
#####################################################################################################################

## Time-varying variable 

# Creating new died variable for each time segment, split by wait
stanford$died1 = stanford$died
stanford$died1 [stanford$wait == 0 & stanford$died ==0 ] = "0"
stanford$died1 [stanford$wait == 0 & stanford$died ==1 ] = "1"
stanford$died1 [stanford$wait > 0 ] = "0"

stanford$died2 = stanford$died
stanford$died2 [stanford$wait > 0 & stanford$died == 0 ] = "0"
stanford$died2 [stanford$wait > 0 & stanford$died == 1 ] = "1"

names(stanford)
View(stanford)
# Splitting up follow-up time with tmerge command
newstanford = tmerge(data1=stanford[, 1:11], data2=stanford, id=id, tstop=stime)
newstanford = tmerge(newstanford, stanford, id=id, split=event(wait))

View(newstanford)

#Creating post-transplant variable
newstanford$postran [newstanford$transplant == 0] = 0
newstanford$postran [newstanford$transplant == 1 & newstanford$tstart == 0] = 0
newstanford$postran [newstanford$transplant == 1 & newstanford$tstart > 0] = 1


#Creating final died variable for long dataset
newstanford$died_final [newstanford$postran == 0 & newstanford$died1 == 0 ] = 0
newstanford$died_final [newstanford$postran == 0 & newstanford$died1 == 1 ] = 1
newstanford$died_final [newstanford$postran == 1 & newstanford$tstart == 0 ] = 0

newstanford$died_final [newstanford$postran == 1 & newstanford$tstart > 0 & newstanford$died2 == 0 ] = 0
newstanford$died_final [newstanford$postran == 1 & newstanford$tstart > 0 & newstanford$died2 == 1 ] = 1

newstanford$died_final = as.numeric(newstanford$died_final)

# Q14 - After splitting data, how many obs?
str(newstanford)

# Q15 - KM curve by Transplant
km_tvar = survfit(Surv(tstart, tstop, died_final) ~  postran, data = newstanford, id = id)
ggsurvplot(km_tvar, conf.int='True')
ggsurvplot(km_tvar, conf.int='False')

# median survival time
km_tvar

## Q17 - Time-varying variable unadjusted cox regression model
cox_tvar_unadj = coxph(Surv(tstart, tstop, died_final) ~ postran, data =newstanford, cluster = id, ties="breslow")
summary(cox_tvar_unadj)

## Q18 - Time-varying variable adjusted cox regression model
cox_tvar_adj = coxph(Surv(tstart, tstop, died_final) ~ postran + age + surgery + year, data =newstanford, cluster = id, ties="breslow")
summary(cox_tvar_adj)
