# Libraries:
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(lubridate)
library(survminer)
library(Rcpp)
library(coxphf)
library(Rfit)
library(rstatix)
library(epitools)
library(data.table)
library(haven)
# Read database:

hivhepc <- read_dta("hivhepc.dta")
                   
#question 1: 
View(hivhepc)
summary(hivhepc)
str(hivhepc)

# questions 2 & 3: (first, convert variables as factors)
hivhepc$hepc=as.factor(hivhepc$hepc)
table(hivhepc$hepc)

hivhepc$msm=as.factor(hivhepc$msm)
table(hivhepc$msm)

hivhepc$ivdruguse=as.factor(hivhepc$ivdruguse)
table(hivhepc$ivdruguse)

table(hivhepc$msm, hivhepc$ivdruguse)

#Part II :
hivhepc$time_death <- hivhepc$death_date - hivhepc$cohort_date
hivhepc$time_death_yr <- hivhepc$time_death/365.25

table(is.na(hivhepc$time_death))
hivhepc$death_datepresent <- ifelse(!is.na(hivhepc$death_date), 1,0)
hivhepc$death_datepresent <- factor(hivhepc$death_datepresent)

hivhepc$timecensored <- fifelse(hivhepc$death_datepresent == "1", hivhepc$death_date, hivhepc$last_alive_date)
hivhepc$timecensoreddiff <- hivhepc$timecensored - hivhepc$cohort_date

hivhepc$timecensored_yr <- hivhepc$timecensoreddiff/365.25
View(hivhepc)

Fit <- survfit(Surv(timecensored_yr, death) ~ 1, data = hivhepc)
print(Fit)
autoplot(Fit)

ggsurvplot(Fit, data=hivhepc, risk.table=T, surv.median.line = "hv", legend.labs = c("censored"), pval = TRUE)
hivhepc$os_months = interval(hivhepc$cohort_date,hivhepc$last_alive_date) %/% months(1)

# Survival:
Surv(hivhepc$timecensored_yr, hivhepc$death)[1:10] #for the first 10 observations

# K-M analysis:
km_fit <- survfit(Surv(timecensored_yr, death) ~ 1, data=hivhepc)
summary(km_fit, times = c(1,30,60,90,120*(1:10))) #60 months = 5-years OS
print(km_fit) #median survival
autoplot(km_fit)

## by hep C status:
km_hepc_fit <- survfit(Surv(timecensored_yr, death) ~ hepc, data=hivhepc)
print(km_hepc_fit) #median survival by Hep C status
autoplot(km_hepc_fit)

## log rank:
surv_diff_hepc <- survdiff(Surv(timecensored_yr, death) ~ hepc, data = hivhepc)
surv_diff_hepc #p=0.03

## Wilcoxon test:
ggsurvplot(km_hepc_fit, data = hivhepc, pval = TRUE, pval.method = TRUE,
           log.rank.weights = "n", pval.method.coord = c(5, 0.1),
           pval.method.size = 3)
#wilcoxon test is giving more weight to those at the beginning of the follow-up
#wilcoxon test -> no significant

## Add database:
hivhepc = read_dta("hivhepc.dta")

## Question 2: Cox-regression:

# Get months and years:

hivhepc$death_datepresent <- ifelse(!is.na(hivhepc$death_date), 1,0)
hivhepc$death_datepresent <- factor(hivhepc$death_datepresent)

hivhepc$timecensored <- fifelse(hivhepc$death_datepresent == "1", hivhepc$death_date, hivhepc$last_alive_date)
hivhepc$timecensoreddiff <- hivhepc$timecensored - hivhepc$cohort_date
hivhepc$timecensored_yr <- hivhepc$timecensoreddiff/365.25

# Create a survival object:
hivhepc$survobj = with(hivhepc, Surv(timecensored_yr, death))

# Hazard Ratio:
res.cox1 = coxph(survobj ~ hepc, data =  hivhepc)
summary(res.cox1)

## Question 3: Risk Ratio:
rrtable = table(hivhepc$hepc, hivhepc$death)
riskratio.wald(rrtable)
# The risk ratio of 1.23 is closer to the null (1.0 or no effect) as compared to the hazard ratio of 1.38

## Question 5:
# person-years:
table (hivhepc$hepc, hivhepc$death)

df = data.frame(hivhepc)
df %>% group_by(hivhepc$hepc) %>%
  summarise( 
    t1t = sum(ifelse(hivhepc$hepc == "1", hivhepc$timecensored_yr, 0)),
    t1u = sum(ifelse(hivhepc$hepc == "0", hivhepc$timecensored_yr, 0)) 
  )

#pyears(Surv(stop/365.25, deaths) ~ hepc, data=hivhepc, subset= hepc==1, scale=365.25 )
pyears(timecensored_yr ~ hepc , data=hivhepc, subset= hepc==1, scale=365.25)

## PART IV
##Question 1: Hazard Ratio

res.cox2 = coxph(survobj ~ ivdruguse, data =  hivhepc)
summary(res.cox2)

##Question 3: 
res.cox3 = coxph(survobj ~ ivdruguse, data =  hivhepc)
summary(res.cox3)

res.cox4 = coxph(survobj ~ hepc + ivdruguse, data =  hivhepc)
summary(res.cox4)

##Question 4: 
((0.523026 - 0.4739054)/0.523026)

##Question 5:
res.cox4 = coxph(survobj ~ ivdruguse + hepc + msm + msm*ivdruguse, data =  hivhepc)
summary(res.cox4)

##Question 6:
exp(coef(res.cox4))
exp(0.18040+0.33183+0.63278)
(3.142473/1.393516) 

## PART V

## Graphical method:
prophr = survfit(Surv(timecensored_yr, death) ~ hiv_diag_1996, data = hivhepc)
ggsurvplot(prophr, fun = "cloglog")

## Statistical test:
#the variable timecensored_yr is a difftime variable we need to change its format to num
hivhepc$timecensored_yr <- as.numeric(hivhepc$timecensored_yr)

hivhepc$logpyrs = logb(hivhepc$timecensored_yr)

hivhepc$logCP = (hivhepc$logpyrs*hivhepc$hiv_diag_1996)

## Cox regression
res.cox5 = coxph(Surv(timecensored_yr, death) ~ ivdruguse + msm + hiv_diag_1996 + logCP, data =  nd2)
summary(res.cox5)

#A violations of proportional hazards assumption can be resolved by: 
# - adding covariate*time interaction
# - stratification
