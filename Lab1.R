#Libraries:
library(questionr)
library(haven)
library(epitools)
library(DescTools)

esoph_cancer <- read_dta("esoph_cancer.dta")
View(esoph_cancer)


#Categorize variables:
esoph_cancer$agegrp [esoph_cancer$age>= 21 & esoph_cancer$age <= 59] ="<60 years"
esoph_cancer$agegrp [esoph_cancer$age > 59 & esoph_cancer$age <= 91] =">=60 years"

esoph_cancer$alcgrp [esoph_cancer$alcohol <= 39] ="0-39 g/day"
esoph_cancer$alcgrp [esoph_cancer$alcohol > 39] ="40+ g/day"

esoph_cancer$tobgrp [esoph_cancer$tobacco <3] ="<10 g/day"
esoph_cancer$tobgrp [esoph_cancer$tobacco >=3 & esoph_cancer$tobacco <8] =">=10 g/day"

#Rename cases & controls:
esoph_cancer$casecont = factor(esoph_cancer$casecont, levels = c(0,1), labels = c("controls", "cases"))

#Factors
#esoph_cancer$tobgrp <- as.factor(esoph_cancer$tobgrp)
#esoph_cancer$alcgrp <- as.factor(esoph_cancer$alcgrp)
#esoph_cancer$agegrp <- as.factor(esoph_cancer$agegrp)

#Table 1 (frequencies & percentages):
table_alcgrp=table(esoph_cancer$alcgrp, esoph_cancer$casecont)
table_agegrp=table(esoph_cancer$agegrp, esoph_cancer$casecont)
table_tobgrp=table(esoph_cancer$tobgrp, esoph_cancer$casecont)
prop.table(table(esoph_cancer$alcgrp, esoph_cancer$casecont), margin = 2)
prop.table(table(esoph_cancer$agegrp, esoph_cancer$casecont), margin = 2)
prop.table(table(esoph_cancer$tobgrp, esoph_cancer$casecont), margin = 2)

#ORs
chisq.test(esoph_cancer$agegrp, esoph_cancer$casecont)
odds.ratio(table_agegrp)

chisq.test(esoph_cancer$alcgrp, esoph_cancer$casecont)
odds.ratio(table_alcgrp)

chisq.test(esoph_cancer$tobgrp, esoph_cancer$casecont)
odds.ratio(table_tobgrp)

#Test association between variables:
chisq.test(esoph_cancer$alcgrp, esoph_cancer$agegrp) #not significant
chisq.test(esoph_cancer$alcgrp, esoph_cancer$tobgrp) #significant, so perform a glm

#GLM to test tobacco:
model1=glm(casecont ~ alcgrp + tobgrp + tobgrp*alcgrp, family = "binomial", data=esoph_cancer)

#ORs in strata:
##Age separately:
mhor(esoph_cancer, alcgrp, casecont, agegrp)

##Tobacco separately:
mhor(esoph_cancer, alcgrp, casecont, tobgrp)

#Age and Tobacco jointly:

esoph_cancer$agetobac [esoph_cancer$agegrp==">=60 years" & esoph_cancer$tobgrp==">=10 g/day"] =0
esoph_cancer$agetobac [esoph_cancer$agegrp=="<60 years" & esoph_cancer$tobgrp==">=10 g/day"] =1
esoph_cancer$agetobac [esoph_cancer$agegrp==">=60 years" & esoph_cancer$tobgrp=="<10 g/day"] =2
esoph_cancer$agetobac [esoph_cancer$agegrp=="<60 years" & esoph_cancer$tobgrp=="<10 g/day"] =3

mhor(esoph_cancer, alcgrp, casecont, agetobac)
