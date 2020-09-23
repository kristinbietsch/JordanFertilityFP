# Jordan Feritlity and Family Planning
# Kristin Bietsch
# 09/18/20



# Knowledge of EC


library(survey)
library(tibble)
library(dplyr)
library(tidyr)
library(haven)
library(xlsx)
library(stringr)
library(questionr)

library(jtools)
library(huxtable)
library(broom)
library(ggplot2)
setwd("C:/Users/KristinBietsch/files/DHS Data/Jordan")

women17 <- read_dta("JOIR73FL.DTA")
women12 <- read_dta("JOIR6CFL.DTA")
nationality2012  <- read_dta("nationality2012.DTA")

women12 <- left_join(women12, nationality2012, by=c("v000", "v001", "v002", "v003"))

men <- read_dta("JOMR71FL.DTA")

# just comparing jordanians to syrians
women17 <- women17 %>% mutate(syrian=case_when(s123a==3 ~ 1,
                                               s123a==1 ~ 0),
                              cpr=case_when(v312!=0 ~ 1, 
                                            v312==0 ~ 0),
                              knows_ec=case_when(v304_16==1 ~ 1,
                                                 v304_16==0 ~ 0),
                              year=1)
women12 <- women12 %>% mutate(syrian=case_when(sh07==3 ~ 1,
                                               sh07==1 ~ 0),
                              cpr=case_when(v312!=0 ~ 1, 
                                            v312==0 ~ 0),
                              knows_ec=case_when(v304_16==1 ~ 1,
                                                 v304_16==0 ~ 0),
                              year=0)

men <- men %>% mutate( knows_ec=case_when(mv304_16==1 ~ 1,
                                                 mv304_16==0 ~ 0))


women17$sampleweights <- women17$v005/1000000
women12$sampleweights <- women12$v005/1000000

men$sampleweights <- men$mv005/1000000

design.women17 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women17, nest=TRUE)
options(survey.lonely.psu="adjust")

design.women12 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women12, nest=TRUE)
options(survey.lonely.psu="adjust")

design.men <- svydesign(ids=~mv021, strata=~mv025, weights=~sampleweights, data=men, nest=TRUE)
options(survey.lonely.psu="adjust")


# JOINT
joint_women <- bind_rows(women17, women12)
joint_women <- joint_women %>% mutate(v001_joint=case_when(year==1 ~ v001,
                                                           year==0 ~ v001 + 1000),
                                      v023_joint=case_when(year==1 ~ v023,
                                                           year==0 ~ v023 + 100))

sel <- select(joint_women, v001, v023, v001_joint, v023_joint, year)
design.womenjoint <- svydesign(ids=~v001_joint, strata=~v023_joint, weights=~sampleweights, data=joint_women, nest=TRUE)
options(survey.lonely.psu="adjust")

###################################3
svyby( ~knows_ec , ~ syrian ,  subset(design.women17, v502==1), svymean)
summ(svyglm( knows_ec ~  + syrian , subset(design.women17, v502==1), family=quasibinomial() ), confint=TRUE)

svyby( ~knows_ec , ~ syrian ,  subset(design.women12, v502==1), svymean)
summ(svyglm( knows_ec ~  + syrian , subset(design.women12, v502==1), family=quasibinomial() ), confint=TRUE)

svyby( ~knows_ec , ~ year ,  subset(design.womenjoint, v502==1), svymean)
summ(svyglm( knows_ec ~  + year , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)


svyby( ~knows_ec , ~ v013 ,  subset(design.women17, v502==1), svymean)
svyby( ~knows_ec , ~ v013 ,  subset(design.women12, v502==1), svymean)

svyby( ~knows_ec , ~ mv502 ,  design.men, svymean)
