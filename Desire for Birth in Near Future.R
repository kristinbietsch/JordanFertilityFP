# Jordan Feritlity and Family Planning
# Kristin Bietsch
# 09/18/20



#Desire for Birth in Near Future


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


women17 <- women17 %>% mutate(wants_year=case_when(v604==0 ~ 1, v604!=0 ~ 0, v602!=1 ~ 0),
                              time_marriage= (v008-v509)/12,
                              marriage_group=case_when(time_marriage<2 ~ 1, 
                                                       time_marriage>=2 & time_marriage<5 ~ 2,
                                                       time_marriage>=5 & time_marriage<10 ~ 3,
                                                       time_marriage>=10 & time_marriage<15 ~ 4,
                                                       time_marriage>=15 ~ 5),
                              mar2=case_when(marriage_group==1 ~ 1,
                                             marriage_group!=1 ~ 0),
                              mar24=case_when(marriage_group==2 ~ 1,
                                              marriage_group!=2 ~ 0),
                              undecided=case_when(v602==2 ~ 1,
                                                  v602!=2 ~ 0),
                              wants_another=case_when(v602==1 ~ 1,
                                                      v602!=1 ~ 0),
                              unmet= case_when(v626a==1 | v626==2 ~ 1,
                                               v626a!=1 & v626a!=2 ~ 0),
                              met_ideal=case_when(v201>=v613 ~ 1,
                                                  v201<v613 ~ 0),
                              g45=case_when(v013==7 ~ 1,
                                            v013!=7 ~ 0),
                              live_husband=case_when(v504==1 ~ 1,
                                                     v504==2 ~ 0),
                              sex_month=case_when(v529==0 ~ 1, v529!=0 ~ 0, is.na(v529) ~ 0),
                              inf_meno=case_when(v626a==9 ~ 1, v626a!=9 ~ 0, is.na(v626a) ~ 0),
                              ster= case_when(v201==0 ~ 1, v201!=0 ~ 0),
                              nulliparous= case_when(v201==0 ~ 1, v201!=0 ~ 0),
                              parity1=case_when(v201==1 ~ 1, v201!=1 ~ 0),
                              parity2=case_when(v201==2 ~ 1, v201!=2 ~ 0),
                              wants_children=case_when(v613>0 ~ 1, v613==0 ~ 0),
                              cpr=case_when(v313!=0 ~ 1, v313==0 ~ 0),
                              condom=case_when(v312==5 ~ 1, v312!=5 ~ 0),
                              pa=case_when(v312==8 ~ 1, v312!=8 ~ 0),
                              withdrawal=case_when(v312==9 ~ 1, v312!=9 ~ 0),
                              year=1)





women12 <- women12 %>% mutate(wants_year=case_when(v604==0 ~ 1, v604!=0 ~ 0, v602!=1 ~ 0),
                              time_marriage= (v008-v509)/12,
                              marriage_group=case_when(time_marriage<2 ~ 1, 
                                                       time_marriage>=2 & time_marriage<5 ~ 2,
                                                       time_marriage>=5 & time_marriage<10 ~ 3,
                                                       time_marriage>=10 & time_marriage<15 ~ 4,
                                                       time_marriage>=15 ~ 5),
                              mar2=case_when(marriage_group==1 ~ 1,
                                             marriage_group!=1 ~ 0),
                              mar24=case_when(marriage_group==2 ~ 1,
                                              marriage_group!=2 ~ 0),
                              undecided=case_when(v602==2 ~ 1,
                                                  v602!=2 ~ 0),
                              wants_another=case_when(v602==1 ~ 1,
                                                      v602!=1 ~ 0),
                              unmet= case_when(v626a==1 | v626==2 ~ 1,
                                               v626a!=1 & v626a!=2 ~ 0),
                              met_ideal=case_when(v201>=v613 ~ 1,
                                                  v201<v613 ~ 0),
                              g45=case_when(v013==7 ~ 1,
                                            v013!=7 ~ 0),
                              live_husband=case_when(v504==1 ~ 1,
                                                     v504==2 ~ 0),
                              sex_month=case_when(v529==0 ~ 1, v529!=0 ~ 0, is.na(v529) ~ 0),
                              inf_meno=case_when(v626a==9 ~ 1, v626a!=9 ~ 0, is.na(v626a) ~ 0),
                              ster= case_when(v201==0 ~ 1, v201!=0 ~ 0),
                              nulliparous= case_when(v201==0 ~ 1, v201!=0 ~ 0),
                              parity1=case_when(v201==1 ~ 1, v201!=1 ~ 0),
                              parity2=case_when(v201==2 ~ 1, v201!=2 ~ 0),
                              wants_children=case_when(v613>0 ~ 1, v613==0 ~ 0),
                              cpr=case_when(v313!=0 ~ 1, v313==0 ~ 0),
                              condom=case_when(v312==5 ~ 1, v312!=5 ~ 0),
                              pa=case_when(v312==8 ~ 1, v312!=8 ~ 0),
                              withdrawal=case_when(v312==9 ~ 1, v312!=9 ~ 0),
                              year=0)

women17$sampleweights <- women17$v005/1000000
women12$sampleweights <- women12$v005/1000000

design.women17 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women17, nest=TRUE)
options(survey.lonely.psu="adjust")

design.women12 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women12, nest=TRUE)
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


svyby( ~wants_year , ~ as.factor(v604), design.women17 ,  svymean, na.rm=T)
svyby( ~wants_year , ~ as.factor(v602), design.women17 ,  svymean, na.rm=T)
svyby( ~wants_year , ~ as.factor(v502), design.women17 ,  svymean, na.rm=T)

svymean(~wants_year,  subset(design.women17, v502==1), na.rm=T)

# wants another birth
svymean(~as.factor(v602),  subset(design.women17, v502==1))

svyby( ~wants_year , ~ as.factor(marriage_group), subset(design.women17, v502==1) ,  svymean, na.rm=T)

svymean(~wants_year,  subset(design.women12, v502==1), na.rm=T)
svymean(~as.factor(v602),  subset(design.women12, v502==1))
svyby( ~wants_year , ~ as.factor(marriage_group), subset(design.women12, v502==1) ,  svymean, na.rm=T)



svyby( ~v213 , ~ year ,  subset(design.womenjoint, v502==1), svymean)
summ(svyglm( v213 ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( wants_year ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)

summ(svyglm( wants_year ~  + year  , subset(design.womenjoint, v502==1 & marriage_group==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( wants_year ~  + year  , subset(design.womenjoint, v502==1 & marriage_group==2), family=quasibinomial() ), confint=TRUE)
summ(svyglm( wants_year ~  + year  , subset(design.womenjoint, v502==1 & marriage_group==3), family=quasibinomial() ), confint=TRUE)
summ(svyglm( wants_year ~  + year  , subset(design.womenjoint, v502==1 & marriage_group==4), family=quasibinomial() ), confint=TRUE)
summ(svyglm( wants_year ~  + year  , subset(design.womenjoint, v502==1 & marriage_group==5), family=quasibinomial() ), confint=TRUE)


summ(svyglm( undecided ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( wants_another ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)

summ(svyglm( unmet ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( met_ideal ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)

summ(svyglm( mar2 ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( mar24 ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)


summ(svyglm( condom ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( pa ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( withdrawal ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( v012 ~  + year  , subset(design.womenjoint, v502==1) ), confint=TRUE)


summ(svyglm( g45 ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( mar2 ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( mar24 ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( live_husband ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( sex_month ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( sex_month ~  + year  , subset(design.womenjoint, v502==1 & live_husband==1), family=quasibinomial() ), confint=TRUE)


summ(svyglm( inf_meno ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( inf_meno ~  + year  , subset(design.womenjoint, v502==1 & v013==6), family=quasibinomial() ), confint=TRUE)
summ(svyglm( inf_meno ~  + year  , subset(design.womenjoint, v502==1 & v013==4), family=quasibinomial() ), confint=TRUE)
summ(svyglm( inf_meno ~  + year  , subset(design.womenjoint, v013==7), family=quasibinomial() ), confint=TRUE)

summ(svyglm( nulliparous ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( parity1 ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( parity2 ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( wants_children ~  + year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)

summ(svyglm( wants_children ~  + year  , subset(design.womenjoint, v502==1 & nulliparous==1), family=quasibinomial() ), confint=TRUE)

summ(svyglm( cpr ~   year  , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( cpr ~   year + as.factor(v013)   , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( cpr ~   year + as.factor(v013) + as.factor(marriage_group)  , subset(design.womenjoint, v502==1 & live_husband==1 & sex_month==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( cpr ~   year + as.factor(v013) + as.factor(marriage_group)   , subset(design.womenjoint, v502==1 & live_husband==1 & sex_month==1 & inf_meno==0), family=quasibinomial() ), confint=TRUE)
