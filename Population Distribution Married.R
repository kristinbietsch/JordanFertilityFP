# Jordan Feritlity and Family Planning
# Kristin Bietsch
# 09/18/20



# Population Distribution Married

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

pr17 <- read_dta("JOPR71FL.DTA")
pr12 <- read_dta("JOPR6CFL.DTA")

pr17 <- pr17 %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<50) %>% mutate(married=case_when(hv116==1 ~ 1,
                                                                                                hv116!=1 ~ 0),
                                                                              age= case_when( hv105>=15 & hv105<20 ~ 1,
                                                                                              hv105>=20 & hv105<25 ~ 2,
                                                                                              hv105>=25 & hv105<30 ~ 3,
                                                                                              hv105>=30 & hv105<35 ~ 4,
                                                                                              hv105>=35 & hv105<40 ~ 5,
                                                                                              hv105>=40 & hv105<45 ~ 6,
                                                                                              hv105>=45 & hv105<50 ~ 7),
                                                                              nationality= case_when(sh07a==1 ~ "Jordanian",
                                                                                                     sh07a==3 ~ "Syrian",
                                                                                                     sh07a==2 | sh07a==4 | sh07a==5 | sh07a==6 | sh07a==8 ~ "Other"))

pr12 <- pr12 %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<50) %>% mutate(married=case_when(hv116==1 ~ 1,
                                                                                                hv116!=1 ~ 0),
                                                                              age= case_when( hv105>=15 & hv105<20 ~ 1,
                                                                                              hv105>=20 & hv105<25 ~ 2,
                                                                                              hv105>=25 & hv105<30 ~ 3,
                                                                                              hv105>=30 & hv105<35 ~ 4,
                                                                                              hv105>=35 & hv105<40 ~ 5,
                                                                                              hv105>=40 & hv105<45 ~ 6,
                                                                                              hv105>=45 & hv105<50 ~ 7),
                                                                              nationality= case_when(sh07==1 ~ "Jordanian",
                                                                                                     sh07==3 ~ "Syrian",
                                                                                                     sh07==2 | sh07==4 | sh07==5 | sh07==6 | sh07==8 ~ "Other"))


women17 <- women17 %>% mutate(wants_year=case_when(v604==0 ~ 1, v604!=0 ~ 0, v602!=1 ~ 0),
                              time_marriage= (v008-v509)/12,
                              marriage_group=case_when(time_marriage<2 ~ 1, 
                                                       time_marriage>=2 & time_marriage<5 ~ 2,
                                                       time_marriage>=5 & time_marriage<10 ~ 3,
                                                       time_marriage>=10 & time_marriage<15 ~ 4,
                                                       time_marriage>=15 ~ 5))
women12 <- women12 %>% mutate(wants_year=case_when(v604==0 ~ 1, v604!=0 ~ 0, v602!=1 ~ 0),
                              time_marriage= (v008-v509)/12,
                              marriage_group=case_when(time_marriage<2 ~ 1, 
                                                       time_marriage>=2 & time_marriage<5 ~ 2,
                                                       time_marriage>=5 & time_marriage<10 ~ 3,
                                                       time_marriage>=10 & time_marriage<15 ~ 4,
                                                       time_marriage>=15 ~ 5),
                              nationality=case_when(sh07== 1 ~ "Jordan",
                                               sh07== 3 ~ "Syrian" ,
                                               sh07==2 | sh07==4 | sh07==5 | sh07==6 | sh07==8 ~  "Other" ))


women17$sampleweights <- women17$v005/1000000
women12$sampleweights <- women12$v005/1000000



pr17$sampleweights <- pr17$hv005/1000000
pr12$sampleweights <- pr12$hv005/1000000


design.PR17 <- svydesign(ids=~hv021, strata=~hv025, weights=~sampleweights, data=pr17, nest=TRUE)
options(survey.lonely.psu="adjust")

design.PR12 <- svydesign(ids=~hv021, strata=~hv025, weights=~sampleweights, data=pr12, nest=TRUE)
options(survey.lonely.psu="adjust")

design.women17 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women17, nest=TRUE)
options(survey.lonely.psu="adjust")

design.women12 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women12, nest=TRUE)
options(survey.lonely.psu="adjust")



svymean(~as.factor(married), design.PR17)
svymean(~as.factor(age), design.PR17)
svymean(~as.factor(age),  subset(design.PR17, married==1))
svyby(~married , ~as.factor(age) , design.PR17 ,  svymean, na.rm=T)

svyby(~as.factor(age)  , ~as.factor(nationality), design.PR17 ,  svymean, na.rm=T)

svyby(~as.factor(married)  , ~as.factor(nationality), subset(design.PR17, age==1) ,  svymean, na.rm=T)
svyby(~as.factor(married)  , ~as.factor(nationality), subset(design.PR17, age==2) ,  svymean, na.rm=T)
svyby(~as.factor(married)  , ~as.factor(nationality), subset(design.PR17, age==3) ,  svymean, na.rm=T)
svyby(~as.factor(married)  , ~as.factor(nationality), subset(design.PR17, age==4) ,  svymean, na.rm=T)
svyby(~as.factor(married)  , ~as.factor(nationality), subset(design.PR17, age==5) ,  svymean, na.rm=T)
svyby(~as.factor(married)  , ~as.factor(nationality), subset(design.PR17, age==6) ,  svymean, na.rm=T)
svyby(~as.factor(married)  , ~as.factor(nationality), subset(design.PR17, age==7) ,  svymean, na.rm=T)

svyby(~as.factor(age)  , ~as.factor(nationality), subset(design.PR17, married==1) ,  svymean, na.rm=T)


svymean(~as.factor(married), design.PR12)
svymean(~as.factor(age), design.PR12)
svymean(~as.factor(age),  subset(design.PR12, married==1))
svyby(~married , ~as.factor(age) , design.PR12 ,  svymean, na.rm=T)

svyby(~as.factor(age)  , ~as.factor(nationality), design.PR12 ,  svymean, na.rm=T)

svyby(~as.factor(married)  , ~as.factor(nationality), subset(design.PR12, age==1) ,  svymean, na.rm=T)
svyby(~as.factor(married)  , ~as.factor(nationality), subset(design.PR12, age==2) ,  svymean, na.rm=T)
svyby(~as.factor(married)  , ~as.factor(nationality), subset(design.PR12, age==3) ,  svymean, na.rm=T)
svyby(~as.factor(married)  , ~as.factor(nationality), subset(design.PR12, age==4) ,  svymean, na.rm=T)
svyby(~as.factor(married)  , ~as.factor(nationality), subset(design.PR12, age==5) ,  svymean, na.rm=T)
svyby(~as.factor(married)  , ~as.factor(nationality), subset(design.PR12, age==6) ,  svymean, na.rm=T)
svyby(~as.factor(married)  , ~as.factor(nationality), subset(design.PR12, age==7) ,  svymean, na.rm=T)

svyby(~as.factor(age)  , ~as.factor(nationality), subset(design.PR12, married==1) ,  svymean, na.rm=T)


svymean(~as.factor(marriage_group), subset(design.women17, v502==1))

svymean(~as.factor(marriage_group), subset(design.women12, v502==1))

svymean(~as.factor(nationality), subset(design.women12, v502==1))
svymean(~as.factor(snat), subset(design.women17, v502==1))
