# Jordan Feritlity and Family Planning
# Kristin Bietsch
# 09/18/20



# CPR by Nationality


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
women09 <- read_dta("JOIR61FL.DTA")
women07 <- read_dta("JOIR51FL.DTA")

nationality2012  <- read_dta("nationality2012.DTA")
women12 <- left_join(women12, nationality2012, by=c("v000", "v001", "v002", "v003"))



women17 <- women17 %>% mutate(parity= case_when(v201>=6 ~ 6,
                                                v201<6 ~ v201),
                              limit=case_when(v605==5 | v605==6 ~ 1,
                                              v605==1 | v605==2 | v605==3 | v605==4 | v605==7 ~ 0 ),
                              syrian=case_when(s123a==3 ~ 1,
                                               s123a==1 ~ 0))


women12 <- women12 %>% mutate(parity= case_when(v201>=6 ~ 6,
                                                v201<6 ~ v201),
                              limit=case_when(v605==5 | v605==6 ~ 1,
                                              v605==1 | v605==2 | v605==3 | v605==4 | v605==7 ~ 0 ),
                              syrian=case_when(sh07==3 ~ 1,
                                               sh07==1 ~ 0))


women09 <- women09 %>% mutate(parity= case_when(v201>=6 ~ 6,
                                                v201<6 ~ v201),
                              limit=case_when(v605==5 | v605==6 ~ 1,
                                              v605==1 | v605==2 | v605==3 | v605==4 | v605==7 ~ 0 ))


women07 <- women07 %>% mutate(parity= case_when(v201>=6 ~ 6,
                                                v201<6 ~ v201),
                              limit=case_when(v605==5 | v605==6 ~ 1,
                                              v605==1 | v605==2 | v605==3 | v605==4 | v605==7 ~ 0 ))

women17$sampleweights <- women17$v005/1000000
women12$sampleweights <- women12$v005/1000000
women09$sampleweights <- women09$v005/1000000
women07$sampleweights <- women07$v005/1000000

design.women17 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women17, nest=TRUE)
options(survey.lonely.psu="adjust")

design.women12 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women12, nest=TRUE)
options(survey.lonely.psu="adjust")

design.women09 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women09, nest=TRUE)
options(survey.lonely.psu="adjust")

design.women07 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women07, nest=TRUE)
options(survey.lonely.psu="adjust")



svyby( ~limit , ~ parity ,  subset(design.women17, v502==1), svymean)
svyby( ~limit , ~ parity ,  subset(design.women17, v502==1 & syrian==1), svymean)
svyby( ~limit , ~ parity ,  subset(design.women17, v502==1 & syrian==0), svymean)

svyby( ~limit , ~ parity ,  subset(design.women12, v502==1), svymean)
svyby( ~limit , ~ parity ,  subset(design.women12, v502==1 & syrian==1), svymean)
svyby( ~limit , ~ parity ,  subset(design.women12, v502==1 & syrian==0), svymean)

svyby( ~limit , ~ parity ,  subset(design.women09, v502==1), svymean)

svyby( ~limit , ~ parity ,  subset(design.women07, v502==1), svymean)
