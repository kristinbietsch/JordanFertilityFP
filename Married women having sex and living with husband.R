# Jordan Feritlity and Family Planning
# Kristin Bietsch
# 09/18/20



# married women having sex and living with husband


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


women17 <- women17 %>% mutate(live_husband=case_when(v504==1 ~ 1,
                                                  v504==2 ~ 0),
                              sex_month=case_when(v529==0 ~ 1,
                                                  v529!=0 ~ 0))


women12 <- women12 %>% mutate(live_husband=case_when(v504==1 ~ 1,
                                                    v504==2 ~ 0),
                             sex_month=case_when(v529==0 ~ 1,
                                                 v529!=0 ~ 0,
                                                 is.na(v529) ~ 0))

# problem with 2012

women17$sampleweights <- women17$v005/1000000
women12$sampleweights <- women12$v005/1000000


design.women17 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women17, nest=TRUE)
options(survey.lonely.psu="adjust")

design.women12 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women12, nest=TRUE)
options(survey.lonely.psu="adjust")

svymean(~live_husband,  subset(design.women17, v502==1), na.rm=T)
svymean(~live_husband,  subset(design.women12, v502==1), na.rm=T)

svymean(~sex_month,  subset(design.women17, v502==1), na.rm=T)
svymean(~sex_month,  subset(design.women12, v502==1), na.rm=T)

svymean(~sex_month,  subset(design.women17, v502==1 & live_husband==1), na.rm=T)
svymean(~sex_month,  subset(design.women12, v502==1 & live_husband==1), na.rm=T)

table(women12$sex_month)
