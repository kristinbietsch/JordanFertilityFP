# Jordan Feritlity and Family Planning
# Kristin Bietsch
# 09/18/20



# Significance testing PR files
# Clean TFR Code

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

pr17 <-  read_dta("JOPR71FL.DTA")
pr12 <- read_dta("JOPR6CFL.DTA")

pr_data17 <- pr17 %>% 
  filter(hv104==2) %>% 
  filter(hv105>=15 & hv105<=49) %>%
  mutate(married=case_when(hv116==1 ~ 1, hv116!=1 ~ 0),
         age=case_when( hv105>=15 & hv105<20 ~ 1,
                        hv105>=20 & hv105<25 ~ 2,
                        hv105>=25 & hv105<30 ~ 3,
                        hv105>=30 & hv105<35 ~ 4,
                        hv105>=35 & hv105<40 ~ 5,
                        hv105>=40 & hv105<45 ~ 6,
                        hv105>=45 & hv105<50 ~ 7),
         nationality=case_when(sh07a==1 ~ 1,
                               sh07a==3 ~ 2,
                               sh07a==2 | sh07a==4 | sh07a==5 | sh07a==6 | sh07a==8 ~ 3),
         year=1,
         sampleweights=hv005/1000000)


pr_data12 <- pr12 %>% 
  filter(hv104==2) %>% 
  filter(hv105>=15 & hv105<=49) %>%
  mutate(married=case_when(hv116==1 ~ 1, hv116!=1 ~ 0),
         age=case_when( hv105>=15 & hv105<20 ~ 1,
                        hv105>=20 & hv105<25 ~ 2,
                        hv105>=25 & hv105<30 ~ 3,
                        hv105>=30 & hv105<35 ~ 4,
                        hv105>=35 & hv105<40 ~ 5,
                        hv105>=40 & hv105<45 ~ 6,
                        hv105>=45 & hv105<50 ~ 7),
         nationality=case_when(sh07==1 ~ 1,
                               sh07==3 ~ 2,
                               sh07==2 | sh07==4 | sh07==5 | sh07==6 | sh07==8 ~ 3),
         year=0,
         sampleweights=hv005/1000000)

joint_women <- bind_rows(pr_data17, pr_data12)
joint_women <- joint_women %>% mutate(v001_joint=case_when(year==1 ~ hv001,
                                                           year==0 ~ hv001 + 1000),
                                      v023_joint=case_when(year==1 ~ hv023,
                                                           year==0 ~ hv023 + 100))

design.womenjoint <- svydesign(ids=~v001_joint, strata=~v023_joint, weights=~sampleweights, data=joint_women, nest=TRUE)
options(survey.lonely.psu="adjust")

summ(svyglm( married ~  + year  , design.womenjoint, family=quasibinomial() ), confint=TRUE)
summ(svyglm( married ~  + year  , subset(design.womenjoint, age==1), family=quasibinomial() ), confint=TRUE)

