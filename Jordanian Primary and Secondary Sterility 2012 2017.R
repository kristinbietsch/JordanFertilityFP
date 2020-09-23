# Jordan Feritlity and Family Planning
# Kristin Bietsch
# 09/18/20



# Primary and Secondary Sterility


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

women17 <- women17 %>% mutate(
                              inf_meno=case_when(v626a==9 ~ 1,
                                                 v626a!=9 ~ 0,
                                                 is.na(v626a) ~ 0))
women12 <- women12 %>% mutate(
                              inf_meno=case_when(v626a==9 ~ 1,
                                                 v626a!=9 ~ 0,
                                                 is.na(v626a) ~ 0))

women17$sampleweights <- women17$v005/1000000
women12$sampleweights <- women12$v005/1000000

design.women17 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women17, nest=TRUE)
options(survey.lonely.psu="adjust")

design.women12 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women12, nest=TRUE)
options(survey.lonely.psu="adjust")

svymean(~as.factor(v201),  subset(design.women17, v013==7))
svyby( ~inf_meno , ~ as.factor(v013), design.women17 ,  svymean, na.rm=T)

table(women17$inf_meno)

svymean(~as.factor(v201),  subset(design.women12, v013==7))
svyby( ~inf_meno , ~ as.factor(v013), design.women12 ,  svymean, na.rm=T)
