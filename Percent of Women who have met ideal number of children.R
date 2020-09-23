# Jordan Feritlity and Family Planning
# Kristin Bietsch
# 09/18/20



# Achieved Ideal Fertility


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


# just comparing jordanians to syrians
women17 <- women17 %>% mutate(met_ideal=case_when(v201>=v613 ~ 1,
                                                  v201<v613 ~ 0))


women12 <- women12 %>% mutate(met_ideal=case_when(v201>=v613 ~ 1,
                                                  v201<v613 ~ 0))



women17$sampleweights <- women17$v005/1000000
women12$sampleweights <- women12$v005/1000000


design.women17 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women17, nest=TRUE)
options(survey.lonely.psu="adjust")

design.women12 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women12, nest=TRUE)
options(survey.lonely.psu="adjust")

svymean(~as.factor(met_ideal),  subset(design.women17, v502==1))
svymean(~as.factor(met_ideal),  subset(design.women12, v502==1))
