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


women17$sampleweights <- women17$v005/1000000
women12$sampleweights <- women12$v005/1000000

design.women17 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women17, nest=TRUE)
options(survey.lonely.psu="adjust")

design.women12 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women12, nest=TRUE)
options(survey.lonely.psu="adjust")


svymean(~as.factor(v312),  design.women17)
svymean(~as.factor(v312),  subset(design.women17, v313==3))

svymean(~as.factor(v312),  design.women12)
svymean(~as.factor(v312),  subset(design.women12, v313==3))
