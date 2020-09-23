# Jordan Feritlity and Family Planning
# Kristin Bietsch
# 09/18/20



# CPR Month by Month


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

vcal_len <- str_length(women17$vcal_1[1])

smallwomen17 <- women17 %>% select(caseid, vcal_1, v005, v008, v011, v017, v018, v021, v022, v023, v025 )

for (i in 1:vcal_len) {

  smallwomen17[paste0("method", i)] <- substr(smallwomen17$vcal_1, i, i)
  
}

long_women <- smallwomen17 %>% gather(Variable, Value, method1:method80)

# find the position of the earliest date of interview (the maximum value of v018) 
v018_max = max(long_women$v018) 

# drop cases outside of the five years preceding the earliest interview 
# months 0-59 before the earliest interview date 
#keep if inrange(i,v018_max,v018_max+59) 
long_women$month <- as.numeric(substr(long_women$Variable, 7, 8))
long_women <- long_women %>% filter(month>= v018_max & month <= v018_max+59)



# calculate age in months for each month in the calendar 
# gen agem = (v008 - v011) - (i - v018) 
# calculate century month code for each month 
#gen cmctime = v008 - (i - v018) 
#label variable cmctime "Century month code" 

long_women <- long_women %>% mutate(agem= (v008 - v011) - (month - v018),
                                    cmctime = v008 - (month - v018) )


# create variable for use of any method as a 0/100 variable 
long_women <- long_women %>% mutate(usingany=case_when(Value== "0" | Value== "B" | Value== "P" | Value== "T"  ~ 0,
                                                       Value== "1" | Value== "2" | Value== "3" | Value== "5" | Value== "6" | 
                                                         Value== "7" | Value== "8" | Value== "9"  | Value== "C" | Value== "E" | 
                                                         Value== "L" | Value== "N" | Value== "Y" ~ 1),
                                    usingmodern=case_when(Value== "0" | Value== "B" | Value== "P" | Value== "T" | Value== "8" | Value== "9" | Value== "Y" ~ 0,
                                                       Value== "1" | Value== "2" | Value== "3" | Value== "5" | Value== "6" | 
                                                         Value== "7"   | Value== "C" | Value== "E" | 
                                                         Value== "L" | Value== "N" ~ 1))
levels(as.factor(long_women$Value))

table(long_women$usingmodern)





long_women$sampleweights <- long_women$v005/1000000

design.women17 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=long_women, nest=TRUE)
options(survey.lonely.psu="adjust")

svyby( ~usingany , ~ cmctime ,  subset(design.women17, agem>=180 & agem<=539), svymean)
svyby( ~usingmodern , ~ cmctime ,  subset(design.women17, agem>=180 & agem<=539), svymean)
