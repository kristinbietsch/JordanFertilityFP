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

women17 <- read_dta("JOIR73FL.DTA")



df <-women17 %>%  select( v005, v021, v025, v008, v011, b3_01, b3_02, b3_03, b3_04, b3_05, b3_06, b3_07,
                          b3_08, b3_09, b3_10, b3_11, b3_12, b3_13, b3_14, b3_15, b3_16, b3_17, b3_18 , b3_19, b3_20, awfactt)


df <- df %>% mutate(top=v008 - 1,
                    bot=v008 - 36,
                    turn15 =v011 + 180) %>%
  mutate(bot=case_when(turn15>bot ~ turn15,
                       turn15 <= bot ~ bot)) %>%
  filter( turn15!=v008) %>%
  mutate(agebot=floor((bot-v011)/12),
         agetop=floor((top-v011)/12),
         nages= agetop - agebot + 1,
         number=1) %>%
  mutate(id=cumsum(number)) %>%
  uncount( weights = nages, .remove = FALSE) %>%
  group_by(id) %>%
  mutate(withinid=cumsum(number)) %>%
  mutate(age=agebot + withinid-1,
         bday = v011 + (12*age)) %>%
  mutate(bot=case_when(bday > bot ~ bday,
                       bday <= bot ~ bot),
         top=case_when( (bday + 11) <top ~ bday + 11,
                        (bday + 11) >= top ~ top),
         expo=top - bot + 1)  %>% 
  mutate(b3_01= case_when(is.na(b3_01) ~ 0, !is.na(b3_01) ~ b3_01 ),
         b3_02= case_when(is.na(b3_02) ~ 0, !is.na(b3_02) ~ b3_02 ),
         b3_03= case_when(is.na(b3_03) ~ 0, !is.na(b3_03) ~ b3_03 ),
         b3_04= case_when(is.na(b3_04) ~ 0, !is.na(b3_04) ~ b3_04 ),
         b3_05= case_when(is.na(b3_05) ~ 0, !is.na(b3_05) ~ b3_05 ),
         b3_06= case_when(is.na(b3_06) ~ 0, !is.na(b3_06) ~ b3_06 ),
         b3_07= case_when(is.na(b3_07) ~ 0, !is.na(b3_07) ~ b3_07 ),
         b3_08= case_when(is.na(b3_08) ~ 0, !is.na(b3_08) ~ b3_08 ),
         b3_09= case_when(is.na(b3_09) ~ 0, !is.na(b3_09) ~ b3_09 )) %>%
  mutate(sampleweights=v005/1000000,
         awfact_weights = (awfactt /100) * (v005 /1000000),
         births=0)

for (i in 1:9) {
  
  df$births <-  ifelse(df[paste0("b3_0", i)] >= df$bot &  df[paste0("b3_0", i)] <= df$top, df$births + 1, df$births)
  
}


design <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=df)
design.awf <- svydesign(ids=~v021, strata=~v025, weights=~awfact_weights, data=df)

asfr <- svyby(~births, by=~age,
              design=design,
              FUN=svytotal,
              na.rm=TRUE,
              weights=~sampleweights,
              data=df)
asfr1 <- svyby(~expo, by=~age,
               design=design.awf,
               FUN=svytotal,
               na.rm=TRUE,
               weights=~awfact_weights,
               data=df)
asfr <- full_join(asfr, asfr1, by="age") %>% mutate(group=case_when(age>=15 & age<=19 ~ 1,
                                                                    age>=20 & age<=24 ~ 2,
                                                                    age>=25 & age<=29 ~ 3,
                                                                    age>=30 & age<=34 ~ 4,
                                                                    age>=35 & age<=39 ~ 5,
                                                                    age>=40 & age<=44 ~ 6,
                                                                    age>=45 & age<=49 ~ 7)) %>%
  group_by(group) %>%
  dplyr::summarise(births=sum(births), expo=sum(expo/12)) %>% mutate(asfr=births/expo)

tfr <- asfr %>% dplyr::summarise(TFR=sum(asfr)*5) 
