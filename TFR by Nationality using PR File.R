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
pr17 <-  read_dta("JOPR71FL.DTA")

women12 <- read_dta("JOIR6CFL.DTA")
pr12 <- read_dta("JOPR6CFL.DTA")

nationality2012  <- read_dta("nationality2012.DTA")

women12 <- left_join(women12, nationality2012, by=c("v000", "v001", "v002", "v003"))

############################
# PR Prep

pr_data <- pr17 %>% 
  filter(hv104==2) %>% 
  filter(hv105>=15 & hv105<=49) %>% 
  filter(hv116==0) %>%
  mutate(v011 = hv008-((12*hv105) + 6),
         b3_01=NA,
         b3_02=NA,
         b3_03=NA,
         b3_04=NA,
         b3_05=NA,
         b3_06=NA,
         b3_07=NA,
         b3_08=NA,
         b3_09=NA,
         b3_10=NA,
         b3_11=NA,
         b3_12=NA,
         b3_13=NA,
         b3_14=NA,
         b3_15=NA,
         b3_16=NA,
         b3_17=NA,
         b3_18=NA,
         b3_19=NA,
         b3_20=NA) %>%
  rename(v008=hv008, 
         v005=hv005,
         s123a=sh07a,
         v021=hv021,
         v025=hv025) %>%
  select(v005, v008, v011, v021, v025, s123a,  b3_01:b3_20)

pr_data12 <- pr12 %>% 
  filter(hv104==2) %>% 
  filter(hv105>=15 & hv105<=49) %>% 
  filter(hv116==0) %>%
  mutate(v011 = hv008-((12*hv105) + 6),
         b3_01=NA,
         b3_02=NA,
         b3_03=NA,
         b3_04=NA,
         b3_05=NA,
         b3_06=NA,
         b3_07=NA,
         b3_08=NA,
         b3_09=NA,
         b3_10=NA,
         b3_11=NA,
         b3_12=NA,
         b3_13=NA,
         b3_14=NA,
         b3_15=NA,
         b3_16=NA,
         b3_17=NA,
         b3_18=NA,
         b3_19=NA,
         b3_20=NA) %>%
  rename(v008=hv008, 
         v005=hv005,
         v021=hv021,
         v025=hv025) %>%
  select(v005, v008, v011, v021, v025, sh07,  b3_01:b3_20)



df <-women17 %>%  select(v005, v008, v011, v021, v025, s123a,  b3_01:b3_20)
df <- bind_rows(df, pr_data)


df12 <-women12 %>%  select(v005, v008, v011, v021, v025, sh07,  b3_01:b3_20)
df12 <- bind_rows(df12, pr_data12)



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
         births=0)

for (i in 1:9) {
  
  df$births <-  ifelse(df[paste0("b3_0", i)] >= df$bot &  df[paste0("b3_0", i)] <= df$top, df$births + 1, df$births)
  
}






df12 <- df12 %>% mutate(top=v008 - 1,
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
         births=0)

for (i in 1:9) {
  
  df12$births <-  ifelse(df12[paste0("b3_0", i)] >= df12$bot &  df12[paste0("b3_0", i)] <= df12$top, df12$births + 1, df12$births)
  
}

design <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=df)
design12 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=df12)


##################################################################3
# National
asfr <- svyby(~births, by=~age,
              design=design,
              FUN=svytotal,
              na.rm=TRUE,
              weights=~sampleweights,
              data=df)
asfr1 <- svyby(~expo, by=~age,
               design=design,
               FUN=svytotal,
               na.rm=TRUE,
               weights=~sampleweights,
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





asfr12 <- svyby(~births, by=~age,
              design=design12,
              FUN=svytotal,
              na.rm=TRUE,
              weights=~sampleweights,
              data=df12)
asfr112 <- svyby(~expo, by=~age,
               design=design12,
               FUN=svytotal,
               na.rm=TRUE,
               weights=~sampleweights,
               data=df12)
asfr12 <- full_join(asfr12, asfr112, by="age") %>% mutate(group=case_when(age>=15 & age<=19 ~ 1,
                                                                    age>=20 & age<=24 ~ 2,
                                                                    age>=25 & age<=29 ~ 3,
                                                                    age>=30 & age<=34 ~ 4,
                                                                    age>=35 & age<=39 ~ 5,
                                                                    age>=40 & age<=44 ~ 6,
                                                                    age>=45 & age<=49 ~ 7)) %>%
  group_by(group) %>%
  dplyr::summarise(births=sum(births), expo=sum(expo/12)) %>% mutate(asfr=births/expo)

tfr12 <- asfr12 %>% dplyr::summarise(TFR=sum(asfr)*5) 
####################################################################
# Jordanian
asfr_jor12 <- svyby(~births, by=~age,
              design=subset(design12, sh07==1),
              FUN=svytotal,
              na.rm=TRUE,
              weights=~sampleweights,
              data=df12)
asfr1_jor12 <- svyby(~expo, by=~age,
               design=subset(design12, sh07==1),
               FUN=svytotal,
               na.rm=TRUE,
               weights=~sampleweights,
               data=df12)
asfr_jor12 <- full_join(asfr_jor12, asfr1_jor12, by="age") %>% mutate(group=case_when(age>=15 & age<=19 ~ 1,
                                                                    age>=20 & age<=24 ~ 2,
                                                                    age>=25 & age<=29 ~ 3,
                                                                    age>=30 & age<=34 ~ 4,
                                                                    age>=35 & age<=39 ~ 5,
                                                                    age>=40 & age<=44 ~ 6,
                                                                    age>=45 & age<=49 ~ 7)) %>%
  group_by(group) %>%
  dplyr::summarise(births=sum(births), expo=sum(expo/12)) %>% mutate(asfr=births/expo)

tfr_jor12 <- asfr_jor12 %>% dplyr::summarise(TFR=sum(asfr)*5) 


asfr_jor <- svyby(~births, by=~age,
                    design=subset(design, s123a==1),
                    FUN=svytotal,
                    na.rm=TRUE,
                    weights=~sampleweights,
                    data=df)
asfr1_jor<- svyby(~expo, by=~age,
                     design=subset(design, s123a==1),
                     FUN=svytotal,
                     na.rm=TRUE,
                     weights=~sampleweights,
                     data=df)
asfr_jor <- full_join(asfr_jor, asfr1_jor, by="age") %>% mutate(group=case_when(age>=15 & age<=19 ~ 1,
                                                                                      age>=20 & age<=24 ~ 2,
                                                                                      age>=25 & age<=29 ~ 3,
                                                                                      age>=30 & age<=34 ~ 4,
                                                                                      age>=35 & age<=39 ~ 5,
                                                                                      age>=40 & age<=44 ~ 6,
                                                                                      age>=45 & age<=49 ~ 7)) %>%
  group_by(group) %>%
  dplyr::summarise(births=sum(births), expo=sum(expo/12)) %>% mutate(asfr=births/expo)

tfr_jor <- asfr_jor %>% dplyr::summarise(TFR=sum(asfr)*5) 
####################################################################
# Syrian
asfr_syr12 <- svyby(~births, by=~age,
                  design=subset(design12, sh07==3),
                  FUN=svytotal,
                  na.rm=TRUE,
                  weights=~sampleweights,
                  data=df12)
asfr1_syr12 <- svyby(~expo, by=~age,
                   design=subset(design12, sh07==3),
                   FUN=svytotal,
                   na.rm=TRUE,
                   weights=~sampleweights,
                   data=df12)
asfr_syr12 <- full_join(asfr_syr12, asfr1_syr12, by="age") %>% mutate(group=case_when(age>=15 & age<=19 ~ 1,
                                                                                age>=20 & age<=24 ~ 2,
                                                                                age>=25 & age<=29 ~ 3,
                                                                                age>=30 & age<=34 ~ 4,
                                                                                age>=35 & age<=39 ~ 5,
                                                                                age>=40 & age<=44 ~ 6,
                                                                                age>=45 & age<=49 ~ 7)) %>%
  group_by(group) %>%
  dplyr::summarise(births=sum(births), expo=sum(expo/12)) %>% mutate(asfr=births/expo)

tfr_syr12 <- asfr_syr12 %>% dplyr::summarise(TFR=sum(asfr)*5) 


# Syrian
asfr_syr <- svyby(~births, by=~age,
                    design=subset(design, s123a==3),
                    FUN=svytotal,
                    na.rm=TRUE,
                    weights=~sampleweights,
                    data=df)
asfr1_syr <- svyby(~expo, by=~age,
                     design=subset(design, s123a==3),
                     FUN=svytotal,
                     na.rm=TRUE,
                     weights=~sampleweights,
                     data=df)
asfr_syr <- full_join(asfr_syr, asfr1_syr, by="age") %>% mutate(group=case_when(age>=15 & age<=19 ~ 1,
                                                                                      age>=20 & age<=24 ~ 2,
                                                                                      age>=25 & age<=29 ~ 3,
                                                                                      age>=30 & age<=34 ~ 4,
                                                                                      age>=35 & age<=39 ~ 5,
                                                                                      age>=40 & age<=44 ~ 6,
                                                                                      age>=45 & age<=49 ~ 7)) %>%
  group_by(group) %>%
  dplyr::summarise(births=sum(births), expo=sum(expo/12)) %>% mutate(asfr=births/expo)

tfr_syr <- asfr_syr %>% dplyr::summarise(TFR=sum(asfr)*5) 

######################################################################################