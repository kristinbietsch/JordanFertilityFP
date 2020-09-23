# Jordan Feritlity and Family Planning
# Kristin Bietsch
# 09/18/20



# Abortion 2012 2017


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

women17 <- women17 %>% mutate(ever_t=case_when(v228==1 ~ 1,
                                               v228==0 ~ 0))
vcal_len <- str_length(women17$vcal_1[1])

smallwomen17 <- women17 %>% select(caseid, ever_t, s234a, vcal_1, v005, v008, v011, v017, v018, v021, v022, v023, v025 )

for (i in 1:vcal_len) {
  
  smallwomen17[paste0("method", i)] <- substr(smallwomen17$vcal_1, i, i)
  
}


long_women <- smallwomen17 %>% gather(Variable, Value, method1:method80)
long_women <- long_women %>% mutate(Termination=case_when(Value=="T" ~ 1,
                                                          Value!="T" ~ 0  ))

t_women <- long_women %>% group_by(caseid) %>% summarise(nT=sum(Termination)) 

women17 <- full_join(women17, t_women, by="caseid")

women17 <- women17 %>% mutate(term_2012 = case_when(nT==0 ~ 0,
                                                    nT>0 & s234a==1 ~ 1,
                                                    nT>0 & s234a==2 ~ 2,
                                                    nT>0 & s234a==3 ~ 3))


B_pos <- str_locate(women17$vcal_1, "B")
women17$B_pos <- B_pos[, 1]

T_pos <- str_locate(women17$vcal_1, "T")
women17$T_pos <- T_pos[, 1]

women17$B_pos[is.na(women17$B_pos)] <- 200
women17$T_pos[is.na(women17$T_pos)] <- 200


women17 <- women17 %>% mutate(BorT=case_when(B_pos<T_pos ~ 1,
                                             B_pos>T_pos ~ 2,
                                             B_pos==200 & T_pos==200 ~ 0),
                              outcome=case_when(BorT ==0 ~ "No Pregnancy Outcome in Calendar",
                                                BorT ==1 ~ "Most Recent Pregnancy Outcome: Birth",
                                                BorT==2 & s234a==1 ~ "Most Recent Pregnancy Outcome: Miscarriage",
                                                BorT==2 & s234a==2 ~ "Most Recent Pregnancy Outcome: Abortion",
                                                BorT==2 & s234a==3 ~ "Most Recent Pregnancy Outcome: Stillbirth"),
                              abortion=case_when(term_2012==2 ~ 1,
                                                        term_2012==1 | term_2012==3 ~ 0),
                              abortion_recent= case_when(BorT==2 & s234a==2 ~ 1,
                                                         (BorT ==1) | ( BorT==2 & s234a==1) | (BorT==2 & s234a==3) ~ 0),
                              miscarriage_recent= case_when(BorT==2 & s234a==1 ~ 1,
                                                            (BorT ==1) | ( BorT==2 & s234a==2) | (BorT==2 & s234a==3) ~ 0),
                              year=1)
  



#######################################
vcal_len12 <- str_length(women12$vcal_1[1])

smallwomen12 <- women12 %>% select(caseid,  s230a, vcal_1, v005, v008, v011, v017, v018, v021, v022, v023, v025 )

for (i in 1:vcal_len12) {
  
  smallwomen12[paste0("method", i)] <- substr(smallwomen12$vcal_1, i, i)
  
}


long_women12 <- smallwomen12 %>% gather(Variable, Value, method1:method80)
long_women12 <- long_women12 %>% mutate(Termination=case_when(Value=="T" ~ 1,
                                                          Value!="T" ~ 0  ))

t_women12 <- long_women12 %>% group_by(caseid) %>% summarise(nT=sum(Termination)) 

women12 <- full_join(women12, t_women12, by="caseid")

women12 <- women12 %>% mutate(term_pre2012 = case_when(nT==0 ~ 0,
                                                    nT>0 & s230a==1 ~ 1,
                                                    nT>0 & s230a==2 ~ 2,
                                                    nT>0 & s230a==3 ~ 3))


B_pos <- str_locate(women12$vcal_1, "B")
women12$B_pos <- B_pos[, 1]

T_pos <- str_locate(women12$vcal_1, "T")
women12$T_pos <- T_pos[, 1]

women12$B_pos[is.na(women12$B_pos)] <- 200
women12$T_pos[is.na(women12$T_pos)] <- 200


women12 <- women12 %>% mutate(BorT=case_when(B_pos<T_pos ~ 1,
                                             B_pos>T_pos ~ 2,
                                             B_pos==200 & T_pos==200 ~ 0),
                              outcome=case_when(BorT ==0 ~ "No Pregnancy Outcome in Calendar",
                                                BorT ==1 ~ "Most Recent Pregnancy Outcome: Birth",
                                                BorT==2 & s230a==1 ~ "Most Recent Pregnancy Outcome: Miscarriage",
                                                BorT==2 & s230a==2 ~ "Most Recent Pregnancy Outcome: Abortion",
                                                BorT==2 & s230a==3 ~ "Most Recent Pregnancy Outcome: Stillbirth"),
                              abortion=case_when(term_pre2012==2 ~ 1,
                                                 term_pre2012==1 | term_pre2012==3 ~ 0),
                              abortion_recent= case_when(BorT==2 & s230a==2 ~ 1,
                                                         (BorT ==1) | ( BorT==2 & s230a==1) | (BorT==2 & s230a==3) ~ 0),
                              miscarriage_recent=case_when(BorT==2 & s230a==1 ~ 1,
                                                           (BorT ==1) | ( BorT==2 & s230a==2) | (BorT==2 & s230a==3) ~ 0),
                              year=0)




########################################






women17$sampleweights <- women17$v005/1000000
women12$sampleweights <- women12$v005/1000000

design.women17 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women17, nest=TRUE)
options(survey.lonely.psu="adjust")

design.women12 <- svydesign(ids=~v021, strata=~v025, weights=~sampleweights, data=women12, nest=TRUE)
options(survey.lonely.psu="adjust")


# JOINT
joint_women <- bind_rows(women17, women12)
joint_women <- joint_women %>% mutate(v001_joint=case_when(year==1 ~ v001,
                                                           year==0 ~ v001 + 1000),
                                      v023_joint=case_when(year==1 ~ v023,
                                                           year==0 ~ v023 + 100))

sel <- select(joint_women, v001, v023, v001_joint, v023_joint, year)
design.womenjoint <- svydesign(ids=~v001_joint, strata=~v023_joint, weights=~sampleweights, data=joint_women, nest=TRUE)
options(survey.lonely.psu="adjust")

########################################


svymean(~as.factor(nT),  design.women17, na.rm=T)
svymean(~as.factor(term_2012),  design.women17, na.rm=T)
svymean(~as.factor(nT),  subset(design.women17, v502==1), na.rm=T)
svymean(~as.factor(term_2012),  subset(design.women17, v502==1), na.rm=T)

svymean(~as.factor(s234a),  subset(design.women17, nT>=1), na.rm=T)
svymean(~as.factor(s234a),  subset(design.women17, nT>=1 & v502==1), na.rm=T)


svymean(~as.factor(outcome),  design.women17, na.rm=T)
svymean(~as.factor(outcome),  subset(design.women17, v502==1), na.rm=T)
svymean(~as.factor(outcome),  subset(design.women17, outcome!="No Pregnancy Outcome in Calendar"), na.rm=T)
svymean(~as.factor(outcome),  subset(design.women17, outcome!="No Pregnancy Outcome in Calendar" &  v502==1), na.rm=T)



svymean(~as.factor(nT),  design.women12, na.rm=T)
svymean(~as.factor(term_pre2012),  design.women12, na.rm=T)
svymean(~as.factor(nT),  subset(design.women12, v502==1), na.rm=T)
svymean(~as.factor(term_pre2012),  subset(design.women12, v502==1), na.rm=T)

svymean(~as.factor(s230a),  subset(design.women12, nT>=1), na.rm=T)
svymean(~as.factor(s230a),  subset(design.women12, nT>=1 & v502==1), na.rm=T)


svymean(~as.factor(outcome),  design.women12, na.rm=T)
svymean(~as.factor(outcome),  subset(design.women12, v502==1), na.rm=T)
svymean(~as.factor(outcome),  subset(design.women12, outcome!="No Pregnancy Outcome in Calendar"), na.rm=T)
svymean(~as.factor(outcome),  subset(design.women12, outcome!="No Pregnancy Outcome in Calendar" &  v502==1), na.rm=T)



summ(svyglm( abortion ~  + year , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( abortion_recent ~  + year , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
summ(svyglm( miscarriage_recent ~  + year , subset(design.womenjoint, v502==1), family=quasibinomial() ), confint=TRUE)
