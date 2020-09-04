* Jordan Abortions 2017

clear all
set maxvar 10000

use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR72FL.DTA"

* Ever had a terminated Pregnancy
tab v228 [aw=v005/1000000]

 gen nT = length(vcal_1) - length(subinstr(vcal_1, "T", "", .))
 
tab nT [aw=v005/1000000]

* termination since 2012
gen termination=0 if nT==0
replace termination=1 if s234a==1 & nT!=0
replace termination=2 if s234a==2 & nT!=0
replace termination=3 if s234a==3 & nT!=0

label define termination 0 "No Termination since Before Calendar" 1 "Most Recent Termination: Miscarriage" 2 "Most Recent Termination: Abortion" 3 "Most Recent Termination: Stillbirth"
label values termination termination

tab termination [aw=v005/1000000]

*tab nT termination [aw=v005/1000000]
*tab nT s234a [aw=v005/1000000], m
tab nT  [aw=v005/1000000]


tab termination if v502==1 [aw=v005/1000000]
tab nT  if v502==1  [aw=v005/1000000]


tab s234a if nT>=1  [aw=v005/1000000]
tab s234a if nT>=1 & v502==1 [aw=v005/1000000]


***** looking at most recent end of pregnancy in calendar
gen posB = strpos(vcal_1,"B")
replace posB=200 if posB==0
gen posT = strpos(vcal_1,"T")
replace posT=200 if posT==0

gen BorT= 1 if posB<posT
replace BorT= 2 if posB>posT
replace BorT=0 if posB==200 & posT==200


gen outcome=0 if BorT==0
replace outcome=1 if BorT==1
replace outcome=2 if BorT==2 & s234a==1
replace outcome=3 if BorT==2 & s234a==2
replace outcome=4 if BorT==2 & s234a==3

label define outcome 0 "No pregnancy outcome in calendar" 1 "Most Recent Pregnancy Outcome: Birth" 2 "Most Recent Pregnancy Outcome: Miscarriage" 3 "Most Recent Pregnancy Outcome: Abortion" 4 "Most Recent Pregnancy Outcome: Stillbirth"
label values outcome outcome

tab outcome [aw=v005/1000000]
tab outcome if v502==1 [aw=v005/1000000]

tab outcome if outcome!=0 [aw=v005/1000000]
tab outcome if outcome!=0 & v502==1 [aw=v005/1000000]

tab outcome s123a if outcome!=0 & v502==1 [aw=v005/1000000], col nofreq


*********************************************
clear all
 use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"
 
 gen nT = length(vcal_1) - length(subinstr(vcal_1, "T", "", .))
 
 tab nT [aw=v005/1000000]

 tab nT s230a [aw=v005/1000000],m
 
 gen termination=0 if nT==0
replace termination=1 if s230a==1 & nT!=0
replace termination=2 if s230a==2 & nT!=0
replace termination=3 if s230a==3 & nT!=0

label define termination 0 "No Termination since Before Calendar" 1 "Most Recent Termination: Miscarriage" 2 "Most Recent Termination: Abortion" 3 "Most Recent Termination: Stillbirth"
label values termination termination

tab termination [aw=v005/1000000]

*tab nT termination [aw=v005/1000000]
*tab nT s230a [aw=v005/1000000], m

tab nT [aw=v005/1000000]

tab termination if v502==1 [aw=v005/1000000]
tab nT  if v502==1  [aw=v005/1000000]

tab s230a if nT>=1  [aw=v005/1000000]
tab s230a if nT>=1  & v502==1 [aw=v005/1000000]


***** looking at most recent end of pregnancy in calendar
gen posB = strpos(vcal_1,"B")
replace posB=200 if posB==0
gen posT = strpos(vcal_1,"T")
replace posT=200 if posT==0

gen BorT= 1 if posB<posT
replace BorT= 2 if posB>posT
replace BorT=0 if posB==200 & posT==200


gen outcome=0 if BorT==0
replace outcome=1 if BorT==1
replace outcome=2 if BorT==2 & s230a==1
replace outcome=3 if BorT==2 & s230a==2
replace outcome=4 if BorT==2 & s230a==3

label define outcome 0 "No pregnancy outcome in calendar" 1 "Most Recent Pregnancy Outcome: Birth" 2 "Most Recent Pregnancy Outcome: Miscarriage" 3 "Most Recent Pregnancy Outcome: Abortion" 4 "Most Recent Pregnancy Outcome: Stillbirth"
label values outcome outcome

tab outcome [aw=v005/1000000]
tab outcome if v502==1 [aw=v005/1000000]

tab outcome if outcome!=0 [aw=v005/1000000]
tab outcome if outcome!=0 & v502==1 [aw=v005/1000000]



***********************************************************************
clear all
set maxvar 10000

use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR72FL.DTA"
gen nT = length(vcal_1) - length(subinstr(vcal_1, "T", "", .))

gen termination=0 if nT==0
replace termination=1 if s234a==1 & nT!=0
replace termination=2 if s234a==2 & nT!=0
replace termination=3 if s234a==3 & nT!=0

label define termination 0 "No Termination since Before Calendar" 1 "Most Recent Termination: Miscarriage" 2 "Most Recent Termination: Abortion" 3 "Most Recent Termination: Stillbirth"
label values termination termination
gen abortion=1 if termination==2
replace abortion=0 if termination==1 | termination==3

***** looking at most recent end of pregnancy in calendar
gen posB = strpos(vcal_1,"B")
replace posB=200 if posB==0
gen posT = strpos(vcal_1,"T")
replace posT=200 if posT==0

gen BorT= 1 if posB<posT
replace BorT= 2 if posB>posT
replace BorT=0 if posB==200 & posT==200


gen outcome=0 if BorT==0
replace outcome=1 if BorT==1
replace outcome=2 if BorT==2 & s234a==1
replace outcome=3 if BorT==2 & s234a==2
replace outcome=4 if BorT==2 & s234a==3

label define outcome 0 "No pregnancy outcome in calendar" 1 "Most Recent Pregnancy Outcome: Birth" 2 "Most Recent Pregnancy Outcome: Miscarriage" 3 "Most Recent Pregnancy Outcome: Abortion" 4 "Most Recent Pregnancy Outcome: Stillbirth"
label values outcome outcome

gen abortion_recent=1 if outcome==3
replace abortion_recent=0 if outcome==1 | outcome==2 | outcome==4

gen year=1

save "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIRcombo1.dta", replace

clear all
set maxvar 10000

 use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"
 
 gen nT = length(vcal_1) - length(subinstr(vcal_1, "T", "", .))
  
 gen termination=0 if nT==0
replace termination=1 if s230a==1 & nT!=0
replace termination=2 if s230a==2 & nT!=0
replace termination=3 if s230a==3 & nT!=0

label define termination 0 "No Termination since Before Calendar" 1 "Most Recent Termination: Miscarriage" 2 "Most Recent Termination: Abortion" 3 "Most Recent Termination: Stillbirth"
label values termination termination
gen abortion=1 if termination==2
replace abortion=0 if termination==1 | termination==3

gen posB = strpos(vcal_1,"B")
replace posB=200 if posB==0
gen posT = strpos(vcal_1,"T")
replace posT=200 if posT==0

gen BorT= 1 if posB<posT
replace BorT= 2 if posB>posT
replace BorT=0 if posB==200 & posT==200


gen outcome=0 if BorT==0
replace outcome=1 if BorT==1
replace outcome=2 if BorT==2 & s230a==1
replace outcome=3 if BorT==2 & s230a==2
replace outcome=4 if BorT==2 & s230a==3

label define outcome 0 "No pregnancy outcome in calendar" 1 "Most Recent Pregnancy Outcome: Birth" 2 "Most Recent Pregnancy Outcome: Miscarriage" 3 "Most Recent Pregnancy Outcome: Abortion" 4 "Most Recent Pregnancy Outcome: Stillbirth"
label values outcome outcome


gen abortion_recent=1 if outcome==3
replace abortion_recent=0 if outcome==1 | outcome==2 | outcome==4
gen miscarriage_recent=1 if outcome==2
replace miscarriage_recent=0 if outcome==1 | outcome==3 | outcome==4


gen year=0

replace v001=v001 + 1000
replace v023=v023+100

append using "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIRcombo1.dta"

gen wt=v005/1000000
svyset [pw=wt], psu(v001) strata(v023)
svy: tab abortion year if v502==1 , ci



svy: logit abortion year if v502==1

svy: logit abortion_recent year if v502==1
svy: logit miscarriage_recent year if v502==1
