* Percent of women who have achieved their desired family size

* percent of women currently pregnant

clear all
set maxvar 10000
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR71FL.DTA"

gen year=1

save "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIRcombo.dta", replace


clear all
set maxvar 10000

use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"

gen year=0

replace v001=v001 + 1000
replace v023=v023+100

append using "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIRcombo.dta"

gen wt=v005/1000000


svyset [pw=wt], psu(v001) strata(v023)
svy: tab v213 year if v502==1 , ci

svy: logit v213 year if v502==1


* percent of currently married women who want a child in the next 12 months
gen wants_year=1 if v604==0
replace wants_year=0 if  v604!=0
replace wants_year=0 if v602!=1

tab v604 wants_year , m
tab v602 wants_year , m
tab v502 wants_year , m

tab wants_year if v502==1 [aw=v005/1000000]
svy: logit wants_year year if v502==1


* time since marriage
gen time_marriage= (v008-v509)/12

gen marriage_group=1 if time_marriage<2
replace marriage_group=2 if time_marriage>=2 & time_marriage<5
replace marriage_group=3 if time_marriage>=5 & time_marriage<10
replace marriage_group=4 if time_marriage>=10 & time_marriage<15
replace marriage_group=5 if time_marriage>=15 & time_marriage!=.

label define marriage_group 1 "Under 2 Years" 2 "2-5 Years" 3 "5-10 Years" 4 "10-15 Years" 5 "Over 15 Years"
label values marriage_group marriage_group

svy: logit wants_year year if v502==1 & marriage_group==1
svy: logit wants_year year if v502==1 & marriage_group==2
svy: logit wants_year year if v502==1 & marriage_group==3
svy: logit wants_year year if v502==1 & marriage_group==4
svy: logit wants_year year if v502==1 & marriage_group==5

******************************************************
* Undecided about wanting another child
gen undecided=1 if v602==2
replace undecided=0 if v602!=2

svy: logit undecided year if v502==1 

gen wantanother=1 if v602==1
replace wantanother=0 if v602!=1

svy: logit wantanother year if v502==1 

***************************************************
* unmet need
gen unmet=1 if v626a==1 | v626==2
replace unmet=0 if v626a!=1 & v626a!=2
svy: logit unmet year if v502==1 

*************************************************
gen met_ideal=1 if v201>=v613
replace met_ideal=0 if v201<v613

svy: logit met_ideal year if v502==1 
svy: tab met_ideal year if v502==1, col

*************************************************
gen condom=1 if v312==5
replace condom=0 if v312!=5

svy: logit condom year if v502==1 

*************************************************
gen pa=1 if v312==8
replace pa=0 if v312!=8

svy: logit pa year if v502==1 

*************************************************
gen withdrawal=1 if v312==9
replace withdrawal=0 if v312!=9

svy: logit withdrawal year if v502==1 
*************************************************

svy: regress v012 year if v502==1 

*************************************************
gen g45=1 if v013==7
replace g45=0 if v013!=7

svy: logit g45 year if v502==1 

*************************************************
gen mar_2 = 1 if marriage_group==1
replace mar_2=0 if marriage_group!=1
svy: logit mar_2 year if v502==1 

gen mar_24 = 1 if marriage_group==2
replace mar_24=0 if marriage_group!=2
svy: logit mar_24 year if v502==1 

*************************************************
gen live_husband= 1 if v504==1
replace live_husband=0 if v504==2

svy: logit live_husband year if v502==1 

gen sex_month=1 if v529==0
replace sex_month=0 if v529!=0

svy: logit sex_month year if v502==1 

svy: logit sex_month year if v502==1 & live_husband==1


*************************************************
* infecund/menopausal by age
gen inf_meno=1 if v626a==9
replace inf_meno=0 if v626a!=9

svy: logit inf_meno year if v502==1 
svy: logit inf_meno year if v502==1 & v013==6
svy: logit inf_meno year if v502==1 & v013==4


*************************************************
gen ster=1 if v201==0
replace ster=0 if v201!=0

svy: logit inf_meno year if v013==7

*************************************************
gen nulliparous=1 if v201==0
replace nulliparous=0 if v201!=0

svy: logit nulliparous year if v502==1 

gen parity_1=1 if v201==1
replace parity_1=0 if v201!=1

svy: logit parity_1 year if v502==1 

gen parity_2=1 if v201==1
replace parity_2=0 if v201!=1

svy: logit parity_2 year if v502==1 


svy: tab v613 year if v502==1 , col

gen wants_children=1 if v613>0
replace wants_children=0 if v613==0

svy: logit wants_children year if v502==1 

svy: tab wants_children year if v502==1 & nulliparous==1 , col
svy: logit wants_children year if v502==1 & nulliparous==1 
************************************************
gen cpr=1 if v313!=0
replace cpr=0 if v313==0

svy: logit cpr year if v502==1 
svy: logit cpr year i.v013 if v502==1 
svy: logit cpr year i.v013  i.marriage_group  if v502==1 & live_husband==1 & sex_month==1
svy: logit cpr year i.v013  i.marriage_group  if v502==1 & live_husband==1 & sex_month==1 & inf_meno==0

svy: tab v201 year, col
************************************************



*************************************************
*************************************************
*************************************************
*************************************************
*************************************************
*************************************************
clear all

 use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOPR71FL.DTA"
  * keep women 15-49
keep if hv104==2
keep if hv105>=15 & hv105<50 

gen married=1 if hv116==1
replace married=0 if hv116!=1

gen age=1 if hv105>=15 & hv105<20
replace age=2 if hv105>=20 & hv105<25
replace age=3 if hv105>=25 & hv105<30
replace age=4 if hv105>=30 & hv105<35
replace age=5 if hv105>=35 & hv105<40
replace age=6 if hv105>=40 & hv105<45
replace age=7 if hv105>=45 & hv105<50

gen nationality = 1 if sh07a==1
replace nationality=2 if sh07a==3
replace nationality=3 if sh07a==2 | sh07a==4 | sh07a==5 | sh07a==6 | sh07a==8
 
 label define nationality 1 "Jordan" 2 "Syrian" 3 "Other Nationalities" 
label values nationality nationality

gen year=1


 save "C:\Users\KBietsch\Files\DHS Data\Jordan\JOPRcombo.dta", replace

 
 clear all
set maxvar 10000

use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOPR6CFL.DTA"

 * keep women 15-49
keep if hv104==2
keep if hv105>=15 & hv105<50 

gen married=1 if hv116==1
replace married=0 if hv116!=1

gen age=1 if hv105>=15 & hv105<20
replace age=2 if hv105>=20 & hv105<25
replace age=3 if hv105>=25 & hv105<30
replace age=4 if hv105>=30 & hv105<35
replace age=5 if hv105>=35 & hv105<40
replace age=6 if hv105>=40 & hv105<45
replace age=7 if hv105>=45 & hv105<50

gen nationality = 1 if sh07==1
replace nationality=2 if sh07==3
replace nationality=3 if sh07==2 | sh07==4 | sh07==5 | sh07==6 | sh07==8
 
 label define nationality 1 "Jordan" 2 "Syrian" 3 "Other Nationalities" 
label values nationality nationality


gen year=0

replace hv001=hv001 + 1000
replace hv023=hv023+100

append using "C:\Users\KBietsch\Files\DHS Data\Jordan\JOPRcombo.dta"

gen wt=hv005/1000000


svyset [pw=wt], psu(hv001) strata(hv023)
svy: logit married year


svy: logit married year if age==1

