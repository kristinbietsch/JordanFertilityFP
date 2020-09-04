* EC USE and knowledge

* 2017
clear all
 use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR72FL.DTA"

 
  * just comparing jordanians to syrians
 gen syrian=1 if s123a==3
 replace syrian=0 if s123==1
 

 
* knowledge of EC
tab v304_16 [aw=v005/1000000] if v502==1

gen know_ec=1 if v304_16==1
replace know_ec=0 if v304_16==0


gen wt=v005/1000000

svyset [pw=wt], psu(v001) strata(v023)

svy: tab syrian know_ec if v502==1, row
tab v307_16 v312 if v502==1

svy: logit know_ec  syrian if v502==1

******************************************
clear all
set maxvar 10000
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR71FL.DTA"

gen year=1

gen know_ec=1 if v304_16==1
replace know_ec=0 if v304_16==0

save "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIRcombo.dta", replace


clear all
set maxvar 10000

use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"

gen year=0

gen know_ec=1 if v304_16==1
replace know_ec=0 if v304_16==0

replace v001=v001 + 1000
replace v023=v023+100

 

append using "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIRcombo.dta"

gen wt=v005/1000000


svyset [pw=wt], psu(v001) strata(v023) singleunit(certainty)

svy: tab year know_ec  if v502==1, row

svy: logit know_ec year if v502==1 

svy: tab v013 know_ec   if v502==1 & year==1, row
svy: tab v013 know_ec   if v502==1 & year==0, row
* increase in knowledge of EC from 15.3% to 27.3% among married women
	* increated in all age groups
	* only 1 women in 2017 reported currently using EC
	
**************************************************************
keep caseid vcal_1 v005 v008 v011 v017 v018 v021 v022 v023 year

* Step 6.1 
* loop through calendar creating separate variables for each month 
* total length of calendar to loop over including leading blanks (usually 80) 
local vcal_len = strlen(vcal_1[1]) 
forvalues i = 1/`vcal_len' {   
gen str1 method`i' = substr(vcal_1,`i',1) 
}

* Step 6.2 
* drop calendar string variable as we don't need it further 
drop vcal_1 
 
* reshape the data file into a file where the month is the unit of analysis 
reshape long method, i(caseid) j(i) 
	* only 4 reports of EC using in calendar
	
*********************************************************************
* do men know EC
clear all
set maxvar 10000
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOMR71FL.DTA" 

gen know_ec=1 if mv304_16==1
replace know_ec=0 if mv304_16==0

gen wt=mv005/1000000

svyset [pw=wt], psu(mv001) strata(mv023)

svy: tab mv502 know_ec , row

* lower knowledge among married men than married women
