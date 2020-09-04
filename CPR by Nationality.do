* Preportions of Pregnancies unintended by nationality

* 2017
clear all
 use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR72FL.DTA"
 
 * just comparing jordanians to syrians
 gen syrian=1 if s123a==3
 replace syrian=0 if s123==1
 
 
tab v225 syrian

gen cpr=1 if v312!=0
replace cpr=0 if v312==0

gen wt=v005/1000000

svyset [pw=wt], psu(v001) strata(v023)

svy: tab syrian cpr if v502==1, row

svy: logit cpr  syrian if v502==1
* Syrians have lower CPR
*******************************************************************
 clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"
merge 1:1 v000 v001 v002 v003 using "C:\Users\KBietsch\Files\DHS Data\Jordan\nationality2012.DTA"
keep if _merge==3

gen cpr=1 if v312!=0
replace cpr=0 if v312==0
 
 * just comparing jordanians to syrians
 gen syrian=1 if sh07==3
 replace syrian=0 if sh07==1
 
 gen wt=v005/1000000

 svyset [pw=wt], psu(v001) strata(v023)

svy: tab syrian cpr if v502==1, row

svy: logit cpr  syrian if v502==1
* was lower, but not statsitically different

******************************************************

clear all
set maxvar 10000
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR71FL.DTA"

gen year=1

 gen syrian=1 if s123a==3
 replace syrian=0 if s123==1
 
gen cpr=1 if v312!=0
replace cpr=0 if v312==0

save "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIRcombo.dta", replace


clear all
set maxvar 10000

use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"
merge 1:1 v000 v001 v002 v003 using "C:\Users\KBietsch\Files\DHS Data\Jordan\nationality2012.DTA"
keep if _merge==3

gen year=0

replace v001=v001 + 1000
replace v023=v023+100

gen cpr=1 if v312!=0
replace cpr=0 if v312==0

 
 * just comparing jordanians to syrians
 gen syrian=1 if sh07==3
 replace syrian=0 if sh07==1

append using "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIRcombo.dta"

gen wt=v005/1000000


svyset [pw=wt], psu(v001) strata(v023) singleunit(certainty)

svy: tab year cpr  if v502==1, row

svy: tab year cpr  if v502==1 & syrian==0, row
svy: tab year cpr  if v502==1 & syrian==1, row


svy: logit cpr year if v502==1 
svy: logit cpr year if v502==1 & syrian==1
svy: logit cpr year if v502==1 & syrian==0

* the decline for syrians is not statistically significant
* the decline for jordanians is statisically significant- went down by 9 percentage points

svy: logit cpr year syrian if v502==1 
