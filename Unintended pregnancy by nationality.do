* Preportions of Pregnancies unintended by nationality

* 2017
clear all
 use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR72FL.DTA"
 
 * just comparing jordanians to syrians
 gen syrian=1 if s123a==3
 replace syrian=0 if s123==1
 
 
tab v225 syrian
gen unintended=1 if v225==2 | v225==3
replace unintended=0 if v225==1

gen wanted_last=1 if v367==2 | v367==3
replace wanted_last=0 if v367==1

gen wt=v005/1000000

svyset [pw=wt], psu(v001) strata(v023)

svy: tab syrian v225 if v502==1, row

svy: logit unintended  syrian if v502==1
* no statistical different among currenly pregnant in wantedness by nationality
svy: logit wanted_last  syrian if v502==1
* no statistical different among women in wantedness of last birth by nationality
*******************************************************************
 clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"
merge 1:1 v000 v001 v002 v003 using "C:\Users\KBietsch\Files\DHS Data\Jordan\nationality2012.DTA"
keep if _merge==3

gen unintended=1 if v225==2 | v225==3
replace unintended=0 if v225==1

gen wanted_last=1 if v367==2 | v367==3
replace wanted_last=0 if v367==1
 
 * just comparing jordanians to syrians
 gen syrian=1 if sh07==3
 replace syrian=0 if sh07==1
 
 gen wt=v005/1000000

 svyset [pw=wt], psu(v001) strata(v023)

svy: tab syrian v225 if v502==1, row

svy: logit unintended  syrian if v502==1
* no statistical different among currenly pregnant in wantedness by nationality

svy: logit wanted_last  syrian if v502==1
* no statistical different among women in wantedness of last birth by nationality

******************************************************

clear all
set maxvar 10000
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR71FL.DTA"

gen year=1

 gen syrian=1 if s123a==3
 replace syrian=0 if s123==1
 
gen unintended=1 if v225==2 | v225==3
replace unintended=0 if v225==1

gen wanted_last=1 if v367==2 | v367==3
replace wanted_last=0 if v367==1

save "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIRcombo.dta", replace


clear all
set maxvar 10000

use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"
merge 1:1 v000 v001 v002 v003 using "C:\Users\KBietsch\Files\DHS Data\Jordan\nationality2012.DTA"
keep if _merge==3

gen year=0

replace v001=v001 + 1000
replace v023=v023+100

gen unintended=1 if v225==2 | v225==3
replace unintended=0 if v225==1

gen wanted_last=1 if v367==2 | v367==3
replace wanted_last=0 if v367==1

 
 * just comparing jordanians to syrians
 gen syrian=1 if sh07==3
 replace syrian=0 if sh07==1

append using "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIRcombo.dta"

gen wt=v005/1000000


svyset [pw=wt], psu(v001) strata(v023) singleunit(certainty)

svy: tab unintended year if v502==1, col
svy: tab wanted_last year if v502==1, col

svy: logit unintended year if v502==1 
svy: logit unintended year if v502==1 & syrian==1
svy: logit unintended year if v502==1 & syrian==0

svy: logit wanted_last year if v502==1 
svy: logit wanted_last year if v502==1 & syrian==1
svy: logit wanted_last year if v502==1 & syrian==0

* THERE IS A DECLINE IN THE % OF PREGNANCIES WANTED LATER OR NOT AT ALL
* WHILE TFR DECLINED BY 0.8, WTFR DECLINED BY ONLY 0.3
* IN FACT, THE WTF IN 2012 WAS 2.5, NOW 2.2
* THE GAP FROM TFR TO WTFR DECLINED BY 0.5

* HIGHER MOTIVATION TO AVOID UNWANTED PREGNANCIES????
