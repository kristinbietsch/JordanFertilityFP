* Unmet need by nationality

* 2017
clear all
 use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR72FL.DTA"
 
 * just comparing jordanians to syrians
 gen syrian=1 if s123a==3
 replace syrian=0 if s123==1
 
 gen unmet=1 if v626a==1 | v626a==2
 replace unmet=0 if v626a!=1 & v626a!=2
 
 
gen wt=v005/1000000

svyset [pw=wt], psu(v001) strata(v023)

svy: tab syrian unmet if v502==1, row

svy: logit unmet  syrian if v502==1
* syrians have higher unmet need
svy: tab unmet   if v502==1

*******************************************************************
 clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"
merge 1:1 v000 v001 v002 v003 using "C:\Users\KBietsch\Files\DHS Data\Jordan\nationality2012.DTA"
keep if _merge==3

 gen unmet=1 if v626a==1 | v626a==2
 replace unmet=0 if v626a!=1 & v626a!=2

 
 * just comparing jordanians to syrians
 gen syrian=1 if sh07==3
 replace syrian=0 if sh07==1
 
 gen wt=v005/1000000

 svyset [pw=wt], psu(v001) strata(v023)

svy: tab syrian unmet if v502==1, row

svy: logit unmet  syrian if v502==1

******************************************************

clear all
set maxvar 10000
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR71FL.DTA"

gen year=1

 gen syrian=1 if s123a==3
 replace syrian=0 if s123==1
 
 gen unmet=1 if v626a==1 | v626a==2
 replace unmet=0 if v626a!=1 & v626a!=2

save "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIRcombo.dta", replace


clear all
set maxvar 10000

use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"
merge 1:1 v000 v001 v002 v003 using "C:\Users\KBietsch\Files\DHS Data\Jordan\nationality2012.DTA"
keep if _merge==3

gen year=0

replace v001=v001 + 1000
replace v023=v023+100

 gen unmet=1 if v626a==1 | v626a==2
 replace unmet=0 if v626a!=1 & v626a!=2

 
 * just comparing jordanians to syrians
 gen syrian=1 if sh07==3
 replace syrian=0 if sh07==1

append using "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIRcombo.dta"

gen wt=v005/1000000


svyset [pw=wt], psu(v001) strata(v023) singleunit(certainty)
svy: tab v213 year if v502==1 , ci

svy: tab syrian unmet  if v502==1 & year==1, row
svy: tab syrian unmet  if v502==1 & year==0, row 
svy: tab year unmet  if v502==1, row

svy: tab  v626a  year if v502==1, col


svy: logit unmet year if v502==1 


svy: logit unmet syrian if v502==1 & year==1
svy: logit unmet syrian if v502==1 & year==0


svy: logit unmet year if v502==1 & syrian==1
svy: logit unmet year if v502==1 & syrian==0

* unmet need did not signifcantly increase among syrians, but did among jordanians
