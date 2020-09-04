* desired number of children by nationality and sex

* 2017
clear all
 use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR72FL.DTA"

 tab v613 s123a if v502==1
 tab s123a  [aw=v005/1000000]
 tab v613 if v502==1 [aw=v005/1000000]
 * modal ideal number of children for both jordanians and syrians is 4
 
 gen ideal=.
 replace ideal=v613 if v613!=96
 mean ideal if s123a==1 [aw=v005]
 mean ideal if s123a==3 [aw=v005]
 
 * just comparing jordanians to syrians
 gen syrian=1 if s123a==3
 replace syrian=0 if s123==1
 
 gen wt=v005/1000000

 svyset [pw=wt], psu(v001) strata(v023)

svy: glm ideal  syrian
 
* ideal number of children is higher for syrians, but not very much 
 ********************************************************************************

 
 ********************************************************************************
* 2012 
 clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"
merge 1:1 v000 v001 v002 v003 using "C:\Users\KBietsch\Files\DHS Data\Jordan\nationality2012.DTA"
keep if _merge==3


tab sh07 [aw=v005/1000000]

numlabel, add
 tab  v613 sh07 if v502==1 [aw=v005/1000000], col nofreq


 gen ideal=.
 replace ideal=v613 if v613!=96 & v613!=95
 mean ideal if sh07==1 [aw=v005]
 mean ideal if sh07==3 [aw=v005]
 
 * just comparing jordanians to syrians
 gen syrian=1 if sh07==3
 replace syrian=0 if sh07==1
 
 gen wt=v005/1000000

 svyset [pw=wt], psu(v001) strata(v023)

svy: glm ideal  syrian
 
* no significant different between jordanians and syrians



 ********************************************************************************
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

 gen ideal=.
 replace ideal=v613 if v613!=96 & v613!=95

svyset [pw=wt], psu(v001) strata(v023)
svy: glm ideal year if v502==1

* no change in ideal number of children

 ********************************************************************************
clear all
set maxvar 10000
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOMR71FL.DTA" 

* Of ever married men
drop if mv502==0

 tab mv613  sm123a

 * modal ideal number of children for both jordanians and syrians is 4
 
 gen ideal=.
 replace ideal=mv613 if mv613!=96
 mean ideal if sm123a==1 [aw=mv005]
 mean ideal if sm123a==3 [aw=mv005]
 
 * just comparing jordanians to syrians
 gen syrian=1 if sm123a==3
 replace syrian=0 if sm123==1
 
 gen wt=mv005/1000000

 svyset [pw=wt], psu(mv001) strata(mv023)

svy: glm ideal  syrian
* while there is a 5 child different, it is not statistically significant

* No men in 2012
