* Desire to Limit by Parity


* 2017
clear all
 use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR72FL.DTA"
gen parity=v201
replace parity=6 if v201>=6

gen limit=1 if v605==5 | v605==6
replace limit=0 if v605==1 | v605==2 | v605==3 | v605==4 | v605==7 


* desire to limit among currently married
tab parity limit  if v502==1 [aw=v005/1000000], row nofreq

tab parity limit  if v502==1 & s123a==1 [aw=v005/1000000], row nofreq
tab parity limit  if v502==1 & s123a==3 [aw=v005/1000000], row nofreq


********************************************************************************
* 2012 
 clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"
merge 1:1 v000 v001 v002 v003 using "C:\Users\KBietsch\Files\DHS Data\Jordan\nationality2012.DTA"
keep if _merge==3


gen parity=v201
replace parity=6 if v201>=6

gen limit=1 if v605==5 | v605==6
replace limit=0 if v605==1 | v605==2 | v605==3 | v605==4 | v605==7 

* desire to limit among currently married
tab parity limit  if v502==1 [aw=v005/1000000], row nofreq

tab parity limit  if v502==1 & sh07==1 [aw=v005/1000000], row nofreq
tab parity limit  if v502==1 & sh07==3  [aw=v005/1000000], row nofreq

********************************************************************************
* 2009
 clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR61FL.DTA"

gen parity=v201
replace parity=6 if v201>=6

gen limit=1 if v605==5 | v605==6
replace limit=0 if v605==1 | v605==2 | v605==3 | v605==4 | v605==7 


* desire to limit among currently married
tab parity limit  if v502==1 [aw=v005/1000000], row nofreq


********************************************************************************
* 2007
clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR51FL.DTA"

gen parity=v201
replace parity=6 if v201>=6

gen limit=1 if v605==5 | v605==6
replace limit=0 if v605==1 | v605==2 | v605==3 | v605==4 | v605==7 


* desire to limit among currently married
tab parity limit  if v502==1 [aw=v005/1000000], row nofreq
