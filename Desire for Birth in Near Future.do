* Jordan 2017
clear all
set maxvar 10000
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR71FL.DTA"

* percent of currently married women who want a child in the next 12 months
gen wants_year=1 if v604==0
replace wants_year=0 if  v604!=0
replace wants_year=0 if v602!=1

tab v604 wants_year , m
tab v602 wants_year , m
tab v502 wants_year , m

tab wants_year if v502==1 [aw=v005/1000000]


* wants another birth
tab v602 if  v502==1 [aw=v005/1000000]

* time since marriage
gen time_marriage= (v008-v509)/12

gen marriage_group=1 if time_marriage<2
replace marriage_group=2 if time_marriage>=2 & time_marriage<5
replace marriage_group=3 if time_marriage>=5 & time_marriage<10
replace marriage_group=4 if time_marriage>=10 & time_marriage<15
replace marriage_group=5 if time_marriage>=15 & time_marriage!=.

label define marriage_group 1 "Under 2 Years" 2 "2-5 Years" 3 "5-10 Years" 4 "10-15 Years" 5 "Over 15 Years"
label values marriage_group marriage_group

tab marriage_group wants_year  if v502==1 [aw=v005/1000000], row nofreq



* Jordan 2012
clear all
set maxvar 10000

use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"

* percent of currently married women who want a child in the next 12 months
gen wants_year=1 if v604==0
replace wants_year=0 if  v604!=0
replace wants_year=0 if v602!=1

tab v604 wants_year , m
tab v602 wants_year , m
tab v502 wants_year , m

tab wants_year if v502==1 [aw=v005/1000000]


* wants another birth
tab v602 if v502==1 [aw=v005/1000000]


* time since marriage
gen time_marriage= (v008-v509)/12

gen marriage_group=1 if time_marriage<2
replace marriage_group=2 if time_marriage>=2 & time_marriage<5
replace marriage_group=3 if time_marriage>=5 & time_marriage<10
replace marriage_group=4 if time_marriage>=10 & time_marriage<15
replace marriage_group=5 if time_marriage>=15 & time_marriage!=.

label define marriage_group 1 "Under 2 Years" 2 "2-5 Years" 3 "5-10 Years" 4 "10-15 Years" 5 "Over 15 Years"
label values marriage_group marriage_group

tab marriage_group wants_year  if v502==1 [aw=v005/1000000], row nofreq

