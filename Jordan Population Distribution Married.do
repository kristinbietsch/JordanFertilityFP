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

tab married [aw=hv005/1000000]

tab age [aw=hv005/1000000]
tab age if married==1 [aw=hv005/1000000]
tab age  married [aw=hv005/1000000], row nofreq

tab age nationality  [aw=hv005/1000000], col nofreq
tab   married nationality if age==1 [aw=hv005/1000000], col nofreq
tab   married nationality if age==2 [aw=hv005/1000000], col nofreq
tab   married nationality if age==3 [aw=hv005/1000000], col nofreq
tab   married nationality if age==4 [aw=hv005/1000000], col nofreq
tab   married nationality if age==5 [aw=hv005/1000000], col nofreq
tab   married nationality if age==6 [aw=hv005/1000000], col nofreq
tab   married nationality if age==7 [aw=hv005/1000000], col nofreq


tab    age  nationality if married==1 [aw=hv005/1000000], col nofreq


*******************************************************
clear all

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

tab married [aw=hv005/1000000]


tab age [aw=hv005/1000000]
tab age if married==1 [aw=hv005/1000000]
tab age  married [aw=hv005/1000000], row nofreq



tab age nationality  [aw=hv005/1000000], col nofreq
tab   married nationality if age==1 [aw=hv005/1000000], col nofreq
tab   married nationality if age==2 [aw=hv005/1000000], col nofreq
tab   married nationality if age==3 [aw=hv005/1000000], col nofreq
tab   married nationality if age==4 [aw=hv005/1000000], col nofreq
tab   married nationality if age==5 [aw=hv005/1000000], col nofreq
tab   married nationality if age==6 [aw=hv005/1000000], col nofreq
tab   married nationality if age==7 [aw=hv005/1000000], col nofreq

tab    age  nationality if married==1 [aw=hv005/1000000], col nofreq

************************************************************
clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR71FL.DTA"

* time since marriage
gen time_marriage= (v008-v509)/12

gen marriage_group=1 if time_marriage<2
replace marriage_group=2 if time_marriage>=2 & time_marriage<5
replace marriage_group=3 if time_marriage>=5 & time_marriage<10
replace marriage_group=4 if time_marriage>=10 & time_marriage<15
replace marriage_group=5 if time_marriage>=15 & time_marriage!=.

label define marriage_group 1 "Under 2 Years" 2 "2-5 Years" 3 "5-10 Years" 4 "10-15 Years" 5 "Over 15 Years"
label values marriage_group marriage_group

tab marriage_group if v502==1 [aw=v005/1000000]

************************************************************
clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"

* time since marriage
gen time_marriage= (v008-v509)/12

gen marriage_group=1 if time_marriage<2
replace marriage_group=2 if time_marriage>=2 & time_marriage<5
replace marriage_group=3 if time_marriage>=5 & time_marriage<10
replace marriage_group=4 if time_marriage>=10 & time_marriage<15
replace marriage_group=5 if time_marriage>=15 & time_marriage!=.

label define marriage_group 1 "Under 2 Years" 2 "2-5 Years" 3 "5-10 Years" 4 "10-15 Years" 5 "Over 15 Years"
label values marriage_group marriage_group

tab marriage_group if v502==1 [aw=v005/1000000]

*************************************************************
* ethnic breakdown of married women
clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOPR6CFL.DTA"

keep if hv104==2
keep if hv105>=15 & hv105<50 
keep if hv116!=0

keep hv000 hv001 hv002 hvidx sh07

rename hv000 v000
rename hv001 v001
rename hv002 v002
rename hvidx v003

save "C:\Users\KBietsch\Files\DHS Data\Jordan\nationality2012.DTA", replace

clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"

merge 1:1 v000 v001 v002 v003 using "C:\Users\KBietsch\Files\DHS Data\Jordan\nationality2012.DTA"

*numlabel, add
tab sh07

gen nationality = 1 if sh07==1
replace nationality=2 if sh07==3
replace nationality=3 if sh07==2 | sh07==4 | sh07==5 | sh07==6
 replace nationality=4 if sh07==8
 
 label define nationality 1 "Jordan" 2 "Syrian" 3 "Other Nationalities" 
label values nationality nationality


tab nationality if v502==1 [aw=v005/1000000]

************************************************************
clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR71FL.DTA"
tab snat if v502==1 [aw=v005/1000000]
