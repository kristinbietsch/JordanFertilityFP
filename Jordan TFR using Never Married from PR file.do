clear all 
set more off
set maxvar 10000
* pr recode
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOPR71FL.DTA"

* women
keep if hv104==2
* 15-49
keep if hv105>=15 & hv105<=49
* never married
keep if hv116==0

* there is no CMC- going to assume everyone is halfway through year
gen v011 = hv008-((12*hv105) + 6)
gen b3_01=.
gen b3_02=.
gen b3_03=.
gen b3_04=.
gen b3_05=.
gen b3_06=.
gen b3_07=.
gen b3_08=.
gen b3_09=.
gen b3_10=.
gen b3_11=.
gen b3_12=.
gen b3_13=.
gen b3_14=.
gen b3_15=.
gen b3_16=.
gen b3_17=.
gen b3_18=.
gen b3_19=.
gen b3_20=.

* create variables
rename hv008 v008
rename hv005 v005
rename sh07a s123a

keep v005 v008 v011 b3_* s123a
save "C:\Users\KBietsch\Files\DHS Data\Jordan\nevermarried_Jordan2017.dta", replace



* womens recode
clear all 
set more off
set maxvar 10000

use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR72FL.DTA"
keep v005 v008 v011 b3_* s123a

append using "C:\Users\KBietsch\Files\DHS Data\Jordan\nevermarried_Jordan2017.dta"

//delineating the interval of time for which we'll be calculating asfr
//exposure starts 36 months ago (or at age 15 whichever is latest) until the last competed month prior to the date of the survey
gen top = v008-1
gen bot = v008-36
gen turn15 = v011 + 180
replace bot = turn15 if turn15 > bot 	//exposure starts at age 15 (btw: could we have started at an earlier age as well?)
drop if turn15 ==  v008			// 15 on month of interview

//calculating age at the beginning and end of the exposure interval 
gen agebot = floor((bot-v011)/12)		//we use floor function because we want age at last birthday (i.e., completed years)
gen agetop = floor((top-v011)/12) 

//this gives us one record per woman with the ages of exposure in the 36 months interval prior to the survey
gen nages = agetop-agebot+1			//nrber of ages of exposure 
gen id = _n						//creates an id number for each woman


expand nages					//<expand> creates as # records per equal to the value of nages

//not we have nrages x records for each woman
//let's construct 1 record per age per woman
bysort id: gen age = agebot + _n - 1	//calculate age for each record of each woman 

//one record per women for each age in the 36-month interval 
//now we can fix the start and end date of each record or segment. A segment starts at bot or at a birthday, and ends a year later or at top. 

gen bday = v011 + 12*age  //the months (coded in CMC) in which a respondent celebrates her birthday during the three-year exposure
replace bot = bday if bday > bot
replace top = bday+11 if bday+11 < top
gen expo = top-bot+1 				// the exposure at each age (in months) in the 36months interval 


gen births = 0
forvalues i=1/9 {				
    qui replace births = births+1 if b3_0`i' >= bot & b3_0`i' <= top
    }


// now we can aggregate exposure times and births keeping the distinction between 
// note also that the DHS works with sampling weights (v005), so let's take them into account when aggregating 
gen w=v005/1000000      //weights need to be divided by 1000000

collapse (sum) births (sum) expo [pweight=w], by(age s123a)   //in the by() statement you could also include other variables that you wish to retain in the aggregated dataset (e.g., educational status)


//now we can calculate age-specific fertility rates
gen asfr =births /(expo/12)
list
//and the TFR
quietly sum asfr if s123a==1
dis "Jordanian TFR="r(sum)

quietly sum asfr if s123a==3
dis "Syrian TFR="r(sum)

*************************************************************************************
* non-breakdown by nationality
clear all 
set more off
set maxvar 10000

use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR72FL.DTA"
keep v005 v008 v011 b3_* s123a

append using "C:\Users\KBietsch\Files\DHS Data\Jordan\nevermarried_Jordan2017.dta"

//delineating the interval of time for which we'll be calculating asfr
//exposure starts 36 months ago (or at age 15 whichever is latest) until the last competed month prior to the date of the survey
gen top = v008-1
gen bot = v008-36
gen turn15 = v011 + 180
replace bot = turn15 if turn15 > bot 	//exposure starts at age 15 (btw: could we have started at an earlier age as well?)
drop if turn15 ==  v008			// 15 on month of interview

//calculating age at the beginning and end of the exposure interval 
gen agebot = floor((bot-v011)/12)		//we use floor function because we want age at last birthday (i.e., completed years)
gen agetop = floor((top-v011)/12) 

//this gives us one record per woman with the ages of exposure in the 36 months interval prior to the survey
gen nages = agetop-agebot+1			//nrber of ages of exposure 
gen id = _n						//creates an id number for each woman


expand nages					//<expand> creates as # records per equal to the value of nages

//not we have nrages x records for each woman
//let's construct 1 record per age per woman
bysort id: gen age = agebot + _n - 1	//calculate age for each record of each woman 

//one record per women for each age in the 36-month interval 
//now we can fix the start and end date of each record or segment. A segment starts at bot or at a birthday, and ends a year later or at top. 

gen bday = v011 + 12*age  //the months (coded in CMC) in which a respondent celebrates her birthday during the three-year exposure
replace bot = bday if bday > bot
replace top = bday+11 if bday+11 < top
gen expo = top-bot+1 				// the exposure at each age (in months) in the 36months interval 


gen births = 0
forvalues i=1/9 {				
    qui replace births = births+1 if b3_0`i' >= bot & b3_0`i' <= top
    }


// now we can aggregate exposure times and births keeping the distinction between 
// note also that the DHS works with sampling weights (v005), so let's take them into account when aggregating 
gen w=v005/1000000      //weights need to be divided by 1000000

collapse (sum) births (sum) expo [pweight=w], by(age)   //in the by() statement you could also include other variables that you wish to retain in the aggregated dataset (e.g., educational status)


//now we can calculate age-specific fertility rates
gen asfr =births /(expo/12)
list
//and the TFR
quietly sum asfr 
dis "TFR="r(sum)


*********************************************************************************************************************************
* 2012
clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOPR6CFL.DTA"
* jordanian 1, syrian 3 sh07

* women
keep if hv104==2
* 15-49
keep if hv105>=15 & hv105<=49
* never married
keep if hv116==0

* there is no CMC- going to assume everyone is halfway through year
gen v011 = hv008-((12*hv105) + 6)
gen b3_01=.
gen b3_02=.
gen b3_03=.
gen b3_04=.
gen b3_05=.
gen b3_06=.
gen b3_07=.
gen b3_08=.
gen b3_09=.
gen b3_10=.
gen b3_11=.
gen b3_12=.
gen b3_13=.
gen b3_14=.
gen b3_15=.
gen b3_16=.
gen b3_17=.
gen b3_18=.
gen b3_19=.
gen b3_20=.

* create variables
rename hv008 v008
rename hv005 v005


keep v005 v008 v011 b3_* sh07
save "C:\Users\KBietsch\Files\DHS Data\Jordan\nevermarried_Jordan2012.dta", replace


*******

clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA" 
* merge in nationality for married women (it isnt the women's recode)
merge 1:1 v000 v001 v002 v003 using "C:\Users\KBietsch\Files\DHS Data\Jordan\nationality2012.DTA"
keep if _merge==3

keep v005 v008 v011 b3_* sh07

append using "C:\Users\KBietsch\Files\DHS Data\Jordan\nevermarried_Jordan2012.dta"

//delineating the interval of time for which we'll be calculating asfr
//exposure starts 36 months ago (or at age 15 whichever is latest) until the last competed month prior to the date of the survey
gen top = v008-1
gen bot = v008-36
gen turn15 = v011 + 180
replace bot = turn15 if turn15 > bot 	//exposure starts at age 15 (btw: could we have started at an earlier age as well?)
drop if turn15 ==  v008			// 15 on month of interview

//calculating age at the beginning and end of the exposure interval 
gen agebot = floor((bot-v011)/12)		//we use floor function because we want age at last birthday (i.e., completed years)
gen agetop = floor((top-v011)/12) 

//this gives us one record per woman with the ages of exposure in the 36 months interval prior to the survey
gen nages = agetop-agebot+1			//nrber of ages of exposure 
gen id = _n						//creates an id number for each woman


expand nages					//<expand> creates as # records per equal to the value of nages

//not we have nrages x records for each woman
//let's construct 1 record per age per woman
bysort id: gen age = agebot + _n - 1	//calculate age for each record of each woman 

//one record per women for each age in the 36-month interval 
//now we can fix the start and end date of each record or segment. A segment starts at bot or at a birthday, and ends a year later or at top. 

gen bday = v011 + 12*age  //the months (coded in CMC) in which a respondent celebrates her birthday during the three-year exposure
replace bot = bday if bday > bot
replace top = bday+11 if bday+11 < top
gen expo = top-bot+1 				// the exposure at each age (in months) in the 36months interval 


gen births = 0
forvalues i=1/9 {				
    qui replace births = births+1 if b3_0`i' >= bot & b3_0`i' <= top
    }


// now we can aggregate exposure times and births keeping the distinction between 
// note also that the DHS works with sampling weights (v005), so let's take them into account when aggregating 
gen w=v005/1000000      //weights need to be divided by 1000000

collapse (sum) births (sum) expo [pweight=w], by(age sh07)   //in the by() statement you could also include other variables that you wish to retain in the aggregated dataset (e.g., educational status)


//now we can calculate age-specific fertility rates
gen asfr =births /(expo/12)
list
//and the TFR
quietly sum asfr if sh07==1
dis "Jordanian TFR="r(sum)

quietly sum asfr if sh07==3
dis "Syrian TFR="r(sum)

****************************************************************************************
** TFR not by ethnicity
clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA" 
* merge in nationality for married women (it isnt the women's recode)
merge 1:1 v000 v001 v002 v003 using "C:\Users\KBietsch\Files\DHS Data\Jordan\nationality2012.DTA"
keep if _merge==3

keep v005 v008 v011 b3_* sh07

append using "C:\Users\KBietsch\Files\DHS Data\Jordan\nevermarried_Jordan2012.dta"

//delineating the interval of time for which we'll be calculating asfr
//exposure starts 36 months ago (or at age 15 whichever is latest) until the last competed month prior to the date of the survey
gen top = v008-1
gen bot = v008-36
gen turn15 = v011 + 180
replace bot = turn15 if turn15 > bot 	//exposure starts at age 15 (btw: could we have started at an earlier age as well?)
drop if turn15 ==  v008			// 15 on month of interview

//calculating age at the beginning and end of the exposure interval 
gen agebot = floor((bot-v011)/12)		//we use floor function because we want age at last birthday (i.e., completed years)
gen agetop = floor((top-v011)/12) 

//this gives us one record per woman with the ages of exposure in the 36 months interval prior to the survey
gen nages = agetop-agebot+1			//nrber of ages of exposure 
gen id = _n						//creates an id number for each woman


expand nages					//<expand> creates as # records per equal to the value of nages

//not we have nrages x records for each woman
//let's construct 1 record per age per woman
bysort id: gen age = agebot + _n - 1	//calculate age for each record of each woman 

//one record per women for each age in the 36-month interval 
//now we can fix the start and end date of each record or segment. A segment starts at bot or at a birthday, and ends a year later or at top. 

gen bday = v011 + 12*age  //the months (coded in CMC) in which a respondent celebrates her birthday during the three-year exposure
replace bot = bday if bday > bot
replace top = bday+11 if bday+11 < top
gen expo = top-bot+1 				// the exposure at each age (in months) in the 36months interval 


gen births = 0
forvalues i=1/9 {				
    qui replace births = births+1 if b3_0`i' >= bot & b3_0`i' <= top
    }


// now we can aggregate exposure times and births keeping the distinction between 
// note also that the DHS works with sampling weights (v005), so let's take them into account when aggregating 
gen w=v005/1000000      //weights need to be divided by 1000000

collapse (sum) births (sum) expo [pweight=w], by(age )   //in the by() statement you could also include other variables that you wish to retain in the aggregated dataset (e.g., educational status)


//now we can calculate age-specific fertility rates
gen asfr =births /(expo/12)
list
//and the TFR
quietly sum asfr 
dis "TFR="r(sum)




