clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR71FL.DTA"


* Step 6.5 
* compute weight variable 
gen wt=v005/1000000 
  
* set up the svy paramters and calculate the mean of usingany (which is the CPR) 
svyset v021 [pweight=wt], strata(v023) 

gen modern=1 if v313==3
replace modern=0 if v313!=3

gen cpr=1 if v313!=0
replace cpr=0 if v313==0

 svy: tab modern, ci
 svy: tab cpr, ci
 

keep caseid vcal_1 v005 v008 v011 v017 v018 v021 v022 v023 

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


* Step 6.3 
* find the position of the earliest date of interview (the maximum value of v018) 
egen v018_max = max(v018) 
 
* drop cases outside of the five years preceding the earliest interview 
* months 0-59 before the earliest interview date 
keep if inrange(i,v018_max,v018_max+59) 

* Step 6.4 
* calculate age in months for each month in the calendar 
gen agem = (v008 - v011) - (i - v018) 
 
* calculate century month code for each month 
gen cmctime = v008 - (i - v018) 
label variable cmctime "Century month code" 
 
* create variable for use of any method as a 0/100 variable 
gen usingany = !inlist(method, "0","B","P","T") * 100 
label variable usingany "Using any method" 
label def usingany 0 "Not using" 100 "Using a method" 
label val usingany usingany 

* create variable for use of any method as a 0/100 variable 
gen usingmodern = 100 if method=="1" | method=="2" | method=="3" | method=="4" | method=="5" | method=="6" | method=="7" | method=="N" | method=="L" | method=="C" | method=="F" | method=="E" | method=="S" | method=="M"  
replace usingmodern=0 if usingmodern==.
label variable usingmodern "Using modern method" 
label def usingmodern 0 "Not using" 100 "Using a modern method" 
label val usingmodern usingmodern 


* Step 6.5 
* compute weight variable 
gen wt=v005/1000000 
  
* set up the svy paramters and calculate the mean of usingany (which is the CPR) 
svyset v021 [pweight=wt], strata(v023) 
 
* tabulate CPR for women 15-44 
svy, subpop(if inrange(agem,180,539)): mean usingany, over(cmctime) nolegend 

* tabulate mCPR for women 15-44 
svy, subpop(if inrange(agem,180,539)): mean usingmodern, over(cmctime) nolegend 
