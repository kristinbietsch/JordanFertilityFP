************************************************************
clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR71FL.DTA"

tab v504 if v502==1 [aw=v005/1000000]

gen sex_month=1 if v529==0
replace sex_month=0 if v529!=0

tab sex_month if v502==1 [aw=v005/1000000]
tab sex_month if v502==1 & v504==1 [aw=v005/1000000]


************************************************************
clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"

tab v504 if v502==1 [aw=v005/1000000]

gen sex_month=1 if v529==0
replace sex_month=0 if v529!=0

tab sex_month if v502==1 [aw=v005/1000000]
tab sex_month if v502==1 & v504==1 [aw=v005/1000000]
