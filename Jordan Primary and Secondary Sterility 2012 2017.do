* primary and secondary sterility

* Jordan 2017
clear all
set maxvar 10000
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR71FL.DTA"

* primary sterility
tab v201 if v013==7 [aw=v005/1000000]
* 6.16%

* infecund/menopausal by age
gen inf_meno=1 if v626a==9
replace inf_meno=0 if v626a!=9

tab v013 inf_meno  [aw=v005/1000000], row nofreq

* Jordan 2012
clear all
set maxvar 10000

use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"

tab v201 if v013==7 [aw=v005/1000000]
* 7.41%

* infecund/menopausal by age
gen inf_meno=1 if v626a==9
replace inf_meno=0 if v626a!=9

tab v013 inf_meno  [aw=v005/1000000], row nofreq
