* ACHIEVED IDEAL FERTILITY

* Jordan 2017
clear all
set maxvar 10000
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR71FL.DTA"

gen met_ideal=1 if v201>=v613
replace met_ideal=0 if v201<v613
tab met_ideal if v502==1 [aw=v005/1000000]

* Jordan 2012
clear all
set maxvar 10000

use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"

gen met_ideal=1 if v201>=v613
replace met_ideal=0 if v201<v613
tab met_ideal if v502==1 [aw=v005/1000000]
