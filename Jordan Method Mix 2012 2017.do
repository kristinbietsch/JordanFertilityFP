* Jordan 2017
clear all
set maxvar 10000
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR71FL.DTA"

tab v312 [aw=v005/1000000]

tab v312 if v313==3 [aw=v005/1000000]

* Jordan 2012
clear all
set maxvar 10000

use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA"


tab v312 [aw=v005/1000000]

tab v312 if v313==3 [aw=v005/1000000]
