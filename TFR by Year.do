clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR71FL.DTA"

tabexp, trend(1) endy(2016) rates

tabexp, awf(awfactt)


tabexp, trend(1) endy(2016) rates awf(awfactt)
tabexp, trend(1) endy(2013) rates awf(awfactt)

tabexp [pweight=v005], length(10) ageg(5) bvar(b3_*) dates(v008) wbirth(v011) trend(1) endy(2016) rates awf(awfactt)


clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR6CFL.DTA" 

tabexp [pweight=v005], length(5) ageg(5) bvar(b3_*) dates(v008) wbirth(v011) trend(1) endy(2011) rates awf(awfactt)


use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOIR61FL.DTA"
tabexp [pweight=v005], length(2) ageg(5) bvar(b3_*) dates(v008) wbirth(v011) trend(1) endy(2008) rates awf(awfactt)



clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOPR71FL.DTA" 
tab hv105 hv104 [aw=hv105/1000000], cell nofreq

clear all
use "C:\Users\KBietsch\Files\DHS Data\Jordan\JOPR6CFL.DTA" 
tab hv105 hv104 [aw=hv105/1000000], cell nofreq
