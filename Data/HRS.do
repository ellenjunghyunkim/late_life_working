
clear all
set maxvar 30000
query memory
cd "/Users/junghyun.kim/Documents/CognitiveDecline/randhrs1992_2018v2_STATA"
use "/Users/junghyun.kim/Documents/CognitiveDecline/randhrs1992_2018v2_STATA/randhrs1992_2018v2.dta"

keep hhidpn  r*proxy r*agem_e r*agey_e  r*tr20 r*ser7 r*bwc20 r*dy r*mo r*yr r*dw r*pres r*vp r*cact r*scis r*shlt r*mstat r*jcoccb r*cogtot r*work r*memrys r*slfmem h*atotb h*itot rabyear ragender raracem rahispan rabplace raedyrs 

reshape long r@proxy r@agem_e r@agey_e r@tr20 r@ser7 r@bwc20 r@dy r@mo r@yr r@dw r@pres r@vp r@cact r@scis r@shlt r@mstat r@jcoccb r@cogtot r@work r@memrys r@slfmem  h@atotb h@itot, i(hhidpn rabyear ragender raracem rahispan rabplace raedyrs) j(wave)

save HRS_data

