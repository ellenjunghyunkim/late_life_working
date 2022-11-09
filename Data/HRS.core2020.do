* Note: activate this do file from within Stata
clear
set more off

* Cognition data
global path "/Users/junghyun.kim/Documents/CognitiveDecline"
use "/Users/junghyun.kim/Documents/CognitiveDecline/h20sta/h20d_r.dta"
gen hhidpn = hhid + pn
destring hhidpn, replace
keep hhidpn RD142 RD143 RD144 RD145 RD146 RD124 RD129 RD174 RD174W RD184 RD184W
save "$path/h20sta/cogtition.dta", replace
clear

* Current work status data
use "/Users/junghyun.kim/Documents/CognitiveDecline/h20sta/h20pr_r.dta"
gen hhidpn = hhid + pn
destring hhidpn, replace
keep hhidpn  RZ123
save "$path/h20sta/work_status.dta", replace
clear

clear
set more off

use "/Users/junghyun.kim/Documents/CognitiveDecline/h20sta/h20a_r.dta"
gen hhidpn = hhid + pn
destring hhidpn, replace
keep hhidpn  RA019  RA009
save "$path/h20sta/age_proxy.dta", replace
clear


