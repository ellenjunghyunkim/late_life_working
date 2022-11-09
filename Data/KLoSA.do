clear all
set maxvar 30000
query memory
cd "/Users/junghyun.kim/Documents/CognitiveDecline/KLoSA_STATA_2022v3"

use "/Users/junghyun.kim/Documents/CognitiveDecline/KLoSA_STATA_2022v3/Lt01.dta"
sort pid
save d:data01, replace
use "/Users/junghyun.kim/Documents/CognitiveDecline/KLoSA_STATA_2022v3/Lt02.dta"
sort pid
save d:data02, replace
use "/Users/junghyun.kim/Documents/CognitiveDecline/KLoSA_STATA_2022v3/Lt03.dta"
sort pid
save d:data03, replace
use "/Users/junghyun.kim/Documents/CognitiveDecline/KLoSA_STATA_2022v3/Lt04.dta"
sort pid
save d:data04, replace
use "/Users/junghyun.kim/Documents/CognitiveDecline/KLoSA_STATA_2022v3/Lt05.dta"
sort pid
save d:data05, replace
use "/Users/junghyun.kim/Documents/CognitiveDecline/KLoSA_STATA_2022v3/Lt06.dta"
sort pid
save d:data06, replace
use "/Users/junghyun.kim/Documents/CognitiveDecline/KLoSA_STATA_2022v3/Lt07.dta"
sort pid
save d:data07, replace
use "/Users/junghyun.kim/Documents/CognitiveDecline/KLoSA_STATA_2022v3/Lt08.dta"
sort pid
save d:data08, replace

merge pid using d:data01 d:data02  d:data03 d:data04 d:data05 d:data06 d:data07 d:data08
save d:merged_data_KLoSA

use "/Users/junghyun.kim/Documents/CognitiveDecline/KLoSA_STATA_2022v3/d:merged_data_KLoSA"

keep pid  w*C001  w*gender1 w*A002_age w*edu w*job w*mmse w*present_labor w*marital w*A002y w*hhnetassets w*hhinc 

duplicates report pid
bysort pid: gen dups=_N
browse if dups>1
duplicates drop
by pid: replace dups=_N
drop if dups>1
drop dups

reshape long w@C001 w@gender1 w@A002_age w@edu w@job w@mmse w@present_labor w@marital w@A002y w@hhnetassets w@hhinc, i(pid) j(wave) string

save d:KLoSA_data



