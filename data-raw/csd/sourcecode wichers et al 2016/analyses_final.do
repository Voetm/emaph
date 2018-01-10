clear
clear matrix
cd "F:\Mijn documenten\algemeen\zonmw_doelmatigheid\case peter\medstop_afbouw\datafiles_new"
use ews_peter3, clear
drop id
gen id= (dayno*100 + beepno)
gen time = _n

lpoly na time, degree(3) kernel(gaussian) generate (x s) se(se) n(1480)
gen na_smooth3 = na - s
lpoly pa time, degree(3) kernel(gaussian) generate (pa_x3 pa_s3) se(pa_se3) n(1480)
tsset time
tsline pa_s3
gen pa_smooth3 = pa - pa_s3

lpoly agit time, degree(3) kernel(gaussian) generate (agit_x3 agit_s3) se(agit_se3) n(1480)

tsset time
tsline agit_s3
gen agit_smooth3 = agit - agit_s3

lpoly pat_sus time, degree(3) kernel(gaussian) generate (sus_x3 sus_s3) se(sus_se3) n(1480)

tsset time
tsline sus_s3
gen sus_smooth3 = pat_sus - sus_s3

lpoly thou_worry time, degree(3) kernel(gaussian) generate (worry_x3 worry_s3) se(worry_se3) n(1480)

tsset time
tsline worry_s3
gen worry_smooth3 = thou_worry - worry_s3
*recoding positive affect to go in same direction as the others
gen pa_re_smooth3=pa_smooth*-1
gen smooth_tot = pa_re_smooth3 + na_smooth3 + agit_smooth3 + sus_smooth3 + worry_smooth3

*voor mean moving window
gen pa_reraw= pa*-1
egen pa_re_std=std(pa_reraw)
egen na_std=std(na)
egen agit_std=std(agit)
egen sus_std=std(pat_sus)

gen sum_raw= pa_reraw + na + agit + thou_worry + pat_sus
gen sum_raw_std= pa_re_std + na_std + agit_std + sus_std



gen sum_all= mood_irritat - mood_satisfi - mood_enthus - mood_cheerf + mood_restl + mood_agitate + mood_lonely1 + mood_anxious1 + mood_guilty1 - mood_decis - mood_strong1
save kernel_fin, replace

use "kernel_fin.dta",clear 
clear
*number of days - moving window
set obs 210
gen variance1=.
gen y1=""


preserve
use "kernel_fin.dta",clear
local j 0
foreach v of var smooth_tot {
				 local j =`j'+1
				 local i 0
				 while `i' < 210 {
						local i=`i'+1
                                noisily display "This is run #" `i' " analysing variables: " "`v'"     
							    sum `v' if fase>0 &  dayno>`i'  &  dayno<`i' +30 
								scalar variance=r(sd)
								display "-->          " variance  "       
                                restore
							
								replace variance`j' =scalar(variance) in `i'
                                replace y`j'="`v'" in `i'                        
                                preserve
                                use "kernel_fin.dta",clear
                                }
				 noisily display `j'		
                }
*end of loop



*change in variance based on the sum of detrended variables
save matrixi_kern_var_total.dta,replace
use matrixi_kern_var_total.dta,clear
gen vark3_tot=variance1

drop variance1 y*
gen obs = _n
sort obs
gen new_obs=obs+29
sort new_obs
save matrix_variance_total.dta,replace
tsset new_obs
tsline vark3_tot

use "kernel_fin.dta",clear 
clear
*number of days - moving window
set obs 210
gen autoreg1=.
gen y1=""


preserve
use "kernel_fin.dta",clear
local j 0
foreach v of var smooth_tot {
local j =`j'+1
				 local i 0
				 while `i' < 210 {
						local i=`i'+1
                                noisily display "This is run #" `i' " analysing variables: " "`v'"     
							    xtmixed `v' l.`v' if fase>0 &  dayno>`i'  &  dayno<`i' +30, noconst || dayno:, noconst
                                scalar autoreg=_b[L.`v']
                                test L.`v'
                                display "-->          " autoreg  "        
                                restore
								replace autoreg`j' =scalar(autoreg) in `i'
                                replace y`j'="`v'" in `i'
								preserve
                                use "kernel_fin.dta",clear
                                }
				noisily display `j'		
                }
*end of loop

*change in autocorrelation based on sum of smoothed variables
save matrix_auto.dta,replace
use matrix_auto.dta,clear
gen autk3_tot=autoreg1
drop autoreg* y*
gen obs = _n
sort obs
gen new_obs=obs+29
save matrix_auto_fin.dta,replace
tsset new_obs
tsline autk3_tot

*mean (toegevoegd bij letter psychother)
use "kernel_fin.dta",clear 
clear
*number of days - moving window
set obs 210
gen m1=.
gen y1=""

preserve
use "kernel_fin.dta",clear
local j 0
foreach v of var sum_all {
local j=`j'+1	
				 local i 0
				 while `i' < 210 {
						local i=`i'+1
                                noisily display "This is run #" `i' " analysing variables: " "`v'"     
							    sum `v' if fase>0 &  dayno>`i'  &  dayno<`i' +30 
								scalar m`j'= r(mean) 
                                display m`j'
                                display "-->          " m`j'  "        
                                restore
								replace m`j' =scalar(m`j') in `i'
                                replace y`j'="`v'" in `i'
								preserve
                                use "kernel_fin.dta",clear
                                }
				noisily display `j'	
                }
*end of loop


gen meank3_tot=m1
drop m1 y1
gen obs = _n
sort obs
gen new_obs=obs+29
save matrix_mean_fin.dta,replace
tsset new_obs
tsline meank3_tot

*NETWORKS
*different detrending method than smoothing to not obscure local micro fluctuations. As here we look for lag 1 effects.
* however we need to detrend the data before analysis (but without smoothing)
*we choose to detrend per phase as we can expect that trends will differ depending on phase
*we detrended per variable and per phase in case of a significant trend. The difference between observation and predicted trend value was taken in case of sign trends.
*the difference between observation and average value was taken in case of nonsignificance. IN this way we always used the deviation of the expected value of variables in the analyses

clear
clear matrix
cd "C:\Mijn documenten\algemeen\zonmw_doelmatigheid\case peter\medstop_afbouw\datafiles_new"
use ews_peter3, clear
drop id
gen id= (dayno*100 + beepno)

*detrending per fase
gen id_time=(dayno-1)*10 + beepno
tsset id_time

** AGITATION
regress agit realtime if fase==1
regress agit realtime if fase==2
regress agit realtime if fase==3
predict fit_agit_3 if fase==3, xb
twoway (lfit agit realtime if fase==3, atobs) (scatter agit realtime if fase==3, sort) (scatter fit_agit_3 realtime if fase==3, sort)
order dayno beepno fase agit fit_agit_3
regress agit realtime if fase==4
regress agit realtime if fase==5

gen agit_not_=agit - fit_agit_3 if fase==3
egen agit_av=mean(agit), by(fase)
replace agit_not_=agit -agit_av if fase~=3
order dayno beepno fase agit_not_ agit agit_av fit_agit_3

*NA
regress na realtime if fase==1
regress na realtime if fase==2 
regress na realtime if fase==3
regress na realtime if fase==4
regress na realtime if fase==5
*phase 2,3, en 4 have trend
regress na realtime if fase==2 
predict fit_na_2 if fase==2, xb
twoway (lfit na realtime if fase==2, atobs) (scatter na realtime if fase==2, sort) (scatter fit_na_2 realtime if fase==2, sort)
order dayno beepno fase na fit_na_2
regress na realtime if fase==3
predict fit_na_3 if fase==3, xb
twoway (lfit na realtime if fase==3, atobs) (scatter na realtime if fase==3, sort) (scatter fit_na_3 realtime if fase==3, sort)
order dayno beepno fase na fit_na_2 fit_na_3
regress na realtime if fase==4
predict fit_na_4 if fase==4, xb
twoway (lfit na realtime if fase==4, atobs) (scatter na realtime if fase==4, sort) (scatter fit_na_4 realtime if fase==4, sort)

egen na_av=mean(na), by(fase)
gen na_not_=na - fit_na_2 if fase==2
replace na_not_= na - fit_na_3 if fase==3
replace na_not_= na - fit_na_4 if fase==4
replace na_not_= na - na_av if fase==1 | fase==5
order dayno beepno fase na_not_ fit_na_2 fit_na_3 fit_na_4 na_av


*WORRY
regress thou_worry realtime if fase==1
regress thou_worry realtime if fase==2
regress thou_worry realtime if fase==3
regress thou_worry realtime if fase==4
regress thou_worry realtime if fase==5

regress thou_worry realtime if fase==4
predict fit_worry_4 if fase==4, xb
twoway (lfit thou_worry realtime if fase==4, atobs) (scatter thou_worry realtime if fase==4, sort) (scatter fit_worry_4 realtime if fase==4, sort)
egen worry_av=mean(thou_worry), by(fase)
gen worry_not_=thou_worry - fit_worry_4 if fase==4
replace worry_not_=thou_worry - worry_av if fase~=4
order dayno beepno fase worry_not_ thou_worry fit_worry_4 worry_av

*SUSPICIOUS

regress pat_sus realtime if fase==1
regress pat_sus realtime if fase==2
regress pat_sus realtime if fase==3
regress pat_sus realtime if fase==4
regress pat_sus realtime if fase==5

regress pat_sus realtime if fase==1
predict fit_sus_1 if fase==1, xb
twoway (lfit pat_sus realtime if fase==1, atobs) (scatter pat_sus realtime if fase==1, sort) (scatter fit_sus_1 realtime if fase==1, sort)
regress pat_sus realtime if fase==3
predict fit_sus_3 if fase==3, xb
twoway (lfit pat_sus realtime if fase==3, atobs) (scatter pat_sus realtime if fase==3, sort) (scatter fit_sus_3 realtime if fase==3, sort)
regress pat_sus realtime if fase==4
predict fit_sus_4 if fase==4, xb
twoway (lfit pat_sus realtime if fase==4, atobs) (scatter pat_sus realtime if fase==4, sort) (scatter fit_sus_4 realtime if fase==4, sort)
regress pat_sus realtime if fase==5
predict fit_sus_5 if fase==5, xb
twoway (lfit pat_sus realtime if fase==5, atobs) (scatter pat_sus realtime if fase==5, sort) (scatter fit_sus_5 realtime if fase==5, sort)

egen sus_av=mean(pat_sus), by(fase)
gen sus_not_=pat_sus - fit_sus_1 if fase==1
replace sus_not_=pat_sus - fit_sus_3 if fase==3
replace sus_not_=pat_sus - fit_sus_4 if fase==4
replace sus_not_=pat_sus - fit_sus_5 if fase==5
replace sus_not_=pat_sus - sus_av if fase==2
order dayno beepno fase sus_not_ pat_sus fit_sus_1 sus_av fit_sus_3 fit_sus_4 fit_sus_5
tsset id
tsline sus_not_

*PA
regress pa realtime if fase==1
regress pa realtime if fase==2
regress pa realtime if fase==3
regress pa realtime if fase==4
regress pa realtime if fase==5

regress pa realtime if fase==1
predict fit_pa_1 if fase==1, xb
twoway (lfit pa realtime if fase==1, atobs) (scatter pa realtime if fase==1, sort) (scatter fit_pa_1 realtime if fase==1, sort)
egen pa_av=mean(pa), by(fase)
gen pa_not_=pa - fit_pa_1 if fase==1
replace pa_not_= pa - pa_av if fase~=1
order dayno beepno fase pa_not_ pa fit_pa_1 pa_av
tsset id
tsline pa_not_

egen dayvar_agit=sd(agit_not_), by(dayno)
egen dayvar_pa=sd(pa_not_), by(dayno)
egen dayvar_na=sd(na_not_), by(dayno)
egen dayvar_worry=sd(worry_not_), by(dayno)
egen dayvar_sus=sd(sus_not_), by(dayno)

save detrend_,replace
use detrend_, clear


*sensitivity fase before shift: we only use data of before day 128 (before the shift) in phase 4
*we use no constant as we expect intercept is 0 (as we use deviations from expected value). 



regress pa_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==1 & dayno<128, noconst 
regress agit_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==1 & dayno<128, noconst
regress na_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==1 & dayno<128, noconst 
regress worry_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==1 & dayno<128, noconst 
regress sus_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==1 & dayno<128, noconst 


regress pa_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==2 & dayno<128, noconst  
regress agit_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==2 & dayno<128, noconst 
regress na_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==2 & dayno<128, noconst 
regress worry_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==2 & dayno<128, noconst 
regress sus_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==2 & dayno<128, noconst 



regress pa_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==3 & dayno<128, noconst 
regress agit_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==3 & dayno<128, noconst 
regress na_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==3 & dayno<128, noconst 
regress worry_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==3 & dayno<128, noconst 
regress sus_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==3 & dayno<128, noconst 



regress pa_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==4 & dayno<128, noconst 
regress agit_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==4 & dayno<128, noconst 
regress na_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==4 & dayno<128, noconst 
regress worry_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==4 & dayno<128, noconst 
regress sus_not_ l.pa_not_ l.agit_not_ l.na_not_ l.worry_not_ l.sus_not_ if fase==4 & dayno<128, noconst 



