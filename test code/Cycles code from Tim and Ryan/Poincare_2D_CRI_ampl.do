use "C:\Research\Unstable Learning\Markets\data\order_pri_101817.dta"
append using "C:\Research\Unstable Learning\Markets\data\order_pri_110117A.dta"
append using "C:\Research\Unstable Learning\Markets\data\order_pri_110117B.dta"
** Note these are the data for the fall 2017 data with wide starting points **

cd "C:\Research\Unstable Learning\Markets\data\cycles2D"

** lets get all periods except discrete **
keep if adjustSpeed!="Discrete"

** Noticed a problem with tiny amount of missing data (about 15 time points) **
drop if price_r6==.

gen median=(price_r3+price_r4)/2
gen IQR=(price_r1+price_r2-price_r5-price_r6)/2

** Make sure sorted with adjacent datapoints **
sort session group period timeticker

** Now drop the first 15 seconds of each period for the initial adjustment **
drop if timeticker<39
** This leaves just the final 75 seconds of the 90-second period **

** Find the means for the summary statistics, by period, for centers **
collapse (mean) median IQR, by(session group period)
rename median cent_median
rename IQR cent_IQR
sort session group period

save medIQR_centers_Un.dta, replace

clear

** Now go back and prepare the data as before **
use "C:\Research\Unstable Learning\Markets\data\order_pri_101817.dta"
append using "C:\Research\Unstable Learning\Markets\data\order_pri_110117A.dta"
append using "C:\Research\Unstable Learning\Markets\data\order_pri_110117B.dta"
** Note these are the data for the fall 2017 data with wide starting points **

** lets get all periods except discrete **
keep if adjustSpeed!="Discrete"

** Noticed a problem with tiny amount of missing data (about 15 time points) **
drop if price_r6==.

gen median=(price_r3+price_r4)/2
gen IQR=(price_r1+price_r2-price_r5-price_r6)/2

** Make sure sorted with adjacent datapoints **
sort session group period timeticker

** Now drop the first 15 seconds of each period for the initial adjustment **
drop if timeticker<39
** This leaves just the final 75 seconds of the 90-second period **

** Merge in the earlier created centers for the summary statistics, by period **
merge m:1 session group period using "C:\Research\Unstable Learning\Markets\data\cycles2D\medIQR_centers_Un.dta"

drop _merge

** Now we are ready to count trips **
gen vertCW=0
gen vertCCW=0
gen horzCW=0
gen horzCCW=0

replace vertCW=1 if median<cent_median & median[_n-1]>=cent_median & IQR<cent_IQR & IQR[_n-1]<cent_IQR & period==period[_n-1]
replace vertCCW=1 if median>cent_median & median[_n-1]<=cent_median & IQR<cent_IQR & IQR[_n-1]<cent_IQR & period==period[_n-1]
replace horzCW=1 if IQR>cent_IQR & IQR[_n-1]<=cent_IQR & median<cent_median & median[_n-1]<cent_median & period==period[_n-1]
replace horzCCW=1 if IQR<cent_IQR & IQR[_n-1]>=cent_IQR & median<cent_median & median[_n-1]<cent_median & period==period[_n-1]

** Now construct cycle rotation index **
collapse (sum) vertCW vertCCW horzCW horzCCW, by(session period group payofftype adjustSpeed)
gen vert_cycle_index = (vertCW - vertCCW)/(vertCW + vertCCW)
gen horz_cycle_index = (horzCW - horzCCW)/(horzCW + horzCCW)

** Create identifier for independent session-group **
gen sessgroup = session+group

** Summarize cycle rotation indexes and trips across tripwire in each direction **
bysort adjustSpeed payofftype: tabstat vert_cycle_index horz_cycle_index vertCW horzCW vertCCW horzCCW, statistics( mean ) by(sessgroup)

corr vert_cycle_index horz_cycle_index vertCW horzCW vertCCW horzCCW

** Aggregate them to the session-treatment level **
collapse (mean) vert_cycle_index horz_cycle_index vertCW horzCW vertCCW horzCCW, by (sessgroup payofftype adjustSpeed)

/*
** Simple, initial statistical tests **
by adjustSpeed, sort : ttest vert_cycle_index, by(payofftype)
by adjustSpeed, sort : ttest horz_cycle_index, by(payofftype)

sort payofftype adjustSpeed
by payofftype adjustSpeed : signrank vert_cycle_index = horz_cycle_index
by payofftype  : signrank vert_cycle_index = horz_cycle_index

*/

clear

** This section below here calculates the cycle amplitude by collecting info on distance from the **
** center when crossing the tripwire **

** Now go back to re-count trips to summarize the mean and median crossing point with the tripwire **
** Now go back and prepare the data as before **
use "C:\Research\Unstable Learning\Markets\data\order_pri_101817.dta"
append using "C:\Research\Unstable Learning\Markets\data\order_pri_110117A.dta"
append using "C:\Research\Unstable Learning\Markets\data\order_pri_110117B.dta"
** Note these are the data for the fall 2017 data with wide starting points **

** lets get all periods except discrete **
keep if adjustSpeed!="Discrete"

** Noticed a problem with tiny amount of missing data (about 15 time points) **
drop if price_r6==.

gen median=(price_r3+price_r4)/2
gen IQR=(price_r1+price_r2-price_r5-price_r6)/2

** Make sure sorted with adjacent datapoints **
sort session group period timeticker

** Now drop the first 15 seconds of each period for the initial adjustment **
drop if timeticker<39
** This leaves just the final 75 seconds of the 90-second period **

** Merge in the earlier created centers for the summary statistics, by period **
merge m:1 session group period using "C:\Research\Unstable Learning\Markets\data\cycles2D\medIQR_centers_Un.dta"

drop _merge

** Now we are ready to count trips **
gen vertCW=0
gen vertCCW=0
gen horzCW=0
gen horzCCW=0

replace vertCW=1 if median<cent_median & median[_n-1]>=cent_median & IQR<cent_IQR & IQR[_n-1]<cent_IQR & period==period[_n-1]
replace vertCCW=1 if median>cent_median & median[_n-1]<=cent_median & IQR<cent_IQR & IQR[_n-1]<cent_IQR & period==period[_n-1]
replace horzCW=1 if IQR>cent_IQR & IQR[_n-1]<=cent_IQR & median<cent_median & median[_n-1]<cent_median & period==period[_n-1]
replace horzCCW=1 if IQR<cent_IQR & IQR[_n-1]>=cent_IQR & median<cent_median & median[_n-1]<cent_median & period==period[_n-1]

** Need to calculate distance from the center of the cycle **
gen dist_median = abs(median - cent_median)
gen dist_IQR = abs(IQR - cent_IQR)

** Keeping correct (clockwise) direction crossings separate from incorrect (counter) **

** Now calculate mean and median distance from the center, specifically for crossing points **
collapse (mean) ave_median=dist_median ave_IQR=dist_IQR (median) med_median=dist_median med_IQR=dist_IQR, by (vertCW vertCCW horzCW horzCCW session group payofftype adjustSpeed)

** Higher amplitude is greater distance from the center **
gen ave_vert_amp = ave_IQR if vertCW==1
gen med_vert_amp = med_IQR if vertCW==1
gen ave_horz_amp = ave_median if horzCW==1
gen med_horz_amp = med_median if horzCW==1

** Create identifier for independent session-group **
gen sessgroup = session+group

** Summarize amplitude size by session and treatment **
bysort adjustSpeed payofftype: tabstat ave_vert_amp med_vert_amp ave_horz_amp med_horz_amp, statistics( mean ) by(sessgroup)

** Aggregate them to the session-treatment level **
collapse (mean) ave_vert_amp med_vert_amp ave_horz_amp med_horz_amp, by (sessgroup payofftype adjustSpeed)

