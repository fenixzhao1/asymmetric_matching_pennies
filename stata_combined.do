**********Data preparation**********
* open dataset
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_production.dta", clear

* set subperiod variable
gen subperiod = 0
replace subperiod = 1 if tick==0 & num_subperiods!=0
replace subperiod = 2 if tick==1 & num_subperiods!=0
replace subperiod = 3 if tick==2 & num_subperiods!=0
replace subperiod = 4 if tick==3 & num_subperiods!=0
replace subperiod = 5 if tick==4 & num_subperiods!=0
replace subperiod = 6 if tick==5 & num_subperiods!=0
replace subperiod = 7 if tick==6 & num_subperiods!=0
replace subperiod = 8 if tick==7 & num_subperiods!=0
replace subperiod = 9 if tick==8 & num_subperiods!=0
replace subperiod = 10 if tick==9 & num_subperiods!=0
replace subperiod = 11 if tick==10 & num_subperiods!=0
replace subperiod = 12 if tick==11 & num_subperiods!=0
replace subperiod = 13 if tick==12 & num_subperiods!=0
replace subperiod = 14 if tick==13 & num_subperiods!=0
replace subperiod = 15 if tick==14 & num_subperiods!=0
replace subperiod = 16 if tick==15 & num_subperiods!=0
replace subperiod = 17 if tick==16 & num_subperiods!=0
replace subperiod = 18 if tick==17 & num_subperiods!=0
replace subperiod = 19 if tick==18 & num_subperiods!=0
replace subperiod = 20 if tick==19 & num_subperiods!=0
replace subperiod = 21 if tick==20 & num_subperiods!=0
replace subperiod = 22 if tick==21 & num_subperiods!=0
replace subperiod = 23 if tick==22 & num_subperiods!=0
replace subperiod = 24 if tick==23 & num_subperiods!=0
replace subperiod = 25 if tick==24 & num_subperiods!=0

replace subperiod = 1 if tick>=0 & tick<=11 & num_subperiods==0
replace subperiod = 2 if tick>=12 & tick<=23 & num_subperiods==0
replace subperiod = 3 if tick>=24 & tick<=35 & num_subperiods==0
replace subperiod = 4 if tick>=36 & tick<=47 & num_subperiods==0
replace subperiod = 5 if tick>=48 & tick<=59 & num_subperiods==0
replace subperiod = 6 if tick>=60 & tick<=71 & num_subperiods==0
replace subperiod = 7 if tick>=72 & tick<=83 & num_subperiods==0
replace subperiod = 8 if tick>=84 & tick<=95 & num_subperiods==0
replace subperiod = 9 if tick>=96 & tick<=107 & num_subperiods==0
replace subperiod = 10 if tick>=108 & tick<=119 & num_subperiods==0
replace subperiod = 11 if tick>=120 & tick<=131 & num_subperiods==0
replace subperiod = 12 if tick>=132 & tick<=143 & num_subperiods==0
replace subperiod = 13 if tick>=144 & tick<=155 & num_subperiods==0
replace subperiod = 14 if tick>=156 & tick<=167 & num_subperiods==0
replace subperiod = 15 if tick>=168 & tick<=179 & num_subperiods==0
replace subperiod = 16 if tick>=180 & tick<=191 & num_subperiods==0
replace subperiod = 17 if tick>=192 & tick<=203 & num_subperiods==0
replace subperiod = 18 if tick>=204 & tick<=215 & num_subperiods==0
replace subperiod = 19 if tick>=216 & tick<=227 & num_subperiods==0
replace subperiod = 20 if tick>=228 & tick<=239 & num_subperiods==0
replace subperiod = 21 if tick>=240 & tick<=251 & num_subperiods==0
replace subperiod = 22 if tick>=252 & tick<=263 & num_subperiods==0
replace subperiod = 23 if tick>=264 & tick<=275 & num_subperiods==0
replace subperiod = 24 if tick>=276 & tick<=287 & num_subperiods==0
replace subperiod = 25 if tick>=288 & tick<=299 & num_subperiods==0

* set block variable
gen block = 0
replace block = 1 if round<=10 & dummy_mm==1
replace block = 2 if round>10 & dummy_mm==1
replace block = 1 if round<=15 & dummy_mm==0
replace block = 2 if round>15 & dummy_mm==0

* set second half varibale
gen second_half = 0
replace second_half = 1 if subperiod>=13 & dummy_mm==1
replace second_half = 1 if subperiod>=8 & dummy_mm==0

* save dataset
save "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_production.dta", replace



**********T-test: data summary table with time averages (Table 4)**********
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_summary.dta", clear

gen diff_ne_mid = Deviation_NE - Deviation_Mid
gen diff_mid_mm = Deviation_Mid - Deviation_MM

* Panel A: AMPa games
reg diff_ne_mid if game=="AMPa" & match=="mm", cluster(session_id)
reg diff_mid_mm if game=="AMPa" & match=="mm", cluster(session_id)

reg diff_ne_mid if game=="AMPa" & match=="rp", cluster(session_id)
reg diff_mid_mm if game=="AMPa" & match=="rp", cluster(session_id)

reg diff_ne_mid if game=="AMPa" & actionsets=="M", cluster(session_id)
reg diff_mid_mm if game=="AMPa" & actionsets=="M", cluster(session_id)

reg diff_ne_mid if game=="AMPa" & actionsets=="P", cluster(session_id)
reg diff_mid_mm if game=="AMPa" & actionsets=="P", cluster(session_id)

reg diff_ne_mid if game=="AMPa" & time=="C", cluster(session_id)
reg diff_mid_mm if game=="AMPa" & time=="C", cluster(session_id)

reg diff_ne_mid if game=="AMPa" & time=="D", cluster(session_id)
reg diff_mid_mm if game=="AMPa" & time=="D", cluster(session_id)

* Panel B: AMPb games
reg diff_ne_mid if game=="AMPb" & match=="mm", cluster(session_id)
reg diff_mid_mm if game=="AMPb" & match=="mm", cluster(session_id)

reg diff_ne_mid if game=="AMPb" & match=="rp", cluster(session_id)
reg diff_mid_mm if game=="AMPb" & match=="rp", cluster(session_id)

reg diff_ne_mid if game=="AMPb" & actionsets=="M", cluster(session_id)
reg diff_mid_mm if game=="AMPb" & actionsets=="M", cluster(session_id)

reg diff_ne_mid if game=="AMPb" & actionsets=="P", cluster(session_id)
reg diff_mid_mm if game=="AMPb" & actionsets=="P", cluster(session_id)

reg diff_ne_mid if game=="AMPb" & time=="C", cluster(session_id)
reg diff_mid_mm if game=="AMPb" & time=="C", cluster(session_id)

reg diff_ne_mid if game=="AMPb" & time=="D", cluster(session_id)
reg diff_mid_mm if game=="AMPb" & time=="D", cluster(session_id)

* Panel C: IDDS games
reg diff_ne_mid if game=="IDDS" & match=="mm", cluster(session_id)
reg diff_mid_mm if game=="IDDS" & match=="mm", cluster(session_id)

reg diff_ne_mid if game=="IDDS" & match=="rp", cluster(session_id)
reg diff_mid_mm if game=="IDDS" & match=="rp", cluster(session_id)

reg diff_ne_mid if game=="IDDS" & actionsets=="M", cluster(session_id)
reg diff_mid_mm if game=="IDDS" & actionsets=="M", cluster(session_id)

reg diff_ne_mid if game=="IDDS" & actionsets=="P", cluster(session_id)
reg diff_mid_mm if game=="IDDS" & actionsets=="P", cluster(session_id)

reg diff_ne_mid if game=="IDDS" & time=="C", cluster(session_id)
reg diff_mid_mm if game=="IDDS" & time=="C", cluster(session_id)

reg diff_ne_mid if game=="IDDS" & time=="D", cluster(session_id)
reg diff_mid_mm if game=="IDDS" & time=="D", cluster(session_id)



**********T-test: signed distance to NE, MM, and Center for row and column players (Table 3)**********
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_summary.dta", clear

* generate signed distance dependent variables
gen Diffp1NE = p1_average - p1NEmix
gen Diffp2NE = p2_average - p2NEmix
gen Diffp1MM = p1_average - p1MMmix
gen Diffp2MM = p2_average - p2MMmix
gen Diffp1Center = p1_average - 0.5
gen Diffp2Center = p2_average - 0.5

* Panel A: AMPa games
reg Diffp1NE if game=="AMPa" & match=="mm", cluster(session_id)
reg Diffp2NE if game=="AMPa" & match=="mm", cluster(session_id)
reg Diffp1MM if game=="AMPa" & match=="mm", cluster(session_id)
reg Diffp2MM if game=="AMPa" & match=="mm", cluster(session_id)
reg Diffp1Center if game=="AMPa" & match=="mm", cluster(session_id)
reg Diffp2Center if game=="AMPa" & match=="mm", cluster(session_id)

reg Diffp1NE if game=="AMPa" & match=="rp", cluster(session_id)
reg Diffp2NE if game=="AMPa" & match=="rp", cluster(session_id)
reg Diffp1MM if game=="AMPa" & match=="rp", cluster(session_id)
reg Diffp2MM if game=="AMPa" & match=="rp", cluster(session_id)
reg Diffp1Center if game=="AMPa" & match=="rp", cluster(session_id)
reg Diffp2Center if game=="AMPa" & match=="rp", cluster(session_id)

reg Diffp1NE if game=="AMPa" & actionsets=="M", cluster(session_id)
reg Diffp2NE if game=="AMPa" & actionsets=="M", cluster(session_id)
reg Diffp1MM if game=="AMPa" & actionsets=="M", cluster(session_id)
reg Diffp2MM if game=="AMPa" & actionsets=="M", cluster(session_id)
reg Diffp1Center if game=="AMPa" & actionsets=="M", cluster(session_id)
reg Diffp2Center if game=="AMPa" & actionsets=="M", cluster(session_id)

reg Diffp1NE if game=="AMPa" & actionsets=="P", cluster(session_id)
reg Diffp2NE if game=="AMPa" & actionsets=="P", cluster(session_id)
reg Diffp1MM if game=="AMPa" & actionsets=="P", cluster(session_id)
reg Diffp2MM if game=="AMPa" & actionsets=="P", cluster(session_id)
reg Diffp1Center if game=="AMPa" & actionsets=="P", cluster(session_id)
reg Diffp2Center if game=="AMPa" & actionsets=="P", cluster(session_id)

reg Diffp1NE if game=="AMPa" & time=="C", cluster(session_id)
reg Diffp2NE if game=="AMPa" & time=="C", cluster(session_id)
reg Diffp1MM if game=="AMPa" & time=="C", cluster(session_id)
reg Diffp2MM if game=="AMPa" & time=="C", cluster(session_id)
reg Diffp1Center if game=="AMPa" & time=="C", cluster(session_id)
reg Diffp2Center if game=="AMPa" & time=="C", cluster(session_id)

reg Diffp1NE if game=="AMPa" & time=="D", cluster(session_id)
reg Diffp2NE if game=="AMPa" & time=="D", cluster(session_id)
reg Diffp1MM if game=="AMPa" & time=="D", cluster(session_id)
reg Diffp2MM if game=="AMPa" & time=="D", cluster(session_id)
reg Diffp1Center if game=="AMPa" & time=="D", cluster(session_id)
reg Diffp2Center if game=="AMPa" & time=="D", cluster(session_id)

* Panel B: AMPb games
reg Diffp1NE if game=="AMPb" & match=="mm", cluster(session_id)
reg Diffp2NE if game=="AMPb" & match=="mm", cluster(session_id)
reg Diffp1MM if game=="AMPb" & match=="mm", cluster(session_id)
reg Diffp2MM if game=="AMPb" & match=="mm", cluster(session_id)
reg Diffp1Center if game=="AMPb" & match=="mm", cluster(session_id)
reg Diffp2Center if game=="AMPb" & match=="mm", cluster(session_id)

reg Diffp1NE if game=="AMPb" & match=="rp", cluster(session_id)
reg Diffp2NE if game=="AMPb" & match=="rp", cluster(session_id)
reg Diffp1MM if game=="AMPb" & match=="rp", cluster(session_id)
reg Diffp2MM if game=="AMPb" & match=="rp", cluster(session_id)
reg Diffp1Center if game=="AMPb" & match=="rp", cluster(session_id)
reg Diffp2Center if game=="AMPb" & match=="rp", cluster(session_id)

reg Diffp1NE if game=="AMPb" & actionsets=="M", cluster(session_id)
reg Diffp2NE if game=="AMPb" & actionsets=="M", cluster(session_id)
reg Diffp1MM if game=="AMPb" & actionsets=="M", cluster(session_id)
reg Diffp2MM if game=="AMPb" & actionsets=="M", cluster(session_id)
reg Diffp1Center if game=="AMPb" & actionsets=="M", cluster(session_id)
reg Diffp2Center if game=="AMPb" & actionsets=="M", cluster(session_id)

reg Diffp1NE if game=="AMPb" & actionsets=="P", cluster(session_id)
reg Diffp2NE if game=="AMPb" & actionsets=="P", cluster(session_id)
reg Diffp1MM if game=="AMPb" & actionsets=="P", cluster(session_id)
reg Diffp2MM if game=="AMPb" & actionsets=="P", cluster(session_id)
reg Diffp1Center if game=="AMPb" & actionsets=="P", cluster(session_id)
reg Diffp2Center if game=="AMPb" & actionsets=="P", cluster(session_id)

reg Diffp1NE if game=="AMPb" & time=="C", cluster(session_id)
reg Diffp2NE if game=="AMPb" & time=="C", cluster(session_id)
reg Diffp1MM if game=="AMPb" & time=="C", cluster(session_id)
reg Diffp2MM if game=="AMPb" & time=="C", cluster(session_id)
reg Diffp1Center if game=="AMPb" & time=="C", cluster(session_id)
reg Diffp2Center if game=="AMPb" & time=="C", cluster(session_id)

reg Diffp1NE if game=="AMPb" & time=="D", cluster(session_id)
reg Diffp2NE if game=="AMPb" & time=="D", cluster(session_id)
reg Diffp1MM if game=="AMPb" & time=="D", cluster(session_id)
reg Diffp2MM if game=="AMPb" & time=="D", cluster(session_id)
reg Diffp1Center if game=="AMPb" & time=="D", cluster(session_id)
reg Diffp2Center if game=="AMPb" & time=="D", cluster(session_id)

* Panel C: IDDS games
reg Diffp1NE if game=="IDDS" & match=="mm", cluster(session_id)
reg Diffp2NE if game=="IDDS" & match=="mm", cluster(session_id)
reg Diffp1MM if game=="IDDS" & match=="mm", cluster(session_id)
reg Diffp2MM if game=="IDDS" & match=="mm", cluster(session_id)
reg Diffp1Center if game=="IDDS" & match=="mm", cluster(session_id)
reg Diffp2Center if game=="IDDS" & match=="mm", cluster(session_id)

reg Diffp1NE if game=="IDDS" & match=="rp", cluster(session_id)
reg Diffp2NE if game=="IDDS" & match=="rp", cluster(session_id)
reg Diffp1MM if game=="IDDS" & match=="rp", cluster(session_id)
reg Diffp2MM if game=="IDDS" & match=="rp", cluster(session_id)
reg Diffp1Center if game=="IDDS" & match=="rp", cluster(session_id)
reg Diffp2Center if game=="IDDS" & match=="rp", cluster(session_id)

reg Diffp1NE if game=="IDDS" & actionsets=="M", cluster(session_id)
reg Diffp2NE if game=="IDDS" & actionsets=="M", cluster(session_id)
reg Diffp1MM if game=="IDDS" & actionsets=="M", cluster(session_id)
reg Diffp2MM if game=="IDDS" & actionsets=="M", cluster(session_id)
reg Diffp1Center if game=="IDDS" & actionsets=="M", cluster(session_id)
reg Diffp2Center if game=="IDDS" & actionsets=="M", cluster(session_id)

reg Diffp1NE if game=="IDDS" & actionsets=="P", cluster(session_id)
reg Diffp2NE if game=="IDDS" & actionsets=="P", cluster(session_id)
reg Diffp1MM if game=="IDDS" & actionsets=="P", cluster(session_id)
reg Diffp2MM if game=="IDDS" & actionsets=="P", cluster(session_id)
reg Diffp1Center if game=="IDDS" & actionsets=="P", cluster(session_id)
reg Diffp2Center if game=="IDDS" & actionsets=="P", cluster(session_id)

reg Diffp1NE if game=="IDDS" & time=="C", cluster(session_id)
reg Diffp2NE if game=="IDDS" & time=="C", cluster(session_id)
reg Diffp1MM if game=="IDDS" & time=="C", cluster(session_id)
reg Diffp2MM if game=="IDDS" & time=="C", cluster(session_id)
reg Diffp1Center if game=="IDDS" & time=="C", cluster(session_id)
reg Diffp2Center if game=="IDDS" & time=="C", cluster(session_id)

reg Diffp1NE if game=="IDDS" & time=="D", cluster(session_id)
reg Diffp2NE if game=="IDDS" & time=="D", cluster(session_id)
reg Diffp1MM if game=="IDDS" & time=="D", cluster(session_id)
reg Diffp2MM if game=="IDDS" & time=="D", cluster(session_id)
reg Diffp1Center if game=="IDDS" & time=="D", cluster(session_id)
reg Diffp2Center if game=="IDDS" & time=="D", cluster(session_id)



**********data summary table with time averages (Table 5)**********
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_summary.dta", clear

* drop IDDS
drop if game == "IDDS"

* generate treatment dummies
gen continuous = 0
replace continuous = 1 if time == "C"
gen pure = 0
replace pure = 1 if actionsets == "P"
gen mm = 0
replace mm = 1 if match == "mm"
gen AMPa = 0
replace AMPa = 1 if game == "AMPa"

gen continuous_pure = continuous * pure
gen continuous_mm = continuous * mm
gen continuous_AMPa = continuous * AMPa
gen pure_mm = pure * mm
gen pure_AMPa = pure * AMPa
gen mm_AMPa = mm * AMPa
gen continuous_pure_mm = continuous * pure * mm  
gen continuous_pure_AMPa = continuous * pure * AMPa
gen continuous_mm_AMPa = continuous * mm * AMPa
gen pure_mm_AMPa = pure * mm * AMPa
gen continuous_pure_mm_AMPa = continuous * pure * mm * AMPa 


** regressions with cluster std error at session level 
reg Deviation_NE continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(cluster session_id)
outreg2 using D:\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

reg Deviation_MM continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(cluster session_id)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

reg Deviation_Mid continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(cluster session_id)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

reg sd_geometric continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(cluster session_id)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)


** regressions with bootstrap
reg Deviation_NE continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(bootstrap)
est store regne
outreg2 using D:\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

reg Deviation_MM continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(bootstrap)
est store regmm
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

reg Deviation_Mid continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(bootstrap)
est store regmid
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

reg sd_geometric continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(bootstrap)
est store regsd
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

/*
** comparison of coefficients for the four bootstrap regression above
reg Deviation_NE continuous pure mm g8002 continuous_pure continuous_mm continuous_g8002 ///
    pure_mm pure_g8002 mm_g8002
est store regne
reg Deviation_MM continuous pure mm g8002 continuous_pure continuous_mm continuous_g8002 ///
    pure_mm pure_g8002 mm_g8002
est store regmm
reg Deviation_Mid continuous pure mm g8002 continuous_pure continuous_mm continuous_g8002 ///
    pure_mm pure_g8002 mm_g8002
est store regmid

suest regne regmm regmid, vce(cluster session_id)
test [regmm_mean]_cons = [regne_mean]_cons
test [regne_mean]_cons = [regmid_mean]_cons, accum
test [regne_mean]_cons = [regmid_mean]_cons
test [regne_mean]_cons-[regne_mean]mm = [regmid_mean]_cons-[regmid_mean]mm
*/



**********Fraction of time playing each cycles at pair level**********
* open dataset
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_summary_cycle.dta", clear

* generate treatment dummies
gen continuous = 0
replace continuous = 1 if time == "C"
gen pure = 0
replace pure = 1 if actionsets == "P"
gen mm = 0
replace mm = 1 if match == "mm"
gen AMPa = 0
replace AMPa = 1 if game == "AMPa"

gen continuous_pure = continuous * pure
gen continuous_mm = continuous * mm
gen continuous_AMPa = continuous * AMPa
gen pure_mm = pure * mm
gen pure_AMPa = pure * AMPa
gen mm_AMPa = mm * AMPa
gen continuous_pure_mm = continuous * pure * mm  
gen continuous_pure_AMPa = continuous * pure * AMPa
gen continuous_mm_AMPa = continuous * mm * AMPa
gen pure_mm_AMPa = pure * mm * AMPa
gen continuous_pure_mm_AMPa = continuous * pure * mm * AMPa 


** regressions with cluster std error at session level 
reg cw continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(cluster session_id)
outreg2 using D:\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

reg ccw continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(cluster session_id)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

reg diagonal continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(cluster session_id)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

reg stay continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(cluster session_id)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)


** regressions with bootstrap
reg cw continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(bootstrap)
outreg2 using D:\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

reg ccw continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(bootstrap)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

reg diagonal continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(bootstrap)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

reg stay continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(bootstrap)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)



**********T-test: data summary table with time averages (Table 6)**********
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_summary_cycle.dta", clear

gen diff_cw_ccw = cw - ccw
gen diff_cw_dd = cw - diagonal
gen diff_cw_cd = cw - cdiagonal
gen diff_cw_stay = cw - stay

* Panel A: AMPa games
reg diff_cw_ccw if game=="AMPa" & match=="mm", cluster(session_id)
reg diff_cw_dd if game=="AMPa" & match=="mm", cluster(session_id)
reg diff_cw_cd if game=="AMPa" & match=="mm", cluster(session_id)
reg diff_cw_stay if game=="AMPa" & match=="mm", cluster(session_id)

reg diff_cw_ccw if game=="AMPa" & match=="rp", cluster(session_id)
reg diff_cw_dd if game=="AMPa" & match=="rp", cluster(session_id)
reg diff_cw_cd if game=="AMPa" & match=="rp", cluster(session_id)
reg diff_cw_stay if game=="AMPa" & match=="rp", cluster(session_id)

reg diff_cw_ccw if game=="AMPa" & actionsets=="M", cluster(session_id)
reg diff_cw_dd if game=="AMPa" & actionsets=="M", cluster(session_id)
reg diff_cw_cd if game=="AMPa" & actionsets=="M", cluster(session_id)
reg diff_cw_stay if game=="AMPa" & actionsets=="M", cluster(session_id)

reg diff_cw_ccw if game=="AMPa" & actionsets=="P", cluster(session_id)
reg diff_cw_dd if game=="AMPa" & actionsets=="P", cluster(session_id)
reg diff_cw_cd if game=="AMPa" & actionsets=="P", cluster(session_id)
reg diff_cw_stay if game=="AMPa" & actionsets=="P", cluster(session_id)

reg diff_cw_ccw if game=="AMPa" & time=="C", cluster(session_id)
reg diff_cw_dd if game=="AMPa" & time=="C", cluster(session_id)
reg diff_cw_cd if game=="AMPa" & time=="C", cluster(session_id)
reg diff_cw_stay if game=="AMPa" & time=="C", cluster(session_id)

reg diff_cw_ccw if game=="AMPa" & time=="D", cluster(session_id)
reg diff_cw_dd if game=="AMPa" & time=="D", cluster(session_id)
reg diff_cw_cd if game=="AMPa" & time=="D", cluster(session_id)
reg diff_cw_stay if game=="AMPa" & time=="D", cluster(session_id)

* Panel B: AMPb games
reg diff_cw_ccw if game=="AMPb" & match=="mm", cluster(session_id)
reg diff_cw_dd if game=="AMPb" & match=="mm", cluster(session_id)
reg diff_cw_cd if game=="AMPb" & match=="mm", cluster(session_id)
reg diff_cw_stay if game=="AMPb" & match=="mm", cluster(session_id)

reg diff_cw_ccw if game=="AMPb" & match=="rp", cluster(session_id)
reg diff_cw_dd if game=="AMPb" & match=="rp", cluster(session_id)
reg diff_cw_cd if game=="AMPb" & match=="rp", cluster(session_id)
reg diff_cw_stay if game=="AMPb" & match=="rp", cluster(session_id)

reg diff_cw_ccw if game=="AMPb" & actionsets=="M", cluster(session_id)
reg diff_cw_dd if game=="AMPb" & actionsets=="M", cluster(session_id)
reg diff_cw_cd if game=="AMPb" & actionsets=="M", cluster(session_id)
reg diff_cw_stay if game=="AMPb" & actionsets=="M", cluster(session_id)

reg diff_cw_ccw if game=="AMPb" & actionsets=="P", cluster(session_id)
reg diff_cw_dd if game=="AMPb" & actionsets=="P", cluster(session_id)
reg diff_cw_cd if game=="AMPb" & actionsets=="P", cluster(session_id)
reg diff_cw_stay if game=="AMPb" & actionsets=="P", cluster(session_id)

reg diff_cw_ccw if game=="AMPb" & time=="C", cluster(session_id)
reg diff_cw_dd if game=="AMPb" & time=="C", cluster(session_id)
reg diff_cw_cd if game=="AMPb" & time=="C", cluster(session_id)
reg diff_cw_stay if game=="AMPb" & time=="C", cluster(session_id)

reg diff_cw_ccw if game=="AMPb" & time=="D", cluster(session_id)
reg diff_cw_dd if game=="AMPb" & time=="D", cluster(session_id)
reg diff_cw_cd if game=="AMPb" & time=="D", cluster(session_id)
reg diff_cw_stay if game=="AMPb" & time=="D", cluster(session_id)



**********Directional learning**********
** Bootstrap method without IDDS
* Row player learning
* regression in continuous time
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_production.dta", clear
drop if game_idds == 1
xtset session_round_pair_id tick
xtreg p1_diff p1_regret_sign p1_regret_sign_pure p1_regret_sign_mm p1_regret_sign_AMPa ///
      p1_regret_sign_pure_mm p1_regret_sign_pure_AMPa p1_regret_sign_mm_AMPa /// 
      if num_subperiods==0, fe vce(bootstrap)
outreg2 using D:\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

* regression in discrete time
xtreg p1_diff p1_regret_sign p1_regret_sign_pure p1_regret_sign_mm p1_regret_sign_AMPa ///
      p1_regret_sign_pure_mm p1_regret_sign_pure_AMPa p1_regret_sign_mm_AMPa ///
      if num_subperiods!=0, fe vce(bootstrap)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* Column player learning
* regression in continuous time
xtreg p2_diff p2_regret_sign p2_regret_sign_pure p2_regret_sign_mm p2_regret_sign_AMPa ///
      p2_regret_sign_pure_mm p2_regret_sign_pure_AMPa p2_regret_sign_mm_AMPa /// 
      if num_subperiods==0, fe vce(bootstrap)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* regression in discrete time
xtreg p2_diff p2_regret_sign p2_regret_sign_pure p2_regret_sign_mm p2_regret_sign_AMPa ///
      p2_regret_sign_pure_mm p2_regret_sign_pure_AMPa p2_regret_sign_mm_AMPa /// 
      if num_subperiods!=0, fe vce(bootstrap)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)


** Cluster std error without IDDS
* Row player learning
* regression in continuous time
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_production.dta", clear
drop if game_idds == 1
xtset session_round_pair_id tick
xtreg p1_diff p1_regret_sign p1_regret_sign_pure p1_regret_sign_mm p1_regret_sign_AMPa ///
      p1_regret_sign_pure_mm p1_regret_sign_pure_AMPa p1_regret_sign_mm_AMPa /// 
      if num_subperiods==0, fe vce(cluster session_code)
outreg2 using D:\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

* regression in discrete time
xtreg p1_diff p1_regret_sign p1_regret_sign_pure p1_regret_sign_mm p1_regret_sign_AMPa ///
      p1_regret_sign_pure_mm p1_regret_sign_pure_AMPa p1_regret_sign_mm_AMPa ///
      if num_subperiods!=0, fe vce(cluster session_code)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* Column player learning
* regression in continuous time
xtreg p2_diff p2_regret_sign p2_regret_sign_pure p2_regret_sign_mm p2_regret_sign_AMPa ///
      p2_regret_sign_pure_mm p2_regret_sign_pure_AMPa p2_regret_sign_mm_AMPa /// 
      if num_subperiods==0, fe vce(cluster session_code)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* regression in discrete time
xtreg p2_diff p2_regret_sign p2_regret_sign_pure p2_regret_sign_mm p2_regret_sign_AMPa ///
      p2_regret_sign_pure_mm p2_regret_sign_pure_AMPa p2_regret_sign_mm_AMPa /// 
      if num_subperiods!=0, fe vce(cluster session_code)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)



**********T-test and Regression: Directional learning estimating beta by treatments**********
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_production.dta", clear
drop if game_idds == 1
xtset session_round_pair_id tick

** row player estimation
* AMPa M C mm
xtreg p1_diff p1_regret_sign if game==1 & pure_strategy==0 & num_subperiods==0 & mean_matching==1, fe
* AMPa M C rp
xtreg p1_diff p1_regret_sign if game==1 & pure_strategy==0 & num_subperiods==0 & mean_matching==0, fe
* AMPa M D mm
xtreg p1_diff p1_regret_sign if game==1 & pure_strategy==0 & num_subperiods!=0 & mean_matching==1, fe
* AMPa M D rp
xtreg p1_diff p1_regret_sign if game==1 & pure_strategy==0 & num_subperiods!=0 & mean_matching==0, fe
* AMPa P C mm
xtreg p1_diff p1_regret_sign if game==1 & pure_strategy==1 & num_subperiods==0 & mean_matching==1, fe
* AMPa P C rp
xtreg p1_diff p1_regret_sign if game==1 & pure_strategy==1 & num_subperiods==0 & mean_matching==0, fe
* AMPa P D mm
xtreg p1_diff p1_regret_sign if game==1 & pure_strategy==1 & num_subperiods!=0 & mean_matching==1, fe
* AMPa P D rp
xtreg p1_diff p1_regret_sign if game==1 & pure_strategy==1 & num_subperiods!=0 & mean_matching==0, fe

* AMPb M C mm
xtreg p1_diff p1_regret_sign if game==2 & pure_strategy==0 & num_subperiods==0 & mean_matching==1, fe
* AMPb M C rp
xtreg p1_diff p1_regret_sign if game==2 & pure_strategy==0 & num_subperiods==0 & mean_matching==0, fe
* AMPb M D mm
xtreg p1_diff p1_regret_sign if game==2 & pure_strategy==0 & num_subperiods!=0 & mean_matching==1, fe
* AMPb M D rp
xtreg p1_diff p1_regret_sign if game==2 & pure_strategy==0 & num_subperiods!=0 & mean_matching==0, fe
* AMPb P C mm
xtreg p1_diff p1_regret_sign if game==2 & pure_strategy==1 & num_subperiods==0 & mean_matching==1, fe
* AMPb P C rp
xtreg p1_diff p1_regret_sign if game==2 & pure_strategy==1 & num_subperiods==0 & mean_matching==0, fe
* AMPb P D mm
xtreg p1_diff p1_regret_sign if game==2 & pure_strategy==1 & num_subperiods!=0 & mean_matching==1, fe
* AMPb P D rp
xtreg p1_diff p1_regret_sign if game==2 & pure_strategy==1 & num_subperiods!=0 & mean_matching==0, fe


** column player estimation
* AMPa M C mm
xtreg p2_diff p2_regret_sign if game==1 & pure_strategy==0 & num_subperiods==0 & mean_matching==1, fe
* AMPa M C rp
xtreg p2_diff p2_regret_sign if game==1 & pure_strategy==0 & num_subperiods==0 & mean_matching==0, fe
* AMPa M D mm
xtreg p2_diff p2_regret_sign if game==1 & pure_strategy==0 & num_subperiods!=0 & mean_matching==1, fe
* AMPa M D rp
xtreg p2_diff p2_regret_sign if game==1 & pure_strategy==0 & num_subperiods!=0 & mean_matching==0, fe
* AMPa P C mm
xtreg p2_diff p2_regret_sign if game==1 & pure_strategy==1 & num_subperiods==0 & mean_matching==1, fe
* AMPa P C rp
xtreg p2_diff p2_regret_sign if game==1 & pure_strategy==1 & num_subperiods==0 & mean_matching==0, fe
* AMPa P D mm
xtreg p2_diff p2_regret_sign if game==1 & pure_strategy==1 & num_subperiods!=0 & mean_matching==1, fe
* AMPa P D rp
xtreg p2_diff p2_regret_sign if game==1 & pure_strategy==1 & num_subperiods!=0 & mean_matching==0, fe

* AMPb M C mm
xtreg p2_diff p2_regret_sign if game==2 & pure_strategy==0 & num_subperiods==0 & mean_matching==1, fe
* AMPb M C rp
xtreg p2_diff p2_regret_sign if game==2 & pure_strategy==0 & num_subperiods==0 & mean_matching==0, fe
* AMPb M D mm
xtreg p2_diff p2_regret_sign if game==2 & pure_strategy==0 & num_subperiods!=0 & mean_matching==1, fe
* AMPb M D rp
xtreg p2_diff p2_regret_sign if game==2 & pure_strategy==0 & num_subperiods!=0 & mean_matching==0, fe
* AMPb P C mm
xtreg p2_diff p2_regret_sign if game==2 & pure_strategy==1 & num_subperiods==0 & mean_matching==1, fe
* AMPb P C rp
xtreg p2_diff p2_regret_sign if game==2 & pure_strategy==1 & num_subperiods==0 & mean_matching==0, fe
* AMPb P D mm
xtreg p2_diff p2_regret_sign if game==2 & pure_strategy==1 & num_subperiods!=0 & mean_matching==1, fe
* AMPb P D rp
xtreg p2_diff p2_regret_sign if game==2 & pure_strategy==1 & num_subperiods!=0 & mean_matching==0, fe


** regression for empirical data
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_summary_beta.dta", clear

reg Deviation_NE beta, cluster (session_id)
outreg2 using D:\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)
reg sd_geometric beta, cluster (session_id)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)


** regression for sim data
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_summary_sim.dta", clear

reg Deviation_NE beta
outreg2 using D:\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)
reg Dispersion_Geometric beta
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)



**********Directional learning (combine C and D treatments and rescale regret terms)**********
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_production.dta", clear
drop if game_idds == 1

* rescale regret terms by time treatments
gen p1_regret_sign_scale = p1_regret_sign
*replace p1_regret_sign_scale = p1_regret_sign / 2 if num_subperiods == 0
*replace p1_regret_sign_scale = p1_regret_sign * 6 if num_subperiods != 0
gen p2_regret_sign_scale = p2_regret_sign
*replace p2_regret_sign_scale = p2_regret_sign / 2 if num_subperiods == 0
*replace p2_regret_sign_scale = p2_regret_sign * 6 if num_subperiods != 0

* gen interactive dummies for p1
gen p1_continuous = p1_regret_sign_scale * dummy_continuous
gen p1_pure = p1_regret_sign_scale * dummy_pure
gen p1_mm = p1_regret_sign_scale * dummy_mm
gen p1_AMPa = p1_regret_sign_scale * dummy_8002
gen p1_continuous_pure = p1_regret_sign_scale * dummy_continuous * dummy_pure
gen p1_continuous_mm = p1_regret_sign_scale * dummy_continuous * dummy_mm
gen p1_continuous_AMPa = p1_regret_sign_scale * dummy_continuous * dummy_8002
gen p1_pure_mm = p1_regret_sign_scale * dummy_pure * dummy_mm
gen p1_pure_AMPa = p1_regret_sign_scale * dummy_pure * dummy_8002
gen p1_mm_AMPa = p1_regret_sign_scale * dummy_mm * dummy_8002

* gen interactive dummies for p2
gen p2_continuous = p2_regret_sign_scale * dummy_continuous
gen p2_pure = p2_regret_sign_scale * dummy_pure
gen p2_mm = p2_regret_sign_scale * dummy_mm
gen p2_AMPa = p2_regret_sign_scale * dummy_8002
gen p2_continuous_pure = p2_regret_sign_scale * dummy_continuous * dummy_pure
gen p2_continuous_mm = p2_regret_sign_scale * dummy_continuous * dummy_mm
gen p2_continuous_AMPa = p2_regret_sign_scale * dummy_continuous * dummy_8002
gen p2_pure_mm = p2_regret_sign_scale * dummy_pure * dummy_mm
gen p2_pure_AMPa = p2_regret_sign_scale * dummy_pure * dummy_8002
gen p2_mm_AMPa = p2_regret_sign_scale * dummy_mm * dummy_8002

* run regressions
xtset session_round_pair_id tick
xtreg p1_diff p1_regret_sign_scale p1_continuous p1_pure p1_mm p1_AMPa ///
      p1_continuous_pure p1_continuous_mm p1_continuous_AMPa ///
	  p1_pure_mm p1_pure_AMPa p1_mm_AMPa, fe vce(robust)
outreg2 using D:\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

xtreg p1_diff p1_regret_sign_scale p1_continuous p1_pure p1_mm p1_AMPa ///
      p1_continuous_pure p1_continuous_mm p1_continuous_AMPa ///
	  p1_pure_mm p1_pure_AMPa p1_mm_AMPa, fe vce(bootstrap)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

xtreg p2_diff p2_regret_sign_scale p2_continuous p2_pure p2_mm p2_AMPa ///
      p2_continuous_pure p2_continuous_mm p2_continuous_AMPa ///
	  p2_pure_mm p2_pure_AMPa p2_mm_AMPa, fe vce(robust)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

xtreg p2_diff p2_regret_sign_scale p2_continuous p2_pure p2_mm p2_AMPa ///
      p2_continuous_pure p2_continuous_mm p2_continuous_AMPa ///
	  p2_pure_mm p2_pure_AMPa p2_mm_AMPa, fe vce(bootstrap)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)



**********BR learning**********
* add independent variables
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_production.dta", clear

gen p1_direction_pure = p1_direction * dummy_pure
gen p1_direction_mm = p1_direction * dummy_mm
gen p1_direction_AMPa = p1_direction * dummy_AMPa
gen p1_direction_IDDS = p1_direction * dummy_IDDS
gen p1_direction_pure_mm = p1_direction * dummy_pure * dummy_mm
gen p1_direction_pure_AMPa = p1_direction * dummy_pure * dummy_AMPa
gen p1_direction_pure_IDDS = p1_direction * dummy_pure * dummy_IDDS
gen p1_direction_mm_AMPa = p1_direction * dummy_mm * dummy_AMPa
gen p1_direction_mm_IDDS = p1_direction * dummy_mm * dummy_IDDS
gen p1_direction_pure_mm_AMPa = p1_direction * dummy_pure * dummy_mm * dummy_AMPa
gen p1_direction_pure_mm_IDDS = p1_direction * dummy_pure * dummy_mm * dummy_IDDS

gen p2_direction_pure = p2_direction * dummy_pure
gen p2_direction_mm = p2_direction * dummy_mm
gen p2_direction_AMPa = p2_direction * dummy_AMPa
gen p2_direction_IDDS = p2_direction * dummy_IDDS
gen p2_direction_pure_mm = p2_direction * dummy_pure * dummy_mm
gen p2_direction_pure_AMPa = p2_direction * dummy_pure * dummy_AMPa
gen p2_direction_pure_IDDS = p2_direction * dummy_pure * dummy_IDDS
gen p2_direction_mm_AMPa = p2_direction * dummy_mm * dummy_AMPa
gen p2_direction_mm_IDDS = p2_direction * dummy_mm * dummy_IDDS
gen p2_direction_pure_mm_AMPa = p2_direction * dummy_pure * dummy_mm * dummy_AMPa
gen p2_direction_pure_mm_IDDS = p2_direction * dummy_pure * dummy_mm * dummy_IDDS

* Row player learning
drop if game_idds == 1

* regression in continuous time
xtset session_round_pair_id tick
xtreg p1_diff p1_direction p1_direction_pure p1_direction_mm p1_direction_AMPa ///
      p1_direction_pure_mm p1_direction_pure_AMPa p1_direction_mm_AMPa /// 
      if num_subperiods==0, fe vce(cluster session_code)
outreg2 using D:\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

* regression in discrete time
xtreg p1_diff p1_direction p1_direction_pure p1_direction_mm p1_direction_AMPa ///
      p1_direction_pure_mm p1_direction_pure_AMPa p1_direction_mm_AMPa ///
      if num_subperiods!=0, fe vce(cluster session_code)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* Column player learning
* regression in continuous time
xtreg p2_diff p2_direction p2_direction_pure p2_direction_mm p2_direction_AMPa ///
      p2_direction_pure_mm p2_direction_pure_AMPa p2_direction_mm_AMPa /// 
      if num_subperiods==0, fe vce(cluster session_code)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* regression in discrete time
xtreg p2_diff p2_direction p2_direction_pure p2_direction_mm p2_direction_AMPa ///
      p2_direction_pure_mm p2_direction_pure_AMPa p2_direction_mm_AMPa /// 
      if num_subperiods!=0, fe vce(cluster session_code)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)



**********Pure directional learning**********
* add independent variables
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_production.dta", clear

gen p1_sign_pure = p1_sign * dummy_pure
gen p1_sign_mm = p1_sign * dummy_mm
gen p1_sign_AMPa = p1_sign * dummy_AMPa
gen p1_sign_IDDS = p1_sign * dummy_IDDS
gen p1_sign_pure_mm = p1_sign * dummy_pure * dummy_mm
gen p1_sign_pure_AMPa = p1_sign * dummy_pure * dummy_AMPa
gen p1_sign_pure_IDDS = p1_sign * dummy_pure * dummy_IDDS
gen p1_sign_mm_AMPa = p1_sign * dummy_mm * dummy_AMPa
gen p1_sign_mm_IDDS = p1_sign * dummy_mm * dummy_IDDS
gen p1_sign_pure_mm_AMPa = p1_sign * dummy_pure * dummy_mm * dummy_AMPa
gen p1_sign_pure_mm_IDDS = p1_sign * dummy_pure * dummy_mm * dummy_IDDS

gen p2_sign_pure = p2_sign * dummy_pure
gen p2_sign_mm = p2_sign * dummy_mm
gen p2_sign_AMPa = p2_sign * dummy_AMPa
gen p2_sign_IDDS = p2_sign * dummy_IDDS
gen p2_sign_pure_mm = p2_sign * dummy_pure * dummy_mm
gen p2_sign_pure_AMPa = p2_sign * dummy_pure * dummy_AMPa
gen p2_sign_pure_IDDS = p2_sign * dummy_pure * dummy_IDDS
gen p2_sign_mm_AMPa = p2_sign * dummy_mm * dummy_AMPa
gen p2_sign_mm_IDDS = p2_sign * dummy_mm * dummy_IDDS
gen p2_sign_pure_mm_AMPa = p2_sign * dummy_pure * dummy_mm * dummy_AMPa
gen p2_sign_pure_mm_IDDS = p2_sign * dummy_pure * dummy_mm * dummy_IDDS

* Row player learning
drop if game_idds == 1

* regression in continuous time
xtset session_round_pair_id tick
xtreg p1_diff p1_sign p1_sign_pure p1_sign_mm p1_sign_AMPa ///
      p1_sign_pure_mm p1_sign_pure_AMPa p1_sign_mm_AMPa /// 
      if num_subperiods==0, fe vce(cluster session_code)
outreg2 using D:\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

* regression in discrete time
xtreg p1_diff p1_sign p1_sign_pure p1_sign_mm p1_sign_AMPa ///
      p1_sign_pure_mm p1_sign_pure_AMPa p1_sign_mm_AMPa ///
      if num_subperiods!=0, fe vce(cluster session_code)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* Column player learning
* regression in continuous time
xtreg p2_diff p2_sign p2_sign_pure p2_sign_mm p2_sign_AMPa ///
      p2_sign_pure_mm p2_sign_pure_AMPa p2_sign_mm_AMPa /// 
      if num_subperiods==0, fe vce(cluster session_code)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* regression in discrete time
xtreg p2_diff p2_sign p2_sign_pure p2_sign_mm p2_sign_AMPa ///
      p2_sign_pure_mm p2_sign_pure_AMPa p2_sign_mm_AMPa /// 
      if num_subperiods!=0, fe vce(cluster session_code)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)



**********Directional learning with lagged regret terms (not used)**********
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_production.dta", clear
xtset session_round_pair_id tick

* generate lag regret terms for row player
gen p1_regret_sign_L1 = L.p1_regret_sign
gen p1_regret_sign_L2 = L2.p1_regret_sign
gen p1_regret_sign_L3 = L3.p1_regret_sign
gen p1_regret_sign_L4 = L4.p1_regret_sign
gen p1_regret_sign_L5 = L5.p1_regret_sign
gen p1_regret_sign_L6 = L6.p1_regret_sign
gen p1_regret_sign_L7 = L7.p1_regret_sign
gen p1_regret_sign_L8 = L8.p1_regret_sign
gen p1_regret_sign_L9 = L9.p1_regret_sign
gen p1_regret_sign_L10 = L10.p1_regret_sign

* generate lag regret terms for column player
gen p2_regret_sign_L1 = L.p2_regret_sign
gen p2_regret_sign_L2 = L2.p2_regret_sign
gen p2_regret_sign_L3 = L3.p2_regret_sign
gen p2_regret_sign_L4 = L4.p2_regret_sign
gen p2_regret_sign_L5 = L5.p2_regret_sign
gen p2_regret_sign_L6 = L6.p2_regret_sign
gen p2_regret_sign_L7 = L7.p2_regret_sign
gen p2_regret_sign_L8 = L8.p2_regret_sign
gen p2_regret_sign_L9 = L9.p2_regret_sign
gen p2_regret_sign_L10 = L10.p2_regret_sign

* regression with 5 lagged terms
xtreg p1_diff p1_regret_sign p1_regret_sign_L1 p1_regret_sign_L2 ///
      p1_regret_sign_L3 p1_regret_sign_L4 p1_regret_sign_L5 ///
      if num_subperiods!=0, fe vce(robust)
outreg2 using D:\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

xtreg p1_diff p1_regret_sign p1_regret_sign_L1 p1_regret_sign_L2 ///
      p1_regret_sign_L3 p1_regret_sign_L4 p1_regret_sign_L5 ///
      if num_subperiods==0, fe vce(robust)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

xtreg p2_diff p2_regret_sign p2_regret_sign_L1 p2_regret_sign_L2 ///
      p2_regret_sign_L3 p2_regret_sign_L4 p2_regret_sign_L5 ///
      if num_subperiods!=0, fe vce(robust)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

xtreg p2_diff p2_regret_sign p2_regret_sign_L1 p2_regret_sign_L2 ///
      p2_regret_sign_L3 p2_regret_sign_L4 p2_regret_sign_L5 ///
      if num_subperiods==0, fe vce(robust)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)


* regression with 1 lagged terms
xtreg p1_diff p1_regret_sign p1_regret_sign_L1 ///
      if num_subperiods!=0, fe vce(robust)
outreg2 using D:\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

xtreg p1_diff p1_regret_sign p1_regret_sign_L1 ///
      if num_subperiods==0, fe vce(robust)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

xtreg p2_diff p2_regret_sign p2_regret_sign_L1 ///
      if num_subperiods!=0, fe vce(robust)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

xtreg p2_diff p2_regret_sign p2_regret_sign_L1 ///
      if num_subperiods==0, fe vce(robust)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)



**********Directional learning with discrete (small, medium, large) regret (not used)**********
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_production.dta", clear
xtset session_round_pair_id tick

* reform the regret data
gen row_regret = 0
replace row_regret = 1 if p1_regret <= 0.33
replace row_regret = 2 if p1_regret < 0.67 & p1_regret > 0.33
replace row_regret = 3 if p1_regret >= 0.67

gen col_regret = 0
replace col_regret = 1 if p2_regret <= 0.33
replace col_regret = 2 if p2_regret < 0.67 & p2_regret > 0.33
replace col_regret = 3 if p2_regret >= 0.67

gen row_regret_sign = row_regret * p1_sign
gen col_regret_sign = col_regret * p2_sign

gen row_regret_sign_pure = row_regret_sign * dummy_pure
gen row_regret_sign_mm = row_regret_sign * dummy_mm
gen row_regret_sign_8002 = row_regret_sign * dummy_8002
gen row_regret_sign_IDDS = row_regret_sign * dummy_IDDS
gen row_regret_sign_pure_mm = row_regret_sign * dummy_pure * dummy_mm
gen row_regret_sign_pure_8002 = row_regret_sign * dummy_pure * dummy_8002
gen row_regret_sign_pure_IDDS = row_regret_sign * dummy_pure * dummy_IDDS
gen row_regret_sign_mm_8002 = row_regret_sign * dummy_mm * dummy_8002
gen row_regret_sign_mm_IDDS = row_regret_sign * dummy_mm * dummy_IDDS
gen row_regret_sign_pure_mm_8002 = row_regret_sign * dummy_pure * dummy_mm * dummy_8002
gen row_regret_sign_pure_mm_IDDS = row_regret_sign * dummy_pure * dummy_mm * dummy_IDDS

gen col_regret_sign_pure = col_regret_sign * dummy_pure
gen col_regret_sign_mm = col_regret_sign * dummy_mm
gen col_regret_sign_8002 = col_regret_sign * dummy_8002
gen col_regret_sign_IDDS = col_regret_sign * dummy_IDDS
gen col_regret_sign_pure_mm = col_regret_sign * dummy_pure * dummy_mm
gen col_regret_sign_pure_8002 = col_regret_sign * dummy_pure * dummy_8002
gen col_regret_sign_pure_IDDS = col_regret_sign * dummy_pure * dummy_IDDS
gen col_regret_sign_mm_8002 = col_regret_sign * dummy_mm * dummy_8002
gen col_regret_sign_mm_IDDS = col_regret_sign * dummy_mm * dummy_IDDS
gen col_regret_sign_pure_mm_8002 = col_regret_sign * dummy_pure * dummy_mm * dummy_8002
gen col_regret_sign_pure_mm_IDDS = col_regret_sign * dummy_pure * dummy_mm * dummy_IDDS

* Row player learning
* regression in continuous time
xtset session_round_pair_id tick
xtreg p1_diff row_regret_sign row_regret_sign_pure row_regret_sign_mm row_regret_sign_8002 row_regret_sign_IDDS ///
      row_regret_sign_pure_mm row_regret_sign_pure_8002 row_regret_sign_pure_IDDS row_regret_sign_mm_8002 row_regret_sign_mm_IDDS /// 
      row_regret_sign_pure_mm_8002 row_regret_sign_pure_mm_IDDS if num_subperiods==0, fe vce(robust)
outreg2 using D:\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

* regression in discrete time
xtreg p1_diff row_regret_sign row_regret_sign_pure row_regret_sign_mm row_regret_sign_8002 row_regret_sign_IDDS ///
      row_regret_sign_pure_mm row_regret_sign_pure_8002 row_regret_sign_pure_IDDS row_regret_sign_mm_8002 row_regret_sign_mm_IDDS /// 
      row_regret_sign_pure_mm_8002 row_regret_sign_pure_mm_IDDS if num_subperiods!=0, fe vce(robust)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* Column player learning
* regression in continuous time
xtreg p2_diff col_regret_sign col_regret_sign_pure col_regret_sign_mm col_regret_sign_8002 col_regret_sign_IDDS ///
      col_regret_sign_pure_mm col_regret_sign_pure_8002 col_regret_sign_pure_IDDS col_regret_sign_mm_8002 col_regret_sign_mm_IDDS /// 
      col_regret_sign_pure_mm_8002 col_regret_sign_pure_mm_IDDS if num_subperiods==0, fe vce(robust)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* regression in discrete time
xtreg p2_diff col_regret_sign col_regret_sign_pure col_regret_sign_mm col_regret_sign_8002 col_regret_sign_IDDS ///
      col_regret_sign_pure_mm col_regret_sign_pure_8002 col_regret_sign_pure_IDDS col_regret_sign_mm_8002 col_regret_sign_mm_IDDS /// 
      col_regret_sign_pure_mm_8002 col_regret_sign_pure_mm_IDDS if num_subperiods!=0, fe vce(robust)
outreg2 using D:\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

