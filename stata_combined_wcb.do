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

* generate individual id
gen individual = _n

* Panel A: AMPa games
reg diff_ne_mid if game=="AMPa" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="AMPa" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_ne_mid if game=="AMPa" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="AMPa" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_ne_mid if game=="AMPa" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="AMPa" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_ne_mid if game=="AMPa" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="AMPa" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_ne_mid if game=="AMPa" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="AMPa" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_ne_mid if game=="AMPa" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="AMPa" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

* Panel B: AMPb games
reg diff_ne_mid if game=="AMPb" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="AMPb" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_ne_mid if game=="AMPb" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="AMPb" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_ne_mid if game=="AMPb" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="AMPb" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_ne_mid if game=="AMPb" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="AMPb" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_ne_mid if game=="AMPb" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="AMPb" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_ne_mid if game=="AMPb" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="AMPb" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

* Panel C: IDDS games
reg diff_ne_mid if game=="IDDS" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="IDDS" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_ne_mid if game=="IDDS" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="IDDS" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_ne_mid if game=="IDDS" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="IDDS" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_ne_mid if game=="IDDS" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="IDDS" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_ne_mid if game=="IDDS" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="IDDS" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_ne_mid if game=="IDDS" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_mid_mm if game=="IDDS" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)



**********T-test: signed distance to NE, MM, and Center for row and column players (Table 3)**********
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_summary.dta", clear

* generate signed distance dependent variables
gen Diffp1NE = p1_average - p1NEmix
gen Diffp2NE = p2_average - p2NEmix
gen Diffp1MM = p1_average - p1MMmix
gen Diffp2MM = p2_average - p2MMmix
gen Diffp1Center = p1_average - 0.5
gen Diffp2Center = p2_average - 0.5

* generate individual id
gen individual = _n

* Panel A: AMPa games
reg Diffp1NE if game=="AMPa" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="AMPa" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="AMPa" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="AMPa" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="AMPa" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="AMPa" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg Diffp1NE if game=="AMPa" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="AMPa" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="AMPa" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="AMPa" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="AMPa" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="AMPa" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg Diffp1NE if game=="AMPa" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="AMPa" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="AMPa" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="AMPa" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="AMPa" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="AMPa" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg Diffp1NE if game=="AMPa" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="AMPa" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="AMPa" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="AMPa" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="AMPa" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="AMPa" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg Diffp1NE if game=="AMPa" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="AMPa" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="AMPa" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="AMPa" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="AMPa" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="AMPa" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg Diffp1NE if game=="AMPa" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="AMPa" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="AMPa" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="AMPa" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="AMPa" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="AMPa" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

* Panel B: AMPb games
reg Diffp1NE if game=="AMPb" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="AMPb" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="AMPb" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="AMPb" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="AMPb" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="AMPb" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg Diffp1NE if game=="AMPb" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="AMPb" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="AMPb" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="AMPb" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="AMPb" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="AMPb" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg Diffp1NE if game=="AMPb" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="AMPb" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="AMPb" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="AMPb" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="AMPb" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="AMPb" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg Diffp1NE if game=="AMPb" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="AMPb" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="AMPb" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="AMPb" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="AMPb" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="AMPb" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg Diffp1NE if game=="AMPb" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="AMPb" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="AMPb" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="AMPb" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="AMPb" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="AMPb" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg Diffp1NE if game=="AMPb" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="AMPb" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="AMPb" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="AMPb" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="AMPb" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="AMPb" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

* Panel C: IDDS games
reg Diffp1NE if game=="IDDS" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="IDDS" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="IDDS" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="IDDS" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="IDDS" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="IDDS" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg Diffp1NE if game=="IDDS" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="IDDS" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="IDDS" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="IDDS" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="IDDS" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="IDDS" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg Diffp1NE if game=="IDDS" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="IDDS" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="IDDS" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="IDDS" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="IDDS" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="IDDS" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg Diffp1NE if game=="IDDS" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="IDDS" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="IDDS" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="IDDS" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="IDDS" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="IDDS" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg Diffp1NE if game=="IDDS" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="IDDS" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="IDDS" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="IDDS" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="IDDS" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="IDDS" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg Diffp1NE if game=="IDDS" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2NE if game=="IDDS" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1MM if game=="IDDS" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2MM if game=="IDDS" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp1Center if game=="IDDS" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg Diffp2Center if game=="IDDS" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)



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

* generate individual id
gen individual = _n

** regressions short table
reg Deviation_NE continuous pure mm AMPa, vce(cluster session_id)
boottest {continuous} {pure} {mm} {AMPa} {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) replace nolabel bdec(2)

reg Deviation_MM continuous pure mm AMPa, vce(cluster session_id)
boottest {continuous} {pure} {mm} {AMPa} {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)

reg Deviation_Mid continuous pure mm AMPa, vce(cluster session_id)
boottest {continuous} {pure} {mm} {AMPa} {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)

reg sd_geometric continuous pure mm AMPa, vce(cluster session_id)
boottest {continuous} {pure} {mm} {AMPa} {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)


** regressions full table
reg Deviation_NE continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(cluster session_id)
boottest {continuous} {pure} {mm} {AMPa} {continuous_pure} {continuous_mm} {continuous_AMPa} {pure_mm} {pure_AMPa} {mm_AMPa} {_cons} ///
, noci nograph weight(webb) cluster(session_id) bootcl(individual)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) replace nolabel bdec(2)

reg Deviation_MM continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(cluster session_id)
boottest {continuous} {pure} {mm} {AMPa} {continuous_pure} {continuous_mm} {continuous_AMPa} {pure_mm} {pure_AMPa} {mm_AMPa} {_cons} ///
, noci nograph weight(webb) cluster(session_id) bootcl(individual)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)

reg Deviation_Mid continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(cluster session_id)
boottest {continuous} {pure} {mm} {AMPa} {continuous_pure} {continuous_mm} {continuous_AMPa} {pure_mm} {pure_AMPa} {mm_AMPa} {_cons} ///
, noci nograph weight(webb) cluster(session_id) bootcl(individual)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)

reg sd_geometric continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(cluster session_id)
boottest {continuous} {pure} {mm} {AMPa} {continuous_pure} {continuous_mm} {continuous_AMPa} {pure_mm} {pure_AMPa} {mm_AMPa} {_cons} ///
, noci nograph weight(webb) cluster(session_id) bootcl(individual)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)



**********Fraction of time playing each cycles at pair level (Appendix)**********
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

* generate individual id
gen individual = _n

** regressions with cluster std error at session level 
reg cw continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(cluster session_id)
boottest {continuous} {pure} {mm} {AMPa} {continuous_pure} {continuous_mm} {continuous_AMPa} {pure_mm} {pure_AMPa} {mm_AMPa} {_cons} ///
, noci nograph weight(webb) cluster(session_id) bootcl(individual)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) replace nolabel bdec(2)

reg ccw continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(cluster session_id)
boottest {continuous} {pure} {mm} {AMPa} {continuous_pure} {continuous_mm} {continuous_AMPa} {pure_mm} {pure_AMPa} {mm_AMPa} {_cons} ///
, noci nograph weight(webb) cluster(session_id) bootcl(individual)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)

reg diagonal continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(cluster session_id)
boottest {continuous} {pure} {mm} {AMPa} {continuous_pure} {continuous_mm} {continuous_AMPa} {pure_mm} {pure_AMPa} {mm_AMPa} {_cons} ///
, noci nograph weight(webb) cluster(session_id) bootcl(individual)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)

reg stay continuous pure mm AMPa continuous_pure continuous_mm continuous_AMPa ///
    pure_mm pure_AMPa mm_AMPa, vce(cluster session_id)
boottest {continuous} {pure} {mm} {AMPa} {continuous_pure} {continuous_mm} {continuous_AMPa} {pure_mm} {pure_AMPa} {mm_AMPa} {_cons} ///
, noci nograph weight(webb) cluster(session_id) bootcl(individual)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)



**********T-test: data summary table with time averages (Table 6)**********
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_summary_cycle.dta", clear

gen diff_cw_ccw = cw - ccw
gen diff_cw_dd = cw - diagonal
gen diff_cw_cd = cw - cdiagonal
gen diff_cw_stay = cw - stay

* generate individual id
gen individual = _n

* Panel A: AMPa games
reg diff_cw_ccw if game=="AMPa" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_dd if game=="AMPa" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_cd if game=="AMPa" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_stay if game=="AMPa" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_cw_ccw if game=="AMPa" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_dd if game=="AMPa" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_cd if game=="AMPa" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_stay if game=="AMPa" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_cw_ccw if game=="AMPa" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_dd if game=="AMPa" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_cd if game=="AMPa" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_stay if game=="AMPa" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_cw_ccw if game=="AMPa" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_dd if game=="AMPa" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_cd if game=="AMPa" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_stay if game=="AMPa" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_cw_ccw if game=="AMPa" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_dd if game=="AMPa" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_cd if game=="AMPa" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_stay if game=="AMPa" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_cw_ccw if game=="AMPa" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_dd if game=="AMPa" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_cd if game=="AMPa" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_stay if game=="AMPa" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

* Panel B: AMPb games
reg diff_cw_ccw if game=="AMPb" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_dd if game=="AMPb" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_cd if game=="AMPb" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_stay if game=="AMPb" & match=="mm", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_cw_ccw if game=="AMPb" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_dd if game=="AMPb" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_cd if game=="AMPb" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_stay if game=="AMPb" & match=="rp", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_cw_ccw if game=="AMPb" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_dd if game=="AMPb" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_cd if game=="AMPb" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_stay if game=="AMPb" & actionsets=="M", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_cw_ccw if game=="AMPb" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_dd if game=="AMPb" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_cd if game=="AMPb" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_stay if game=="AMPb" & actionsets=="P", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_cw_ccw if game=="AMPb" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_dd if game=="AMPb" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_cd if game=="AMPb" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_stay if game=="AMPb" & time=="C", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)

reg diff_cw_ccw if game=="AMPb" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_dd if game=="AMPb" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_cd if game=="AMPb" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)
reg diff_cw_stay if game=="AMPb" & time=="D", cluster(session_id)
boottest {_cons}, noci nograph weight(webb) cluster(session_id) bootcl(individual)



**********Directional learning (Table 7)**********
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_production.dta", clear
drop if game_idds == 1

* generate individual id
gen individual = _n

* set up panel data
xtset session_round_pair_id tick


** short table
* regression in continuous time
preserve
keep if num_subperiods==0
xtreg p1_diff p1_regret_sign p1_regret_sign_pure p1_regret_sign_mm p1_regret_sign_AMPa ///
      , fe vce(cluster session_code)
boottest {p1_regret_sign} {p1_regret_sign_pure} {p1_regret_sign_mm} {p1_regret_sign_AMPa} ///
         , noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) replace nolabel bdec(2)
restore

* regression in discrete time
preserve
keep if num_subperiods!=0
xtreg p1_diff p1_regret_sign p1_regret_sign_pure p1_regret_sign_mm p1_regret_sign_AMPa ///
      , fe vce(cluster session_code)
boottest {p1_regret_sign} {p1_regret_sign_pure} {p1_regret_sign_mm} {p1_regret_sign_AMPa} ///
         , noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)
restore

* Column player learning
* regression in continuous time
preserve
keep if num_subperiods==0
xtreg p2_diff p2_regret_sign p2_regret_sign_pure p2_regret_sign_mm p1_regret_sign_AMPa ///
      , fe vce(cluster session_code)
boottest {p2_regret_sign} {p2_regret_sign_pure} {p2_regret_sign_mm} {p1_regret_sign_AMPa} ///
         , noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)
restore

* regression in discrete time
preserve
keep if num_subperiods!=0
xtreg p2_diff p2_regret_sign p2_regret_sign_pure p2_regret_sign_mm p1_regret_sign_AMPa ///
      , fe vce(cluster session_code)
boottest {p2_regret_sign} {p2_regret_sign_pure} {p2_regret_sign_mm} {p1_regret_sign_AMPa} ///
         , noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)
restore


** full table
* regression in continuous time
preserve
keep if num_subperiods==0
xtreg p1_diff p1_regret_sign p1_regret_sign_pure p1_regret_sign_mm p1_regret_sign_AMPa ///
      p1_regret_sign_pure_mm p1_regret_sign_pure_AMPa p1_regret_sign_mm_AMPa /// 
      , fe vce(cluster session_code)
boottest {p1_regret_sign} {p1_regret_sign_pure} {p1_regret_sign_mm} {p1_regret_sign_AMPa} ///
         {p1_regret_sign_pure_mm} {p1_regret_sign_pure_AMPa} {p1_regret_sign_mm_AMPa} /// 
         , noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) replace nolabel bdec(2)
restore

* regression in discrete time
preserve
keep if num_subperiods!=0
xtreg p1_diff p1_regret_sign p1_regret_sign_pure p1_regret_sign_mm p1_regret_sign_AMPa ///
      p1_regret_sign_pure_mm p1_regret_sign_pure_AMPa p1_regret_sign_mm_AMPa ///
      , fe vce(cluster session_code)
boottest {p1_regret_sign} {p1_regret_sign_pure} {p1_regret_sign_mm} {p1_regret_sign_AMPa} ///
         {p1_regret_sign_pure_mm} {p1_regret_sign_pure_AMPa} {p1_regret_sign_mm_AMPa} /// 
         , noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)
restore

* Column player learning
* regression in continuous time
preserve
keep if num_subperiods==0
xtreg p2_diff p2_regret_sign p2_regret_sign_pure p2_regret_sign_mm p2_regret_sign_AMPa ///
      p2_regret_sign_pure_mm p2_regret_sign_pure_AMPa p2_regret_sign_mm_AMPa /// 
      , fe vce(cluster session_code)
boottest {p2_regret_sign} {p2_regret_sign_pure} {p2_regret_sign_mm} {p2_regret_sign_AMPa} ///
         {p2_regret_sign_pure_mm} {p2_regret_sign_pure_AMPa} {p2_regret_sign_mm_AMPa} /// 
         , noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)
restore

* regression in discrete time
preserve
keep if num_subperiods!=0
xtreg p2_diff p2_regret_sign p2_regret_sign_pure p2_regret_sign_mm p2_regret_sign_AMPa ///
      p2_regret_sign_pure_mm p2_regret_sign_pure_AMPa p2_regret_sign_mm_AMPa /// 
      , fe vce(cluster session_code)
boottest {p2_regret_sign} {p2_regret_sign_pure} {p2_regret_sign_mm} {p2_regret_sign_AMPa} ///
         {p2_regret_sign_pure_mm} {p2_regret_sign_pure_AMPa} {p2_regret_sign_mm_AMPa} /// 
         , noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)
restore



**********BR learning (Appendix)**********
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

* set up panel data
xtset session_round_pair_id tick

* regression in continuous time
preserve
keep if num_subperiods==0
xtreg p1_diff p1_direction p1_direction_pure p1_direction_mm p1_direction_AMPa ///
      p1_direction_pure_mm p1_direction_pure_AMPa p1_direction_mm_AMPa /// 
      , fe vce(cluster session_code)
boottest {p1_direction} {p1_direction_pure} {p1_direction_mm} {p1_direction_AMPa} ///
         {p1_direction_pure_mm} {p1_direction_pure_AMPa} {p1_direction_mm_AMPa} /// 
         , noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) replace nolabel bdec(2)
restore

* regression in discrete time
preserve
keep if num_subperiods!=0
xtreg p1_diff p1_direction p1_direction_pure p1_direction_mm p1_direction_AMPa ///
      p1_direction_pure_mm p1_direction_pure_AMPa p1_direction_mm_AMPa /// 
      , fe vce(cluster session_code)
boottest {p1_direction} {p1_direction_pure} {p1_direction_mm} {p1_direction_AMPa} ///
         {p1_direction_pure_mm} {p1_direction_pure_AMPa} {p1_direction_mm_AMPa} /// 
         , noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)
restore

* Column player learning
* regression in continuous time
preserve
keep if num_subperiods==0
xtreg p2_diff p2_direction p2_direction_pure p2_direction_mm p2_direction_AMPa ///
      p2_direction_pure_mm p2_direction_pure_AMPa p2_direction_mm_AMPa /// 
      , fe vce(cluster session_code)
boottest {p2_direction} {p2_direction_pure} {p2_direction_mm} {p2_direction_AMPa} ///
         {p2_direction_pure_mm} {p2_direction_pure_AMPa} {p2_direction_mm_AMPa} /// 
         , noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)
restore

* regression in discrete time
preserve
keep if num_subperiods!=0
xtreg p2_diff p2_direction p2_direction_pure p2_direction_mm p2_direction_AMPa ///
      p2_direction_pure_mm p2_direction_pure_AMPa p2_direction_mm_AMPa /// 
      , fe vce(cluster session_code)
boottest {p2_direction} {p2_direction_pure} {p2_direction_mm} {p2_direction_AMPa} ///
         {p2_direction_pure_mm} {p2_direction_pure_AMPa} {p2_direction_mm_AMPa} /// 
         , noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)
restore



**********Pure directional learning (Appendix)**********
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

* set up panel data
xtset session_round_pair_id tick

* regression in continuous time
preserve
keep if num_subperiods==0
xtreg p1_diff p1_sign p1_sign_pure p1_sign_mm p1_sign_AMPa ///
      p1_sign_pure_mm p1_sign_pure_AMPa p1_sign_mm_AMPa /// 
      , fe vce(cluster session_code)
boottest {p1_sign} {p1_sign_pure} {p1_sign_mm} {p1_sign_AMPa} ///
         {p1_sign_pure_mm} {p1_sign_pure_AMPa} {p1_sign_mm_AMPa} /// 
         , noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) replace nolabel bdec(2)
restore

* regression in discrete time
preserve
keep if num_subperiods!=0
xtreg p1_diff p1_sign p1_sign_pure p1_sign_mm p1_sign_AMPa ///
      p1_sign_pure_mm p1_sign_pure_AMPa p1_sign_mm_AMPa /// 
      , fe vce(cluster session_code)
boottest {p1_sign} {p1_sign_pure} {p1_sign_mm} {p1_sign_AMPa} ///
         {p1_sign_pure_mm} {p1_sign_pure_AMPa} {p1_sign_mm_AMPa} /// 
         , noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)
restore

* Column player learning
* regression in continuous time
preserve
keep if num_subperiods==0
xtreg p2_diff p2_sign p2_sign_pure p2_sign_mm p2_sign_AMPa ///
      p2_sign_pure_mm p2_sign_pure_AMPa p2_sign_mm_AMPa /// 
      , fe vce(cluster session_code)
boottest {p2_sign} {p2_sign_pure} {p2_sign_mm} {p2_sign_AMPa} ///
         {p2_sign_pure_mm} {p2_sign_pure_AMPa} {p2_sign_mm_AMPa} /// 
         , noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)
restore

* regression in discrete time
preserve
keep if num_subperiods!=0
xtreg p2_diff p2_sign p2_sign_pure p2_sign_mm p2_sign_AMPa ///
      p2_sign_pure_mm p2_sign_pure_AMPa p2_sign_mm_AMPa /// 
      , fe vce(cluster session_code)
boottest {p2_sign} {p2_sign_pure} {p2_sign_mm} {p2_sign_AMPa} ///
         {p2_sign_pure_mm} {p2_sign_pure_AMPa} {p2_sign_mm_AMPa} /// 
         , noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)
restore



**********Directional learning with F test for each treatment**********
use "D:/Dropbox/Working Papers/When Are Mixed Equilibria Relevant/data/production/mp_production.dta", clear
drop if game_idds == 1

* generate individual id
gen individual = _n

* set up panel data
xtset session_round_pair_id tick

* set up p value matrix
matrix P = J(8,4,0)
matrix rownames P = "AMPb-Mrp" "AMPb-Prp" "AMPb-Mmm" "AMPb-Pmm" ///
                    "AMPa-Mrp" "AMPa-Prp" "AMPa-Mmm" "AMPa-Pmm"
matrix colnames P = "Row-C" "Row-D" "Col-C" "Col-D"

* Row player learning
* regression in continuous time
preserve
keep if num_subperiods==0
xtreg p1_diff p1_regret_sign p1_regret_sign_pure p1_regret_sign_mm p1_regret_sign_AMPa ///
      p1_regret_sign_pure_mm p1_regret_sign_pure_AMPa p1_regret_sign_mm_AMPa /// 
      , fe vce(cluster session_code)
boottest p1_regret_sign=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[1,1] = r(p) //AMPb-Mrp
boottest p1_regret_sign+p1_regret_sign_pure=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[2,1] = r(p) //AMPb-Prp
boottest p1_regret_sign+p1_regret_sign_mm=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[3,1] = r(p) //AMPb-Mmm
boottest p1_regret_sign+p1_regret_sign_pure+p1_regret_sign_mm+p1_regret_sign_pure_mm=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[4,1] = r(p) //AMPb-Pmm
boottest p1_regret_sign+p1_regret_sign_AMPa=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[5,1] = r(p) //AMPa-Mrp
boottest p1_regret_sign+p1_regret_sign_AMPa+p1_regret_sign_pure+p1_regret_sign_pure_AMPa=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[6,1] = r(p) //AMPa-Prp
boottest p1_regret_sign+p1_regret_sign_AMPa+p1_regret_sign_mm+p1_regret_sign_mm_AMPa=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[7,1] = r(p) //AMPa-Mmm
boottest p1_regret_sign+p1_regret_sign_pure+p1_regret_sign_mm+p1_regret_sign_AMPa+p1_regret_sign_pure_mm+p1_regret_sign_pure_AMPa+p1_regret_sign_mm_AMPa=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[8,1] = r(p) //AMPa-Pmm
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) replace nolabel bdec(2)
restore

* regression in discrete time
preserve
keep if num_subperiods!=0
xtreg p1_diff p1_regret_sign p1_regret_sign_pure p1_regret_sign_mm p1_regret_sign_AMPa ///
      p1_regret_sign_pure_mm p1_regret_sign_pure_AMPa p1_regret_sign_mm_AMPa ///
      , fe vce(cluster session_code)
boottest p1_regret_sign=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[1,2] = r(p) //AMPb-Mrp
boottest p1_regret_sign+p1_regret_sign_pure=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[2,2] = r(p) //AMPb-Prp
boottest p1_regret_sign+p1_regret_sign_mm=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[3,2] = r(p) //AMPb-Mmm
boottest p1_regret_sign+p1_regret_sign_pure+p1_regret_sign_mm+p1_regret_sign_pure_mm=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[4,2] = r(p) //AMPb-Pmm
boottest p1_regret_sign+p1_regret_sign_AMPa=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[5,2] = r(p) //AMPa-Mrp
boottest p1_regret_sign+p1_regret_sign_AMPa+p1_regret_sign_pure+p1_regret_sign_pure_AMPa=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[6,2] = r(p) //AMPa-Prp
boottest p1_regret_sign+p1_regret_sign_AMPa+p1_regret_sign_mm+p1_regret_sign_mm_AMPa=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[7,2] = r(p) //AMPa-Mmm
boottest p1_regret_sign+p1_regret_sign_pure+p1_regret_sign_mm+p1_regret_sign_AMPa+p1_regret_sign_pure_mm+p1_regret_sign_pure_AMPa+p1_regret_sign_mm_AMPa=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[8,2] = r(p) //AMPa-Pmm
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)
restore

* Column player learning
* regression in continuous time
preserve
keep if num_subperiods==0
xtreg p2_diff p2_regret_sign p2_regret_sign_pure p2_regret_sign_mm p2_regret_sign_AMPa ///
      p2_regret_sign_pure_mm p2_regret_sign_pure_AMPa p2_regret_sign_mm_AMPa /// 
      , fe vce(cluster session_code)
boottest p2_regret_sign=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[1,3] = r(p) //AMPb-Mrp
boottest p2_regret_sign+p2_regret_sign_pure=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[2,3] = r(p) //AMPb-Prp
boottest p2_regret_sign+p2_regret_sign_mm=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[3,3] = r(p) //AMPb-Mmm
boottest p2_regret_sign+p2_regret_sign_pure+p2_regret_sign_mm+p2_regret_sign_pure_mm=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[4,3] = r(p) //AMPb-Pmm
boottest p2_regret_sign+p2_regret_sign_AMPa=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[5,3] = r(p) //AMPa-Mrp
boottest p2_regret_sign+p2_regret_sign_AMPa+p2_regret_sign_pure+p2_regret_sign_pure_AMPa=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[6,3] = r(p) //AMPa-Prp
boottest p2_regret_sign+p2_regret_sign_AMPa+p2_regret_sign_mm+p2_regret_sign_mm_AMPa=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[7,3] = r(p) //AMPa-Mmm
boottest p2_regret_sign+p2_regret_sign_pure+p2_regret_sign_mm+p2_regret_sign_AMPa+p2_regret_sign_pure_mm+p2_regret_sign_pure_AMPa+p2_regret_sign_mm_AMPa=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[8,3] = r(p) //AMPa-Pmm
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)
restore

* regression in discrete time
preserve
keep if num_subperiods!=0
xtreg p2_diff p2_regret_sign p2_regret_sign_pure p2_regret_sign_mm p2_regret_sign_AMPa ///
      p2_regret_sign_pure_mm p2_regret_sign_pure_AMPa p2_regret_sign_mm_AMPa /// 
      , fe vce(cluster session_code)
boottest p2_regret_sign=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[1,4] = r(p) //AMPb-Mrp
boottest p2_regret_sign+p2_regret_sign_pure=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[2,4] = r(p) //AMPb-Prp
boottest p2_regret_sign+p2_regret_sign_mm=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[3,4] = r(p) //AMPb-Mmm
boottest p2_regret_sign+p2_regret_sign_pure+p2_regret_sign_mm+p2_regret_sign_pure_mm=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[4,4] = r(p) //AMPb-Pmm
boottest p2_regret_sign+p2_regret_sign_AMPa=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[5,4] = r(p) //AMPa-Mrp
boottest p2_regret_sign+p2_regret_sign_AMPa+p2_regret_sign_pure+p2_regret_sign_pure_AMPa=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[6,4] = r(p) //AMPa-Prp
boottest p2_regret_sign+p2_regret_sign_AMPa+p2_regret_sign_mm+p2_regret_sign_mm_AMPa=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[7,4] = r(p) //AMPa-Mmm
boottest p2_regret_sign+p2_regret_sign_pure+p2_regret_sign_mm+p2_regret_sign_AMPa+p2_regret_sign_pure_mm+p2_regret_sign_pure_AMPa+p2_regret_sign_mm_AMPa=0, noci nograph weight(webb) cluster(session_code) bootcl(session_round_pair_id)
matrix P[8,4] = r(p) //AMPa-Pmm  
outreg2 using D:\Dropbox\stataresult, tex nonote stats(coef pval) append nolabel bdec(2)
restore

* matrix output
matrix list P
esttab matrix(P) using D:\Dropbox\stataresult, tex
