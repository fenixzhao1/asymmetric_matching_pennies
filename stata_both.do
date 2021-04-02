**********Data preparation**********
* open dataset
use "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/stata/mp_production.dta", clear

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

* drop first period of each priod and early data
drop if tick <=3 & num_subperiods != 0
drop if tick <=36 & num_subperiods == 0
drop if round == 1 & dummy_mm == 1
drop if round == 5 & dummy_mm == 1
drop if round == 9 & dummy_mm == 1
drop if round == 13 & dummy_mm == 1
drop if round == 17 & dummy_mm == 1
drop if round == 1 & dummy_mm == 0
drop if round == 7 & dummy_mm == 0
drop if round == 13 & dummy_mm == 0
drop if round == 19 & dummy_mm == 0
drop if round == 25 & dummy_mm == 0

* save dataset
save "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/stata/mp_production.dta", replace


**********Directional learning**********
* Row player learning
* regression in continuous time
use "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/stata/mp_production.dta", clear
xtset session_round_pair_id tick
xtreg p1_diff p1_regret_sign p1_regret_sign_pure p1_regret_sign_mm p1_regret_sign_8002 p1_regret_sign_IDDS ///
      p1_regret_sign_pure_mm p1_regret_sign_pure_8002 p1_regret_sign_pure_IDDS p1_regret_sign_mm_8002 p1_regret_sign_mm_IDDS /// 
      p1_regret_sign_pure_mm_8002 p1_regret_sign_pure_mm_IDDS if num_subperiods==0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

* regression in discrete time
xtreg p1_diff p1_regret_sign p1_regret_sign_pure p1_regret_sign_mm p1_regret_sign_8002 p1_regret_sign_IDDS ///
      p1_regret_sign_pure_mm p1_regret_sign_pure_8002 p1_regret_sign_pure_IDDS p1_regret_sign_mm_8002 p1_regret_sign_mm_IDDS /// 
      p1_regret_sign_pure_mm_8002 p1_regret_sign_pure_mm_IDDS if num_subperiods!=0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* Column player learning
* regression in continuous time
xtreg p2_diff p2_regret_sign p2_regret_sign_pure p2_regret_sign_mm p2_regret_sign_8002 p2_regret_sign_IDDS ///
      p2_regret_sign_pure_mm p2_regret_sign_pure_8002 p2_regret_sign_pure_IDDS p2_regret_sign_mm_8002 p2_regret_sign_mm_IDDS /// 
      p2_regret_sign_pure_mm_8002 p2_regret_sign_pure_mm_IDDS if num_subperiods==0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* regression in discrete time
xtreg p2_diff p2_regret_sign p2_regret_sign_pure p2_regret_sign_mm p2_regret_sign_8002 p2_regret_sign_IDDS ///
      p2_regret_sign_pure_mm p2_regret_sign_pure_8002 p2_regret_sign_pure_IDDS p2_regret_sign_mm_8002 p2_regret_sign_mm_IDDS /// 
      p2_regret_sign_pure_mm_8002 p2_regret_sign_pure_mm_IDDS if num_subperiods!=0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)


**********BR learning**********
* add independent variables
use "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/stata/mp_production.dta", clear

gen p1_direction_pure = p1_direction * dummy_pure
gen p1_direction_mm = p1_direction * dummy_mm
gen p1_direction_8002 = p1_direction * dummy_8002
gen p1_direction_IDDS = p1_direction * dummy_IDDS
gen p1_direction_pure_mm = p1_direction * dummy_pure * dummy_mm
gen p1_direction_pure_8002 = p1_direction * dummy_pure * dummy_8002
gen p1_direction_pure_IDDS = p1_direction * dummy_pure * dummy_IDDS
gen p1_direction_mm_8002 = p1_direction * dummy_mm * dummy_8002
gen p1_direction_mm_IDDS = p1_direction * dummy_mm * dummy_IDDS
gen p1_direction_pure_mm_8002 = p1_direction * dummy_pure * dummy_mm * dummy_8002
gen p1_direction_pure_mm_IDDS = p1_direction * dummy_pure * dummy_mm * dummy_IDDS

gen p2_direction_pure = p2_direction * dummy_pure
gen p2_direction_mm = p2_direction * dummy_mm
gen p2_direction_8002 = p2_direction * dummy_8002
gen p2_direction_IDDS = p2_direction * dummy_IDDS
gen p2_direction_pure_mm = p2_direction * dummy_pure * dummy_mm
gen p2_direction_pure_8002 = p2_direction * dummy_pure * dummy_8002
gen p2_direction_pure_IDDS = p2_direction * dummy_pure * dummy_IDDS
gen p2_direction_mm_8002 = p2_direction * dummy_mm * dummy_8002
gen p2_direction_mm_IDDS = p2_direction * dummy_mm * dummy_IDDS
gen p2_direction_pure_mm_8002 = p2_direction * dummy_pure * dummy_mm * dummy_8002
gen p2_direction_pure_mm_IDDS = p2_direction * dummy_pure * dummy_mm * dummy_IDDS

* Row player learning
* regression in continuous time
xtset session_round_pair_id tick
xtreg p1_diff p1_direction p1_direction_pure p1_direction_mm p1_direction_8002 p1_direction_IDDS ///
      p1_direction_pure_mm p1_direction_pure_8002 p1_direction_pure_IDDS p1_direction_mm_8002 p1_direction_mm_IDDS /// 
      p1_direction_pure_mm_8002 p1_direction_pure_mm_IDDS if num_subperiods==0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

* regression in discrete time
xtreg p1_diff p1_direction p1_direction_pure p1_direction_mm p1_direction_8002 p1_direction_IDDS ///
      p1_direction_pure_mm p1_direction_pure_8002 p1_direction_pure_IDDS p1_direction_mm_8002 p1_direction_mm_IDDS /// 
      p1_direction_pure_mm_8002 p1_direction_pure_mm_IDDS if num_subperiods!=0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* Column player learning
* regression in continuous time
xtreg p2_diff p2_direction p2_direction_pure p2_direction_mm p2_direction_8002 p2_direction_IDDS ///
      p2_direction_pure_mm p2_direction_pure_8002 p2_direction_pure_IDDS p2_direction_mm_8002 p2_direction_mm_IDDS /// 
      p2_direction_pure_mm_8002 p2_direction_pure_mm_IDDS if num_subperiods==0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* regression in discrete time
xtreg p2_diff p2_direction p2_direction_pure p2_direction_mm p2_direction_8002 p2_direction_IDDS ///
      p2_direction_pure_mm p2_direction_pure_8002 p2_direction_pure_IDDS p2_direction_mm_8002 p2_direction_mm_IDDS /// 
      p2_direction_pure_mm_8002 p2_direction_pure_mm_IDDS if num_subperiods!=0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)


**********Pure directional learning**********
* add independent variables
use "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/stata/mp_production.dta", clear

gen p1_sign_pure = p1_sign * dummy_pure
gen p1_sign_mm = p1_sign * dummy_mm
gen p1_sign_8002 = p1_sign * dummy_8002
gen p1_sign_IDDS = p1_sign * dummy_IDDS
gen p1_sign_pure_mm = p1_sign * dummy_pure * dummy_mm
gen p1_sign_pure_8002 = p1_sign * dummy_pure * dummy_8002
gen p1_sign_pure_IDDS = p1_sign * dummy_pure * dummy_IDDS
gen p1_sign_mm_8002 = p1_sign * dummy_mm * dummy_8002
gen p1_sign_mm_IDDS = p1_sign * dummy_mm * dummy_IDDS
gen p1_sign_pure_mm_8002 = p1_sign * dummy_pure * dummy_mm * dummy_8002
gen p1_sign_pure_mm_IDDS = p1_sign * dummy_pure * dummy_mm * dummy_IDDS

gen p2_sign_pure = p2_sign * dummy_pure
gen p2_sign_mm = p2_sign * dummy_mm
gen p2_sign_8002 = p2_sign * dummy_8002
gen p2_sign_IDDS = p2_sign * dummy_IDDS
gen p2_sign_pure_mm = p2_sign * dummy_pure * dummy_mm
gen p2_sign_pure_8002 = p2_sign * dummy_pure * dummy_8002
gen p2_sign_pure_IDDS = p2_sign * dummy_pure * dummy_IDDS
gen p2_sign_mm_8002 = p2_sign * dummy_mm * dummy_8002
gen p2_sign_mm_IDDS = p2_sign * dummy_mm * dummy_IDDS
gen p2_sign_pure_mm_8002 = p2_sign * dummy_pure * dummy_mm * dummy_8002
gen p2_sign_pure_mm_IDDS = p2_sign * dummy_pure * dummy_mm * dummy_IDDS

* Row player learning
* regression in continuous time
xtset session_round_pair_id tick
xtreg p1_diff p1_sign p1_sign_pure p1_sign_mm p1_sign_8002 p1_sign_IDDS ///
      p1_sign_pure_mm p1_sign_pure_8002 p1_sign_pure_IDDS p1_sign_mm_8002 p1_sign_mm_IDDS /// 
      p1_sign_pure_mm_8002 p1_sign_pure_mm_IDDS if num_subperiods==0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

* regression in discrete time
xtreg p1_diff p1_sign p1_sign_pure p1_sign_mm p1_sign_8002 p1_sign_IDDS ///
      p1_sign_pure_mm p1_sign_pure_8002 p1_sign_pure_IDDS p1_sign_mm_8002 p1_sign_mm_IDDS /// 
      p1_sign_pure_mm_8002 p1_sign_pure_mm_IDDS if num_subperiods!=0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* Column player learning
* regression in continuous time
xtreg p2_diff p2_sign p2_sign_pure p2_sign_mm p2_sign_8002 p2_sign_IDDS ///
      p2_sign_pure_mm p2_sign_pure_8002 p2_sign_pure_IDDS p2_sign_mm_8002 p2_sign_mm_IDDS /// 
      p2_sign_pure_mm_8002 p2_sign_pure_mm_IDDS if num_subperiods==0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* regression in discrete time
xtreg p2_diff p2_sign p2_sign_pure p2_sign_mm p2_sign_8002 p2_sign_IDDS ///
      p2_sign_pure_mm p2_sign_pure_8002 p2_sign_pure_IDDS p2_sign_mm_8002 p2_sign_mm_IDDS /// 
      p2_sign_pure_mm_8002 p2_sign_pure_mm_IDDS if num_subperiods!=0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)


**********Directional learning with lagged regret terms**********
use "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/stata/mp_production.dta", clear
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
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

xtreg p1_diff p1_regret_sign p1_regret_sign_L1 p1_regret_sign_L2 ///
      p1_regret_sign_L3 p1_regret_sign_L4 p1_regret_sign_L5 ///
      if num_subperiods==0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

xtreg p2_diff p2_regret_sign p2_regret_sign_L1 p2_regret_sign_L2 ///
      p2_regret_sign_L3 p2_regret_sign_L4 p2_regret_sign_L5 ///
      if num_subperiods!=0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

xtreg p2_diff p2_regret_sign p2_regret_sign_L1 p2_regret_sign_L2 ///
      p2_regret_sign_L3 p2_regret_sign_L4 p2_regret_sign_L5 ///
      if num_subperiods==0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)


* regression with 2 lagged terms
xtreg p1_diff p1_regret_sign p1_regret_sign_L1 ///
      if num_subperiods!=0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

xtreg p1_diff p1_regret_sign p1_regret_sign_L1 ///
      if num_subperiods==0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

xtreg p2_diff p2_regret_sign p2_regret_sign_L1 ///
      if num_subperiods!=0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

xtreg p2_diff p2_regret_sign p2_regret_sign_L1 ///
      if num_subperiods==0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)


**********Directional learning with small, medium and large size**********
use "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/stata/mp_production.dta", clear
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
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

* regression in discrete time
xtreg p1_diff row_regret_sign row_regret_sign_pure row_regret_sign_mm row_regret_sign_8002 row_regret_sign_IDDS ///
      row_regret_sign_pure_mm row_regret_sign_pure_8002 row_regret_sign_pure_IDDS row_regret_sign_mm_8002 row_regret_sign_mm_IDDS /// 
      row_regret_sign_pure_mm_8002 row_regret_sign_pure_mm_IDDS if num_subperiods!=0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* Column player learning
* regression in continuous time
xtreg p2_diff col_regret_sign col_regret_sign_pure col_regret_sign_mm col_regret_sign_8002 col_regret_sign_IDDS ///
      col_regret_sign_pure_mm col_regret_sign_pure_8002 col_regret_sign_pure_IDDS col_regret_sign_mm_8002 col_regret_sign_mm_IDDS /// 
      col_regret_sign_pure_mm_8002 col_regret_sign_pure_mm_IDDS if num_subperiods==0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* regression in discrete time
xtreg p2_diff col_regret_sign col_regret_sign_pure col_regret_sign_mm col_regret_sign_8002 col_regret_sign_IDDS ///
      col_regret_sign_pure_mm col_regret_sign_pure_8002 col_regret_sign_pure_IDDS col_regret_sign_mm_8002 col_regret_sign_mm_IDDS /// 
      col_regret_sign_pure_mm_8002 col_regret_sign_pure_mm_IDDS if num_subperiods!=0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)


**********data summary table with mean_data (regression)**********
use "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/stata/mp_summary.dta", clear

* generate treatment dummies
gen continuous = 0
replace continuous = 1 if num_subperiods == 0
gen pure = 0
replace pure = 1 if pure_strategy == 1
gen mm = 0
replace mm = 1 if mean_matching == 1
gen g8002 = 0
replace g8002 = 1 if game == 2

gen continuous_pure = continuous * pure
gen continuous_mm = continuous * mm
gen continuous_g8002 = continuous * g8002
gen pure_mm = pure * mm
gen pure_g8002 = pure * g8002
gen mm_g8002 = mm * g8002
gen continuous_pure_mm = continuous * pure * mm  
gen continuous_pure_g8002 = continuous * pure * g8002 
gen continuous_mm_g8002 = continuous * mm * g8002
gen pure_mm_g8002 = pure * mm * g8002
gen continuous_pure_mm_g8002 = continuous * pure * mm * g8002 

* drop IDDS
drop if game == 3

* run regressions
reg Deviation_NE continuous pure mm g8002 continuous_pure continuous_mm continuous_g8002 ///
    pure_mm pure_g8002 mm_g8002, vce(cluster session_id)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

reg Deviation_MM continuous pure mm g8002 continuous_pure continuous_mm continuous_g8002 ///
    pure_mm pure_g8002 mm_g8002, vce(cluster session_id)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

reg Deviation_Mid continuous pure mm g8002 continuous_pure continuous_mm continuous_g8002 ///
    pure_mm pure_g8002 mm_g8002, vce(cluster session_id)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

reg sd_geometric continuous pure mm g8002 continuous_pure continuous_mm continuous_g8002 ///
    pure_mm pure_g8002 mm_g8002, vce(cluster session_id)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)


**********data summary table with mean_data (ttest)**********
use "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/stata/mp_summary.dta", clear

* generate treatment dummies
gen continuous = 0
replace continuous = 1 if num_subperiods == 0
gen pure = 0
replace pure = 1 if pure_strategy == 1
gen mm = 0
replace mm = 1 if mean_matching == 1

* ttest for AMPb game








