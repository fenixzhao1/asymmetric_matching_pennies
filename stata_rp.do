**********Data preparation**********
* open dataset
use "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/stata/mp_production_pairwise.dta", clear

* encode round_pair_id
encode session_round_pair_id, gen (individual_id)

* create interaction term
gen dummy_continuous_pure = dummy_continuous * dummy_pure
gen dummy_continuous_8002 = dummy_continuous * dummy_8002
gen dummy_continuous_IDDS = dummy_continuous * dummy_IDDS
gen dummy_pure_8002 = dummy_pure * dummy_8002
gen dummy_pure_IDDS = dummy_pure * dummy_IDDS
gen dummy_continuous_pure_8002 = dummy_continuous * dummy_pure * dummy_8002
gen dummy_continuous_pure_IDDS = dummy_continuous * dummy_pure * dummy_IDDS

* create interraction term in direction learning
gen p1_regret_sign_pure_8002 = p1_regret_sign_pure * dummy_8002
gen p2_regret_sign_pure_8002 = p2_regret_sign_pure * dummy_8002
gen p1_regret_sign_pure_IDDS = p1_regret_sign_pure * dummy_IDDS
gen p2_regret_sign_pure_IDDS = p2_regret_sign_pure * dummy_IDDS

* set subperiod variable
gen subperiod = 0
replace subperiod = 1 if tick==0 & num_subperiods==15
replace subperiod = 2 if tick==1 & num_subperiods==15
replace subperiod = 3 if tick==2 & num_subperiods==15
replace subperiod = 4 if tick==3 & num_subperiods==15
replace subperiod = 5 if tick==4 & num_subperiods==15
replace subperiod = 6 if tick==5 & num_subperiods==15
replace subperiod = 7 if tick==6 & num_subperiods==15
replace subperiod = 8 if tick==7 & num_subperiods==15
replace subperiod = 9 if tick==8 & num_subperiods==15
replace subperiod = 10 if tick==9 & num_subperiods==15
replace subperiod = 11 if tick==10 & num_subperiods==15
replace subperiod = 12 if tick==11 & num_subperiods==15
replace subperiod = 13 if tick==12 & num_subperiods==15
replace subperiod = 14 if tick==13 & num_subperiods==15
replace subperiod = 15 if tick==14 & num_subperiods==15

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

* set block variable
gen block = 0
replace block = 1 if round<=15
replace block = 2 if round>15

* set second half varibale
gen second_half = 0
replace second_half = 1 if subperiod>=8

* drop first period of each priod and early data
drop if tick <=3 & num_subperiods == 15
drop if tick <=36 & num_subperiods == 0
drop if round==1 | round==7 | round==13 | round==19 | round==25

* save dataset
save "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/stata/mp_production_pairwise.dta", replace


**********Directional learning**********
* Row player learning
* regression in continuous time
use "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/stata/mp_production_pairwise.dta", clear
xtset individual_id tick
xtreg p1_diff p1_regret_sign p1_regret_sign_pure p1_regret_sign_8002 p1_regret_sign_IDDS ///
      p1_regret_sign_pure_8002 p1_regret_sign_pure_IDDS /// 
      if num_subperiods==0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

* regression in discrete time
xtreg p1_diff p1_regret_sign p1_regret_sign_pure p1_regret_sign_8002 p1_regret_sign_IDDS ///
      p1_regret_sign_pure_8002 p1_regret_sign_pure_IDDS ///  
      if num_subperiods==15, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* Column player learning
* regression in continuous time
xtreg p2_diff p2_regret_sign p2_regret_sign_pure p2_regret_sign_8002 p2_regret_sign_IDDS ///
      p2_regret_sign_pure_8002 p2_regret_sign_pure_IDDS /// 
      if num_subperiods==0, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)

* regression in discrete time
xtreg p2_diff p2_regret_sign p2_regret_sign_pure p2_regret_sign_8002 p2_regret_sign_IDDS ///
      p2_regret_sign_pure_8002 p2_regret_sign_pure_IDDS ///       
	  if num_subperiods==15, fe vce(robust)
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se append nolabel bdec(2)


**********Anticipatory learning**********
* anticipation for p1 and p2 version 1
xtreg p1_diff p1_regret_sign p1_regret_sign_showworst p1_regret_sign_showbest p1_regret_sign_8002, fe vce(robust)
predict p1_diff_hat, xb
gen p1_strategy_hat = p1_strategy + p1_diff_hat

xtreg p2_diff p2_regret_sign p2_regret_sign_showworst p2_regret_sign_showbest p2_regret_sign_8002, fe vce(robust)
predict p2_diff_hat, xb
gen p2_strategy_hat = p2_strategy + p2_diff_hat

* anticipation for p1 and p2 version 2
gen p1_strategy_hat = p1_strategy + 0.25*p1_sign
replace p1_strategy_hat = 1 if p1_strategy_hat > 1
replace p1_strategy_hat = 0 if p1_strategy_hat < 0
gen p2_strategy_hat = p2_strategy + 0.25*p2_sign
replace p2_strategy_hat = 1 if p2_strategy_hat > 1
replace p2_strategy_hat = 0 if p2_strategy_hat < 0

* generate modified regret variables for p1
gen p1_uhat_a1_new = payoff1Aa*p2_strategy_hat + payoff1Ab*(1-p2_strategy_hat)
gen p1_uhat_a0_new = payoff1Ba*p2_strategy_hat + payoff1Bb*(1-p2_strategy_hat)
gen p1_ahat_new = p1_strategy
replace p1_ahat_new = 1 if p1_uhat_a1_new > p1_uhat_a0_new
replace p1_ahat_new = 0 if p1_uhat_a1_new < p1_uhat_a0_new
gen p1_uhat_new = payoff1Aa*p1_ahat_new*p2_strategy_hat + payoff1Ab*p1_ahat_new*(1-p2_strategy_hat) + payoff1Ba*(1-p1_ahat_new)*p2_strategy_hat + payoff1Bb*(1-p1_ahat_new)*(1-p2_strategy_hat)
gen p1_regret_new = p1_uhat_new - p1_payoff
replace p1_regret_new = p1_regret_new / 800 if game=="8002"
replace p1_regret_new = p1_regret_new / 700 if game=="3117"
gen p1_direction_new = p1_ahat_new - p1_strategy
gen p1_sign_new = 0
replace p1_sign_new = 1 if p1_direction_new > 0
replace p1_sign_new = -1 if p1_direction_new < 0
gen p1_regret_sign_new = p1_regret_new * p1_sign_new

gen p1_regret_sign_continuous_new = p1_regret_sign_new * dummy_continuous
gen p1_regret_sign_pure_new = p1_regret_sign_new * dummy_pure
gen p1_regret_sign_showworst_new = p1_regret_sign_new * dummy_show_worst
gen p1_regret_sign_showbest_new = p1_regret_sign_new * dummy_show_best
gen p1_regret_sign_8002_new = p1_regret_sign_new * dummy_8002

* generate modified regret variables for p2
gen p2_uhat_a1_new = payoff2Aa*p1_strategy_hat + payoff2Ba*(1-p1_strategy_hat)
gen p2_uhat_a0_new = payoff2Ab*p1_strategy_hat + payoff2Bb*(1-p1_strategy_hat)
gen p2_ahat_new = p2_strategy
replace p2_ahat_new = 1 if p2_uhat_a1_new > p2_uhat_a0_new
replace p2_ahat_new = 0 if p2_uhat_a1_new < p2_uhat_a0_new
gen p2_uhat_new = payoff2Aa*p1_strategy_hat*p2_ahat_new + payoff2Ab*p1_strategy_hat*(1-p2_ahat_new) + payoff2Ba*(1-p1_strategy_hat)*p2_ahat_new + payoff2Bb*(1-p1_strategy_hat)*(1-p2_ahat_new)
gen p2_regret_new = p2_uhat_new - p2_payoff
replace p2_regret_new = p2_regret_new / 200 if game=="8002"
replace p2_regret_new = p2_regret_new / 300 if game=="3117"
gen p2_direction_new = p2_ahat_new - p2_strategy
gen p2_sign_new = 0
replace p2_sign_new = 1 if p2_direction_new > 0
replace p2_sign_new = -1 if p2_direction_new < 0
gen p2_regret_sign_new = p2_regret_new * p2_sign_new

gen p2_regret_sign_continuous_new = p2_regret_sign_new * dummy_continuous
gen p2_regret_sign_pure_new = p2_regret_sign_new * dummy_pure
gen p2_regret_sign_showworst_new = p2_regret_sign_new * dummy_show_worst
gen p2_regret_sign_showbest_new = p2_regret_sign_new * dummy_show_best
gen p2_regret_sign_8002_new = p2_regret_sign_new * dummy_8002

* anticipatory learning regression
xtreg p1_diff p1_regret_sign_new p1_regret_sign_showworst_new p1_regret_sign_showbest_new p1_regret_sign_8002_new, fe vce(robust)
xtreg p2_diff p2_regret_sign_new p2_regret_sign_showworst_new p2_regret_sign_showbest_new p2_regret_sign_8002_new, fe vce(robust)

xtreg p1_diff p1_regret_sign p1_regret_sign_showworst p1_regret_sign_showbest p1_regret_sign_8002 ///
p1_regret_sign_new p1_regret_sign_showworst_new p1_regret_sign_showbest_new p1_regret_sign_8002_new, fe vce(robust)
xtreg p2_diff p2_regret_sign p2_regret_sign_showworst p2_regret_sign_showbest p2_regret_sign_8002 ///
p2_regret_sign_new p2_regret_sign_showworst_new p2_regret_sign_showbest_new p2_regret_sign_8002_new, fe vce(robust)

xtreg p1_diff p1_regret_sign p1_regret_sign_showworst p1_regret_sign_showbest p1_regret_sign_8002 ///
p1_regret_sign_new, fe vce(robust)
xtreg p2_diff p2_regret_sign p2_regret_sign_showworst p2_regret_sign_showbest p2_regret_sign_8002 ///
p2_regret_sign_new, fe vce(robust)

gen p1_diff_regret = p1_regret_sign_new - p1_regret_sign
gen p2_diff_regret = p2_regret_sign_new - p1_regret_sign
xtreg p1_diff p1_regret_sign p1_regret_sign_showworst p1_regret_sign_showbest p1_regret_sign_8002 ///
p1_diff_regret, fe vce(robust)
xtreg p2_diff p2_regret_sign p2_regret_sign_showworst p2_regret_sign_showbest p2_regret_sign_8002 ///
p2_diff_regret, fe vce(robust)


**********Multinomial logistic regressions of cycles over time**********
* open dataset
use "/Users/fenix/Dropbox/GSR/Continuous Bimatrix/Matching pennies/stata/mp_pairwise_cycles.dta", clear

* encode round_pair_id
encode session_round_pair_id, gen (individual_id)

* create type dummy
gen type = 0
replace type = 1 if cyctype=="1.CW"
replace type = 2 if cyctype=="2.diagonal"
replace type = 3 if cyctype=="3.stay" | cyctype=="4.stay"
replace type = 4 if cyctype=="4.CCW" | cyctype=="5.CCW"
replace type = 5 if cyctype=="3.Cdiagonal"
drop if type == 0

* set block variable
gen block = 0
replace block = 1 if round<=15
replace block = 2 if round>15

* set second half varibale
gen second_half = 0
replace second_half = 1 if tick>=7 & num_subperiods==15
replace second_half = 1 if tick>=84 & num_subperiods==0

* create interaction term (8002, mm, pure, continuous)
drop dummy_IDDS
gen dummy_continuous_pure = dummy_continuous * dummy_pure
gen dummy_continuous_8002 = dummy_continuous * dummy_8002
gen dummy_continuous_mm = dummy_continuous * dummy_mm
gen dummy_pure_8002 = dummy_pure * dummy_8002
gen dummy_pure_mm = dummy_pure * dummy_mm
gen dummy_8002_mm = dummy_8002 * dummy_mm

gen dummy_continuous_pure_8002 = dummy_continuous * dummy_pure * dummy_8002
gen dummy_continuous_pure_mm = dummy_continuous * dummy_pure * dummy_mm
gen dummy_continuous_8002_mm = dummy_continuous * dummy_8002 * dummy_mm
gen dummy_pure_8002_mm = dummy_pure * dummy_8002 * dummy_mm
gen dummy_continuous_pure_8002_mm = dummy_continuous * dummy_pure * dummy_8002 + dummy_mm

* run multinomial logistic
xi:mlogit type dummy_continuous dummy_pure dummy_8002 dummy_continuous_pure dummy_continuous_8002 ///
               dummy_pure_8002 dummy_continuous_pure_8002 ///
		       second_half i.block, cluster(individual_id) baseoutcome(3) 
outreg2 using C:\Users\fenix\Dropbox\stataresult, tex nonote se replace nolabel bdec(2)

* run onevsall logistic without stay type
gen type_CW = 0
replace type_CW = 1 if type==1
gen type_D = 0
replace type_D = 1 if type==2
gen type_CCW = 0
replace type_CCW = 1 if type==4
gen type_CD = 0
replace type_CD = 1 if type==5

xi:logit type_CW dummy_continuous dummy_pure dummy_8002 dummy_continuous_pure dummy_continuous_8002 ///
               dummy_pure_8002 dummy_continuous_pure_8002 ///
		       second_half i.block, cluster(individual_id)
outreg2 using C:\Users\fenix\Dropbox\stataresult111, tex nonote se replace nolabel bdec(2)

xi:logit type_D dummy_continuous dummy_pure dummy_8002 dummy_continuous_pure dummy_continuous_8002 ///
               dummy_pure_8002 dummy_continuous_pure_8002 ///
		       second_half i.block, cluster(individual_id)
outreg2 using C:\Users\fenix\Dropbox\stataresult111, tex nonote se append nolabel bdec(2)

xi:logit type_CCW dummy_continuous dummy_pure dummy_8002 dummy_continuous_pure dummy_continuous_8002 ///
               dummy_pure_8002 dummy_continuous_pure_8002 ///
		       second_half i.block, cluster(individual_id)
outreg2 using C:\Users\fenix\Dropbox\stataresult111, tex nonote se append nolabel bdec(2)

xi:logit type_CD dummy_continuous dummy_pure dummy_8002 dummy_continuous_pure dummy_continuous_8002 ///
               dummy_pure_8002 dummy_continuous_pure_8002 ///
		       second_half i.block, cluster(individual_id)
outreg2 using C:\Users\fenix\Dropbox\stataresult111, tex nonote se append nolabel bdec(2)


