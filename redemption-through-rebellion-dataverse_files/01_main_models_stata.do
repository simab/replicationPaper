********************************************************************************
********************************************************************************
** Cederman, Rüegger, Schvitz 2020:
** Replication file: Main models 
********************************************************************************
********************************************************************************

** Set working directory to own project folder
cd ""

log using log_file_01_main_models_stata, replace

** Load data
use "epr_segment_level_analysis.dta", clear 

** Generate squared and cubed "peace years" variables
gen pys = peaceyears
gen pys2 = pys*pys
gen pys3 = pys*pys*pys

** Label variables
do "00_label_vars_stata.do"

********************************************************************************
********************************************************************************
** Table 1
********************************************************************************
********************************************************************************

** Define control variables 
global controls "ln_ag_area_sqkm ag_incidence_flag_lag status_excl downgraded2 rbal warhist ln_capdist ln_rgdppc_lag ln_pop_lag colonial ln_state_age pys*"

** Testing H1, H2, H3 (loop that runs across 3 different independent vars)
eststo clear
foreach var of varlist split tfrac tfrac_incr_post1946 {

eststo: logit onset_do_flag `var' $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=. , ///
nolog cluster(ag_id)

estadd local t123 "Yes"
}

** Table code
esttab est* using "seg_logit_main1.tex", replace /// 
order(split tfrac*) ///
label drop(pys*) /// 
nogaps eqlabels(none) obslast ///
b(3) se star(* 0.05 ** 0.01 *** 0.001) ///
refcat(split "\textit{AG-level variables}" status_excl "\textit{Segment-level variables}" ///
ln_rgdppc_lag "\textit{Country-level variables}", nolabel) ///
mtitles("Model 1.1" "Model 1.2" "Model 1.3") nonumbers ///
scalars("t123 Peace years controls" "N N" "ll Log Likelihood" "aic AIC") ///
title(Logit models: Civil conflict onset \label{tab:SEGlogit1}) ///
nonotes addnotes("AG-clustered standard errors in parentheses." "$^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")

********************************************************************************
********************************************************************************
** Figure 7: Predicted probabilities from Model 3
********************************************************************************
********************************************************************************

eststo clear
logit onset_do_flag tfrac_incr_post1946 $controls ///
	if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=. , nolog cluster(ag_id)

sum tfrac_incr_post1946, d
margins, at(tfrac_incr_post1946=(0(0.01).53678)) atmeans vsquish post level(95)	

#delimit ;
marginsplot, 
	recast(line)
	plot1opts(lcolor(black))
	recastci(rarea) 	
	ciopt(color(gs12)) 
	title("")
	graphregion(style(none) color(white)) scheme(s2mono)
	yline(0, lcolor(gs5)) 
	ytick(0.0 (0.01) 0.03) 
	ylabel(0 (0.01) 0.03) 
	xtick(0 (0.1) 0.5)
	xlabel(0 (0.1) 0.5) 
	xtitle("Fractionalization increase since 1946") 
	ytitle("Predicted probability (conflict onset)")
		;
#delimit cr

********************************************************************************
********************************************************************************
** Table 2
********************************************************************************
********************************************************************************

eststo clear
foreach var of varlist tfrac_incr tfrac_incr_pre1946 {

eststo: logit onset_do_flag `var' $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=. , ///
nolog cluster(ag_id)
estadd local t123 "Yes"
estadd local agctrls "Yes"
estadd local segctrls "Yes"
estadd local ctrctrls "Yes"
}

eststo: logit onset_do_flag tfrac_incr_pre1946 tfrac_incr_post1946 $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=. , ///
 nolog cluster(ag_id)
estadd local t123 "Yes"
estadd local agctrls "Yes"
estadd local segctrls "Yes"
estadd local ctrctrls "Yes"

esttab est*

** Table code
esttab est1 est3 est2 using "seg_logit_main2.tex", replace /// 
keep(tfrac*) ///
label /// 
nogaps eqlabels(none) obslast ///
b(3) se star(* 0.05 ** 0.01 *** 0.001) ///
order(tfrac_incr tfrac_incr_post* tfrac_incr_pre*) ///
refcat(tfrac_incr "\textit{AG-level variables}", nolabel) ///
mtitles("Model 2.1" "Model 2.2" "Model 2.3") nonumbers ///
scalars("agctrls AG-level controls" "segctrls Segment-level controls" "ctrctrls Country-level controls" "t123 Peace years controls" "N N" "ll Log Likelihood" "aic AIC") ///
title(Logit models: Civil conflict onset \label{tab:SEGlogit2}) ///
nonotes addnotes("AG-clustered standard errors in parentheses." "$^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")

********************************************************************************
********************************************************************************
** Table 3
********************************************************************************
********************************************************************************

** Testing M1, M2 with AG FE

eststo clear
foreach var of varlist tfrac tfrac_incr_post1946 {

/// AG FE
eststo: areg onset_do_flag `var' $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=. , ///
absorb(ag_id) cluster(ag_id)

estadd local agfe "Yes"
estadd local t123 "Yes"
estadd local agctrls "Yes"
estadd local segctrls "Yes"
estadd local ctrctrls "Yes"
}

esttab est*

esttab est* using "seg_lpm_main3.tex", replace /// 
keep(tfrac*) ///
label /// 
nogaps eqlabels(none) obslast ///
b(3) se star(* 0.05 ** 0.01 *** 0.001) ///
refcat(tfrac "\textit{AG-level variables}", nolabel) ///
mtitles("Model 3.1" "Model 3.2") nonumbers ///
scalars("agfe AG fixed effects" "agctrls AG-level controls" "segctrls Segment-level controls" "ctrctrls Country-level controls" "t123 Peace years controls" "N N" "ar2 Adjusted $ R^{2} $ ") ///
title(Linear probability models: Civil conflict onset \label{tab:SEGlpmFE}) ///
nonotes addnotes("AG-clustered standard errors in parentheses." "$^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")

********************************************************************************
********************************************************************************
** Table 4
********************************************************************************
********************************************************************************

** Load data
use "eur_map_1918_analysis.dta", clear

** Label variables
do "00_label_vars_stata.do"

eststo clear
foreach var of varlist tfrac tfrac_incr tfrac_hist_incr {

eststo: logit onset_do_flag `var' ///
 ln_ag_area_sqkm ag_incidence_flag_lag /// 
    status_excl downgraded2 rbal warhist ln_rgdppc_lag /// 
    ln_pop_lag ln_capdist colonial_past ln_state_age ///
    if isrelevant == 1 & status_monop == 0 & status_dominant == 0 ///
	& ln_ag_area_sqkm!=. , nolog cluster(ag_id) ///


estadd local t123 "Yes"
estadd local agctrls "Yes"
estadd local segctrls "Yes"
estadd local ctrctrls "Yes"
}

esttab est*

** Table code
esttab est* using "histmaps_logit_main4.tex", replace /// 
label keep(tfrac*) /// 
nogaps eqlabels(none) obslast ///
b(3) se star(* 0.05 ** 0.01 *** 0.001) ///
refcat(split "\textit{AG-level variables}" status_excl "\textit{Segment-level variables}" ///
ln_rgdppc_lag "\textit{Country-level variables}", nolabel) ///
mtitles("Model 4.1" "Model 4.2" "Model 4.3") nonumbers ///
scalars("agctrls AG-level controls" "N N" "ll Log Likelihood" "aic AIC") ///
title(Logit models: Civil conflict onset, using historical map of Europe (1918) \label{tab:euromap}) ///
nonotes addnotes("AG-clustered standard errors in parentheses." "$^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")

********************************************************************************
********************************************************************************
** Table 5
********************************************************************************
********************************************************************************

** Load data
use "murdock_analysis.dta", clear

** Label variables
do "00_label_vars_stata.do"

eststo clear
foreach var of varlist split tfrac tfrac_incr {
eststo: logit statebased_os `var' pop1880_log area_sqkm_log elev_sd ///
    ln_capdist french british, nolog cluster(gwcode)
estadd local agctrls "Yes"
}

** Table code
esttab est* using "murdock_logit_main5.tex", replace /// 
label keep(split tfrac*) /// 
nogaps eqlabels(none) obslast ///
b(3) se star(* 0.05 ** 0.01 *** 0.001) ///
refcat(split "\textit{AG-level variables}" status_excl "\textit{Segment-level variables}" ///
ln_rgdppc_lag "\textit{Country-level variables}", nolabel) ///
mtitles("Model 5.1" "Model 5.2" "Model 5.3") nonumbers ///
scalars("agctrls AG-level controls" "N N" "ll Log Likelihood" "aic AIC") ///
title(Logit models: Civil conflict onset, using Murdock's Tribal Map of Africa (1989-2018) \label{tab:Murdock}) ///
nonotes addnotes("Country-clustered standard errors in parentheses." "$^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")

log close
