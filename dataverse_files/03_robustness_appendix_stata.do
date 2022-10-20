********************************************************************************
********************************************************************************
** Cederman, Rüegger, Schvitz 2020:
** Replication file: Robustness 
********************************************************************************
********************************************************************************

** Set working directory to own project folder
cd "/Users/schvitzg/polybox/Redemption Rebellion/2020/rr_paper/stata/harvard_dataverse_ajps_files"

log using log_file_03_robustness_appendix_stata, replace

** Load data
use "epr_segment_level_analysis.dta", clear 

** Generate squared and cubed "peace years" variables
gen pys = peaceyears
gen pys2 = pys*pys
gen pys3 = pys*pys*pys

** Label variables
do "00_label_vars_stata.do"

** Define control variables (without conflict or epr status variables)
global controls "ln_ag_area_sqkm rbal warhist ln_capdist ln_rgdppc_lag ln_pop_lag colonial ln_state_age pys*"

********************************************************************************
** Table A5: Logit models without conflict or epr status variables
********************************************************************************

** Estimate models with different independent variables
eststo clear
foreach var of varlist split tfrac tfrac_incr_post1946 tfrac_incr_pre1946 tfrac_incr {

eststo: logit onset_do_flag `var' $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=., /// 
nolog cluster(ag_id)

estadd local all_ctrls "Yes"
}

eststo: logit onset_do_flag tfrac_incr_post1946 $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=., nolog cluster(ag_id)
estadd local all_ctrls "Yes"

** Table code
esttab est1 est2 est3 est6 est4 est5 using "robust_posttreatment1.tex", replace ///
keep(split tfrac*) ///
label /// 
nogaps eqlabels(none) obslast ///
b(3) se star(* 0.05 ** 0.01 *** 0.001) ///
///refcat(tfrac_incr "\textit{AG-level variables}", nolabel) ///
nomtitles ///
scalars("all_ctrls All controls" "N N" "ll Log Likelihood" "aic AIC") ///
title(Logit models: Civil conflict onset, dropping lagged conflict and EPR status \label{tab:robptreat}) ///
nonotes addnotes("AG-clustered standard errors in parentheses." /// 
"$^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")

********************************************************************************
** Table A6: Logit models with population-based fractionalization variables
********************************************************************************

** Estimate models with different independent variables
eststo clear
foreach var of varlist pfrac pfrac_incr_post1946 pfrac_incr{

eststo: logit onset_do_flag `var' $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=., /// 
nolog cluster(ag_id)

estadd local all_ctrls "Yes"
}

** Table code
esttab est* using "robust_pfrac1.tex", replace /// 
keep(pfrac*) ///
label /// 
nogaps eqlabels(none) obslast ///
b(3) se star(* 0.05 ** 0.01 *** 0.001) ///
refcat(tfrac_incr "\textit{AG-level variables}", nolabel) ///
nomtitles ///
scalars("all_ctrls All controls" "N N" "ll Log Likelihood" "aic AIC") ///
title(Logit models: Civil conflict onset, population-based fractionalization \label{tab:rob:pop1}) ///
nonotes addnotes("AG-clustered standard errors in parentheses." /// 
"$^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")


********************************************************************************
** Table A7: LPM models with year, AG and region fixed effects
********************************************************************************

** Code "world region" variable
gen wreg = . 
replace wreg = 1 if countries_gwid < 200
replace wreg = 2 if countries_gwid >= 200 & countries_gwid < 400
replace wreg = 3 if countries_gwid >= 400 & countries_gwid < 627 | /// 
countries_gwid == 651
replace wreg = 4 if countries_gwid > 626 & countries_gwid  < 651 | /// 
countries_gwid > 651 & countries_gwid < 710
replace wreg = 5 if countries_gwid >= 710

** Estimate models with different independent variables and fixed effects terms
eststo clear
foreach var of varlist tfrac tfrac_incr_post1946 tfrac_incr {
/// Year FE
eststo: areg onset_do_flag `var' $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=., ///
absorb(year) cluster(ag_id)
estadd local ctrls "Yes"
estadd local yearfe "Yes"
estadd local regfe "No"
estadd local agfe "No"

// Year + Region FE 
eststo: areg onset_do_flag `var' $controls i.wreg ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=., ///
absorb(year) cluster(ag_id)
estadd local ctrls "Yes"
estadd local yearfe "Yes"
estadd local regfe "Yes"
estadd local agfe "No"

// Year FE + Region + AG FE
eststo: areg onset_do_flag `var' $controls i.year i.wreg ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=., ///
absorb(ag_id) cluster(ag_id)
estadd local ctrls "Yes"
estadd local yearfe "Yes"
estadd local regfe "Yes"
estadd local agfe "Yes"
}

** Table code
esttab est* using "robust_fe1.tex", replace ///
order(tfrac*) ///
label keep(tfrac*) /// 
nogaps eqlabels(none) obslast ///
b(3) se star(+ 0.10 * 0.05 ** 0.01 *** 0.001) /// ///
refcat(split "\textit{AG-level variables}", nolabel) ///
nomtitles ///
scalars("ctrls All controls" "yearfe Year fixed effects" /// 
"regfe Region fixed effects" "agfe AG FE" "ar2 Adjusted $ R^{2} $ ") ///
title(Linear probability models: Year, AG and region fixed effects. \label{tab:rob:fe1})  ///
nonotes addnotes("AG-clustered standard errors in parentheses." /// 
"$^{+}p<0.1$; $^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")

********************************************************************************
** Table A8: LPM models with year, AG and country fixed effects
********************************************************************************

** Estimate models with different independent variables and fixed effects terms
eststo clear 
foreach var of varlist tfrac tfrac_incr_post1946 tfrac_incr {
/// Year FE
eststo: areg onset_do_flag `var' $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=., /// 
absorb(year) cluster(ag_id)
estadd local ctrls "Yes"
estadd local yearfe "Yes"
estadd local agfe "No"
estadd local ctrfe "No"

// Year + Country FE
eststo: areg onset_do_flag `var' $controls i.year ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=., /// 
absorb(countries_gwid) cluster(ag_id)
estadd local ctrls "Yes"
estadd local yearfe "Yes"
estadd local agfe "No"
estadd local ctrfe "Yes"

// Year FE + AG FE + Country FE
eststo: areg onset_do_flag `var' $controls i.year i.countries_gwid ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=., /// 
absorb(ag_id) cluster(ag_id)
estadd local ctrls "Yes"
estadd local yearfe "Yes"
estadd local agfe "Yes"
estadd local ctrfe "Yes"

}

** Table code
esttab est* using "robust_fe2.tex", replace ///
order(tfrac*) ///
label keep(tfrac*) /// 
nogaps eqlabels(none) obslast ///
b(3) se star(+ 0.10 * 0.05 ** 0.01 *** 0.001) /// ///
refcat(split "\textit{AG-level variables}", nolabel) ///
nomtitles ///
scalars("ctrls All controls" "yearfe Year fixed effects" "ctrfe Country FE" "agfe AG FE" "ar2 Adjusted $ R^{2} $ ") ///
title(Linear probability models: Year, AG and region fixed effects. \label{tab:rob:fe2})  ///
nonotes addnotes("AG-clustered standard errors in parentheses." /// 
"$^{+}p<0.1$; $^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")


********************************************************************************
** Table A9: Logit models with double-clustered SE
********************************************************************************

** See R-Script "03_robustness_appendix.R"

********************************************************************************
** Tables A10, A11, A12, A13: LPM with Conley standard errors (SLOW)
********************************************************************************

** Remove obs without coordinates (required for acreg)
drop if  Y == .

** Set time lag for temporal dependence
scalar timelag = 10
local tl = timelag

** Estimate main model with varying distance cutoffs and time lag 10 years
eststo clear
foreach i in 250 500 1000 2500 5000 {
eststo: acreg  onset_do_flag tfrac $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=., ///
pfe1(year) id(ag_id) time(year) ///
latitude(Y) longitude(X) lag(`tl') dist(`i') spatial hac ak0
estadd local ctrls "Yes"
estadd local yearfe "Yes"
estadd local cutoff `i'
}

** Table code
esttab est* using "robust_conley1.tex", replace ///
order(tfrac*) ///
label keep(tfrac*) /// 
nogaps eqlabels(none) obslast ///
b(3) se star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
refcat(split "\textit{AG-level variables}", nolabel) ///
nomtitles ///
scalars("cutoff Distance Cutoff (km)" "ctrls All controls" "yearfe Year FE" "ar2 Adjusted $ R^{2} $ ") ///
title(LPM: Conley standard errors with varying distance cutoffs (I) \label{tab:rob:cnl1}) ///
nonotes addnotes("Conley GMM standard errors in parentheses." /// 
"$^{+}p<0.1$; $^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")

** Estimate main model with varying distance cutoffs and time lag 10 years
eststo clear
foreach i in 250 500 1000 2500 5000 {
eststo: acreg  onset_do_flag tfrac_incr_post1946 $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=., /// 
pfe1(year) id(ag_id) time(year) ///
latitude(Y) longitude(X) lag(`tl') dist(`i') spatial hac 
estadd local ctrls "Yes"
estadd local yearfe "Yes"
estadd local cutoff `i'
}

** Table code
esttab est* using "robust_conley2.tex", replace ///
order(tfrac*) ///
label keep(tfrac*) /// 
nogaps eqlabels(none) obslast ///
b(3) se star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
refcat(split "\textit{AG-level variables}", nolabel) ///
nomtitles ///
scalars("cutoff Distance Cutoff (km)" "ctrls All controls" "yearfe Year FE") ///
title(LPM: Conley standard errors with varying distance cutoffs (II) \label{tab:rob:cnl2}) ///
nonotes addnotes("Conley GMM standard errors in parentheses." /// 
"$^{+}p<0.1$; $^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")

** Estimate main model with varying distance cutoffs and time lag 10 years
eststo clear
foreach i in 250 500 1000 2500 5000 {
eststo: acreg  onset_do_flag tfrac_incr $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=., /// 
pfe1(year) id(ag_id) time(year) ///
latitude(Y) longitude(X) lag(`tl') dist(`i') spatial hac 
estadd local ctrls "Yes"
estadd local yearfe "Yes"
estadd local cutoff `i'
}

** Table code
esttab est* using "robust_conley3.tex", replace ///
order(tfrac*) ///
label keep(tfrac*) /// 
nogaps eqlabels(none) obslast ///
b(3) se star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
refcat(split "\textit{AG-level variables}", nolabel) ///
nomtitles ///
scalars("cutoff Distance Cutoff (km)" "ctrls All controls" "yearfe Year FE") ///
title(LPM: Conley standard errors with varying distance cutoffs (III) \label{tab:rob:cnl3}) ///
nonotes addnotes("Conley GMM standard errors in parentheses." /// 
"$^{+}p<0.1$; $^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")

********************************************************************************
** Table A13: Logit models by continent
********************************************************************************

** See R-Script "robustness.R"

********************************************************************************
** Table A14: Logit models by time period
********************************************************************************

** Estimate models with different independent variables
eststo clear
foreach var of varlist tfrac_incr tfrac_incr_post1918 tfrac_incr_post1946{

eststo: logit onset_do_flag `var' $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=. , /// 
nolog cluster(ag_id)

estadd local all_ctrls "Yes"
}

** Table code
esttab est* using "robust_periods.tex", replace ///
keep(tfrac*) ///
label /// 
nogaps eqlabels(none) obslast ///
b(3) se star(* 0.05 ** 0.01 *** 0.001) ///
nomtitles ///
scalars("all_ctrls All controls" "ll Log Likelihood" "aic AIC") ///
title("Logit Models: Fractionalization increases by time periods" \label{tab:rob:prd}) ///
nonotes addnotes("AG-clustered standard errors in parentheses." /// 
"$^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")

********************************************************************************
** Table A15: LPM models with country-variable interactions: See R-Script "robustness.R"
********************************************************************************

** See R-Script "robustness.R"

********************************************************************************
** Table A16: Logit models on the Aggregate group level
********************************************************************************

** Load data
use "epr_ag_level_analysis.dta", clear 

do "00_label_vars_stata.do"

set varabbrev off

** Generate peaceyears variables
gen pys = ag_peaceyears
gen pys2 = pys^2
gen pys3 = pys^3

** Define controls
global agcontrols "ag_excl_share ln_ag_area_sqkm ag_warhist ag_incidence_flag_lag pys*"

** Estimate models with different independent variables

eststo clear
foreach var of varlist split tfrac tfrac_incr_post1946 tfrac_incr_pre1946 tfrac_incr {

eststo: logit ag_onset_ko_flag `var' $agcontrols ///
if ag_irrelevant_flag == 0 & ag_monop_flag == 0 & ag_dominant_flag == 0 & /// 
ln_ag_area_sqkm != . ,nolog cluster(ag_id)
estadd local ag_controls "Yes"
estadd local t123 "Yes"
}

eststo: logit ag_onset_ko_flag tfrac_incr_post1946 tfrac_incr_pre1946 $agcontrols ///
if ag_irrelevant_flag == 0 & ag_monop_flag == 0 & ag_dominant_flag == 0 & ///
ln_ag_area_sqkm != . , nolog cluster(ag_id)
estadd local ag_controls "Yes"
estadd local t123 "Yes"

** Table code
esttab est1 est2 est3 est6 est4 est5 using "robust_ag1.tex", replace /// 
order(split tfrac*) ///
label keep(split tfrac*) /// 
nogaps eqlabels(none) obslast ///
b(3) se star(* 0.05 ** 0.01 *** 0.001) ///
nomtitles ///
scalars("ag_controls AG controls" "t123 Peaceyears controls" "ll Log Likelihood" "aic AIC") ///
title(Logit models: Civil conflict onset, AG-level analysis. \label{tab:rob:ag1})  ///
nonotes addnotes("AG-clustered standard errors in parentheses." "$^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")
log close

set varabbrev on
