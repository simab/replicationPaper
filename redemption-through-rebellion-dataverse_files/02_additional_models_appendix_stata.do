********************************************************************************
********************************************************************************
** Cederman, Rüegger, Schvitz 2020:
** Replication file: Additional models (appendix)
********************************************************************************
********************************************************************************

** Set working directory to own project folder
cd ""

log using log_file_02_additional_models_appendix_stata, replace

** Load data
use "epr_segment_level_analysis.dta", clear 

** Generate squared and cubed "peace years" variables
gen pys = peaceyears
gen pys2 = pys*pys
gen pys3 = pys*pys*pys

** Label variables
do "00_label_vars_stata.do"

** Define control variables 
global controls "ln_ag_area_sqkm ag_incidence_flag_lag status_excl downgraded2 rbal warhist ln_capdist ln_rgdppc_lag ln_pop_lag colonial ln_state_age pys*"

********************************************************************************
********************************************************************************
** Table A1: Irredentist claims as dependent variable
********************************************************************************
********************************************************************************

** Estimate models with different independent variables
eststo clear
foreach var of varlist split tfrac tfrac_incr_post1946 {
eststo: logit allclaim3 `var' $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=. /// 
& year < 2013 , nolog cluster(ag_id)

estadd local seg_ctrls "Yes"
estadd local ag_ctrls "Yes"
estadd local ctr_ctrls "Yes"
estadd local t123 "Yes"
}

** Table code
esttab est* using "followup_claim_as_depvar.tex", replace /// 
keep(split tfrac*) ///
label /// 
nogaps eqlabels(none) obslast ///
b(3) se star(* 0.05 ** 0.01 *** 0.001) ///
refcat(tfrac_incr "\textit{AG-level variables}", nolabel) ///
nomtitles ///
scalars("ag_ctrls AG-level controls" "seg_ctrls Segment-level controls" /// 
"ctr_ctrls Country-level controls" "t123 Peace years controls" "N N" "ll Log Likelihood" "aic AIC") ///
title(Logit models: Secessionist / irredentist claims \label{tab:rob:pop1}) ///
nonotes addnotes("AG-clustered standard errors in parentheses." "$^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")

********************************************************************************
** Tables A2, A3: Analysis of configurations
********************************************************************************

** Estimate models with ag-level political status interactions (all excluded)
eststo clear
foreach var of varlist split tfrac tfrac_incr_post1946 {

eststo: areg onset_do_flag c.`var'##i.ag_all_excl $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=. /// 
& year < 2013 , absorb(year) cluster(ag_id)

estadd local yearfe "Yes"
estadd local all_ctrls "Yes"
}

** Table code
esttab est* using "followup_config_mods1.tex", replace /// 
keep(split tfrac* 1.*) order(ag_allexcl split tfrac* 1.*) ///
label nobaselevels interaction(" $\times$ ")style(tex) /// 
nogaps eqlabels(none) obslast ///
b(3) se star(* 0.05 ** 0.01 *** 0.001) ///
refcat(tfrac_incr "\textit{AG-level variables}", nolabel) ///
nomtitles ///
scalars("yearfe Year fixed effects" "all_ctrls All controls" "N N" "ll Log Likelihood" "aic AIC") ///
title(Logit models: Civil conflict onsets, AG-level power constellations \label{tab:rob:config1}) ///
nonotes addnotes("AG-clustered standard errors in parentheses." "$^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")

** Estimate models with ag-level political status interactions (part of AG included)
eststo clear
foreach var of varlist split tfrac tfrac_incr_post1946 {

eststo: areg onset_do_flag c.`var'##i.ag_incl $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=. /// 
& year < 2013 , absorb(year) cluster(ag_id)

estadd local yearfe "Yes"
estadd local all_ctrls "Yes"
}

** Table code
esttab est* using "followup_config_mods2.tex", replace /// 
keep(split tfrac* 1.*) order(ag_incl split tfrac* 1.*) ///
label nobaselevels interaction(" $\times$ ")style(tex) /// 
nogaps eqlabels(none) obslast ///
b(3) se star(* 0.05 ** 0.01 *** 0.001) ///
refcat(tfrac_incr "\textit{AG-level variables}", nolabel) ///
nomtitles ///
scalars("yearfe Year fixed effects" "all_ctrls All controls" "N N" "ll Log Likelihood" "aic AIC") ///
title(Logit models: Civil conflict onsets, AG-level power constellations \label{tab:rob:config2}) ///
nonotes addnotes("AG-clustered standard errors in parentheses." "$^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")

********************************************************************************
** Table A4: Non-linear effects
********************************************************************************

** Code squared and cubed term
gen tfrac2 = tfrac^2
gen tfrac3 = tfrac^3

lab var tfrac2 "Fractionalization$^2$ "
lab var tfrac3 "Fractionalization$^3$ "

** Estimate three models that include non-linear terms
eststo clear
logit onset_do_flag tfrac $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=. ///
& year < 2013 , nolog 
eststo md1
estadd local all_ctrls "Yes"

logit onset_do_flag tfrac tfrac2 $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=. ///
& year < 2013 , nolog
eststo md2
estadd local all_ctrls "Yes"
estadd lrtest md1, force

logit onset_do_flag tfrac tfrac2 tfrac3 $controls ///
if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & seg_area_sqkm!=. /// 
& year < 2013 , nolog
eststo md3
estadd local all_ctrls "Yes"
estadd lrtest md1, force

** Table code
esttab md* using "followup_nonlinear_mods.tex", replace /// 
keep(tfrac*) ///
label /// 
nogaps eqlabels(none) obslast ///
b(3) se star(* 0.05 ** 0.01 *** 0.001) ///
refcat(tfrac_incr "\textit{AG-level variables}", nolabel) ///
nomtitles ///
scalars("all_ctrls All controls" "N N" "ll Log Likelihood" "aic AIC" "lrtest_chi2" "lrtest_df" "lrtest_p") ///
title(Logit models: Civil conflict onsets, non-linear effects of fractionalization \label{tab:rob:nl}) ///
nonotes addnotes("AG-clustered standard errors in parentheses." "$^{*}p<0.05$; $^{**}p<0.01.$; $^{***}p<0.001.$ ")

log close
