## Load packages
library(texreg)
library(brglm)
library(gsfuns)
library(foreign)
library(vdem)
library(lfe)


## Set working directory to own project folder
setwd("")

## Load data
epr.seg.df <- read.dta("epr_segment_level_analysis.dta")

## Load helper functions
source("00_analysis_functions.R")

################################################################################
# Table A9: Double-clustered Standard Errors
################################################################################

## Code "peaceyears" vars: years since last conflict, squared and cubed
epr.seg.df$peaceyears_2 <- epr.seg.df$peaceyears^2
epr.seg.df$peaceyears_3 <- epr.seg.df$peaceyears^3


## Define model variables
dv <- "onset_do_flag"
exp.vars <- c("split", "tfrac", "tfrac_incr_post1946", "tfrac_incr_pre1946  + tfrac_incr_post1946", 
              "tfrac_incr_post1946", "tfrac_incr")
controls <- c("ln_ag_area_sqkm", "rbal", "warhist", "ln_capdist", "ln_rgdppc_lag", 
              "ln_pop_lag", "colonial_past", "ln_state_age", "peaceyears",
              "peaceyears_2","peaceyears_3")

## Build formulas
forms.2cl <- lapply(exp.vars, function(x){
  reformulate(c(x, controls), dv)
})

## Run all models 
mods.2cl <- glmBatch(forms.2cl, epr.seg.df, cl.var = "ag_id", cl.var2 = "countries_gwid")

## Prepare texreg table output
## Add log likelihood, AIC, obs (see function texregAdd)
add.rows <- texregAdd(mods.2cl)
add.rows0 <- c(list("All Controls" = rep("Yes", length(mods.2cl))), 
              add.rows)
## Define coefficient names to be shown in table
coefnames <- c("Divided group", "Fractionalization", "Frac. incr. since 1946", 
               "Frac. incr. before 1946", "Frac. incr. since 1886") 
## Define model names
nmods <- length(mods.2cl)
modnams <- paste0("(", 1:nmods,")")

## Notes to add below table
cl.info <- "AG and Country-clustered standard errors in parentheses. "
sign.info <- "$^{*}p<0.05$, $^{**}p<0.001$, $^{***}p<0.001$ }"

## Table code
texreg.cl(mods.2cl, omit.coef = paste(c("Inter", controls), collapse="|"),
          custom.gof.rows = add.rows0, custom.model.names = modnams, caption.above=T, 
          label = "rob::clust",
          caption = "Logit models: Double SE clustering (Country + AG-level)",
          custom.coef.names = coefnames,
          file = "robust_double_cluster.tex")
          
################################################################################
# Table A13: Re-estimate models by continent
################################################################################

## Define explanatory variables 
exp.vars <- c("tfrac", "tfrac_incr_post1946", "tfrac_incr")

## Define variable to cluster standard errors on
cl.var <- "ag_id"

## Generate continent codes
epr.seg.df$continent <- ifelse(epr.seg.df$countries_gwid <= 199, 
                               "Western Hemisphere", NA)
epr.seg.df$continent <- ifelse(epr.seg.df$countries_gwid %in% c(200:399, 630:650, 652:999), 
                               "Eurasia", epr.seg.df$continent)
epr.seg.df$continent <- ifelse(epr.seg.df$countries_gwid %in% c(400:626, 651), 
                               "Africa", epr.seg.df$continent)

## Build formulas
forms.cont <- lapply(exp.vars, function(x){
  reformulate(c(x, controls), dv)
})

## Estimate all models (using brglm)
mods.regions <- c(glmBatch(forms.cont, filter(epr.seg.df, continent == "Africa"), cl.var, br=T),
                  glmBatch(forms.cont, filter(epr.seg.df, continent == "Eurasia"), cl.var, br=T),
                  glmBatch(forms.cont, filter(epr.seg.df, continent == "Western Hemisphere"), cl.var, br=T))

## Prepare texreg table output
add.rows <- texregAdd(mods.regions)
add.rows0 <- c(list("Region" = rep(c("Africa", "Eurasia", "W. Hem."), each = 
                                     length(exp.vars)),
                    "All Controls" = rep("Yes", length(mods.regions))), 
               add.rows)

## Define coefficient names and model titles
coefnames <- c("Fractionalization", "Frac. incr. since 1946", "Frac. incr. since 1886") 
nmods <- length(mods.regions)
modnams <- paste0("(", 1:nmods,")")

## Notes to add below table
cl.info <- "AG and Country-clustered standard errors in parentheses. "
sign.info <- "$^{*}p<0.05$, $^{**}p<0.001$, $^{***}p<0.001$ }"

## Table output
texreg.cl(mods.regions, omit.coef = paste(c("Inter", controls), collapse="|"),
          custom.gof.rows = add.rows0, custom.model.names = modnams, caption.above=T, 
          label = "rob::regions",
          caption = "Logit models by continent",
          custom.coef.names = coefnames,
          file = "robust_continents.tex")

################################################################################
# Table A15: Country-variable interactions
################################################################################

## Add vdem democracy variable (fetch data from "vdem" package)
vd <- extract_vdem(section=2)
vd.vars <- vd %>% dplyr::select(vdem_country_id, GWn, year, v2x_egaldem)
epr.seg.df <- epr.seg.df %>% 
left_join(vd.vars, by = c("year" = "year", "countries_gwid" = "GWn"))

## Define explanatory variables
exp.vars <- c("tfrac", "tfrac_incr_post1946", "tfrac_incr")

## Define interaction terms
int.terms <- c("v2x_egaldem", "ln_rgdppc_lag", "colonial_past")

exp.vars.int <- lapply(1:length(exp.vars), function(x){
  paste(exp.vars[x], int.terms, sep = " * ")
})

## Build formulas
forms.int <- lapply(exp.vars.int, function(x){
  getFelmFormula(dv, c(x, controls), cl = "ag_id", fe = "year")
})

## Estimate all models
mods.interact <- lapply(forms.int, felm, data = epr.seg.df)

## Prepare texreg table output
modnams <- paste0("(", 1:length(mods.interact), ")")
add.rows0 <- list("All controls" = rep("Yes", length(modnams)),
                  "Year fixed effects" = rep("Yes", length(modnams)))
omit.controls <- controls[!grepl("colon|rgdpp", controls)]

## Table code
texreg(mods.interact, omit.coef = paste(c("Inter", omit.controls), collapse="|"),
       custom.gof.rows = add.rows0, custom.model.names = modnams, caption.above=T, 
       label = "rob::nonlinear",
       stars = c(0.1, 0.05, 0.01, 0.001),
       caption = "LPM: Interacting group features with country characteristics",
      # custom.coef.names = coefnames,
       file = "robust_interactions.tex")
