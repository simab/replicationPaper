library(foreign)
library(mediation)
library(gsfuns)
library(here)
library(viridis)

## Set working directory to own project folder
setwd(here("dataverse_files"))


## Load dataset
an.df <- read.dta("epr_segment_level_analysis.dta")

## Code "peaceyears" variable: years since last conflict, squared, cubed
an.df$pys <- an.df$peaceyears
an.df$pys2 <- an.df$peaceyears^2
an.df$pys3 <- an.df$peaceyears^3

## Subset dataset: only politically relevant groups (EPR definition), exclude 
## dominant and monopoly groups and groups without settlement area in GeoEPR, 
## subset to pre-2013 years (SDM coverage only up to 2013)
an.df.sub <- an.df %>%
  filter(isrelevant==1,
         status_monopoly==0,
         status_dominant==0,
         !is.na(seg_area_sqkm),
         !is.na(onset_do_flag),
         year <= 2013)

################################################################################
## Helper Functions
################################################################################

## Function to build mediation model formulas (see package: "mediation")
makeForms <- function(dep.var, med.var, treat.var, controls){
  ## dep.var: dependent variable
  ## med.var: mediator variable
  ## treat.var: main explanatory variable
  ## controls: vector of controls
  ## returns a list with two formulas: 1: outcome model. 2: mediator model
  
  RHS <- paste(c(treat.var, med.var, controls), collapse="+")
  out <- as.formula(paste(dep.var, RHS, sep= "~"))
  
  RHS <- paste(c(treat.var, controls), collapse="+")
  med <- as.formula(paste(med.var, RHS, sep= "~"))
  
  return(list("outcome" = out,
              "mediation" = med))
}

## Function for running mediation models one by one
runMediation <- function(out.fun, med.fun, treat.var, med.var, df){
  ## out.fun:   formula with outcome variable as dv
  ## med.fun:   formula with mediator variable as dv
  ## treat.var: name of "treatment" variable
  ## med.var:   name of mediator variable
  ## df:        analysis dataframe
  o <- glm(out.fun, data = df, family = binomial(link="probit"))
  m <- glm(med.fun, data = df, family = binomial(link="probit"))
  mm <- mediate(m, o, sims = 200, treat = treat.var,
                mediator = med.var, robustSE = T)
  return(list(out.model=o,
              med.model = m,
              med.full = mm))
}

## Function to run multiple mediation models at once (alternative mediator vars)
multiMed <- function(dep.var, med.vars, treat.var, ctrls, df){
  ## dep.var:     dependent variable
  ## med.vars:    vector of alternative mediator variables
  ## treat.var:   name of "treatment" variable
  ## ctrls:       vector of control variables
  ## df:          analysis dataframe
  med.list <- lapply(1:length(med.vars), function(x){
    med.var <- med.vars[x]
    f.list <- makeForms(dep.var, med.var, treat.var, ctrls)
    med.out <- runMediation(f.list$outcome, f.list$mediation,
                            treat.var, med.var, df)
    return(med.out$med.full)
  })
  names(med.list) <- med.vars
  return(med.list)
}


################################################################################
## Model setup
################################################################################

## Define "treatment" and dependent variable
treat.v <- "tfrac_incr_post1946"
dep.v <- "onset_do_flag"

## Define control variables
ctrls <- c("ln_ag_area_sqkm", "ag_incidence_flag_lag",
           "rbal", "warhist", "ln_rgdppc_lag",
           "ln_pop_lag", "ln_capdist", "colonial_past", "ln_state_age",
           "pys", "pys2", "pys3")

################################################################################
## Main mediation model (Figure 8)
################################################################################
med.vars <- "allclaim2"
med.mods <- multiMed(dep.v, med.vars, treat.v, ctrls, an.df.sub)

## Plot results
pdf("mediation_allclaims.pdf", 5, 5)
plot(med.mods[[1]], treatment="treated",
     main = "Mediator:\nSecessionist / Irredentist claims", xlim=c(-0.005, 0.16))
dev.off()

################################################################################
## Additional mediation models: Alternative treatment vars (Figure A1)
################################################################################

## Define alternative treatment vars
treat.vars <- c("split", "tfrac",  "tfrac_incr_post1946")

## Build mediation formula
forms <- lapply(1:length(treat.vars), function(x){
  makeForms(dep.v, med.vars[1], treat.vars[x], ctrls)
})

## Output models
med.mods <- lapply(1:length(forms), function(x){
  med.out <- runMediation(forms[[x]]$outcome, forms[[x]]$mediation, 
                          treat.vars[x], med.vars[1], an.df.sub)
  return(med.out)
})

## Plot results
pdf("mediation_robustness.pdf", 8, 3)
par(mfrow = c(1,3))
plot(med.mods[[1]]$med.full, main = "Independent variable: \nDivided Group", 
     treat = "treated", xlim = c(-0.005, 0.01))
plot(med.mods[[2]]$med.full, main = "Independent variable: \nFractionalization",
     treat = "treated", xlim = c(-0.005, 0.05))
plot(med.mods[[3]]$med.full, main = "Independent variable: \nFrac incr. since 1946", 
     treat = "treated", xlim = c(-0.005, 0.15))
dev.off()

################################################################################
## Additional mediation models: Use different mediator variable (Figure A2)
################################################################################
med.vars <- "allclaim3"
med.mods <- multiMed(dep.v, med.vars, treat.v, ctrls, an.df.sub)

## Plot results
pdf("mediation_irrclaims.pdf", 5, 5)
plot(med.mods[[1]], treatment="treated",
     main = "Mediator:\nSecessionist / Irredentist claims", xlim=c(-0.005, 0.16))
dev.off()
