library(foreign)
library(mediation)
library(gsfuns)
library("sandwich")
library("lmtest")
library("stargazer")
library("lfe")
library("plm")

# Set working directory to own project folder
setwd("./redemption-through-rebellion-dataverse_files/")


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

an.df.sub.allyears <- an.df %>%
  filter(isrelevant==1,
         status_monopoly==0,
         status_dominant==0,
         !is.na(seg_area_sqkm),
         !is.na(onset_do_flag))

################################################################################
## Table 1: Conflict Onset Logit
################################################################################

vars_logit <- c("onset_do_flag", "ln_ag_area_sqkm", "ag_incidence_flag_lag",
                "status_excl", "downgraded2",
                "rbal", "warhist", "ln_capdist", "ln_rgdppc_lag",
                "ln_pop_lag",  "colonial_past", "ln_state_age",
                "pys", "pys2", "pys3")

treat.vars <- c("split", "tfrac", "tfrac_incr_post1946")

# apologies for the for loop
for(treat_var in treat.vars){
  full_lhs <- append(treat_var, vars_logit)
  onset_logit <- glm(onset_do_flag ~ ., 
                     data=an.df.sub.allyears[,full_lhs], family="binomial")
  assign(paste0("logit_",treat_var), onset_logit)
}


# The below portion of the replication needs a bit more attention
# to ensure the R regression and clustered errors are performing the same as the Stata logit 
# that was run by the authors. Nevertheless, the values are close to those in the 
# authors Table 1 (aside from Exclusion and Relative Size, which are very off). 

# include clustered standard errors as in stata model
# as suggested in this stackoverflow: https://stackoverflow.com/questions/16498849/logistic-regression-with-robust-clustered-standard-errors-in-r
# and https://www.r-bloggers.com/2021/05/clustered-standard-errors-with-r/

logit_split_coefs <- coeftest(logit_split, 
                              vcov. = vcovCL(logit_split, 
                                             cluster = an.df.sub.allyears$ag_id))
logit_tfrac_coefs <- coeftest(logit_tfrac, 
                              vcov. = vcovCL(logit_tfrac, 
                                             cluster = an.df.sub.allyears$ag_id))
logit_tfrac_1946_coefs <- coeftest(logit_tfrac_incr_post1946, 
                                   vcov. = vcovCL(logit_tfrac_incr_post1946, 
                                                  cluster = an.df.sub.allyears$ag_id))

stargazer(logit_split_coefs, logit_tfrac_coefs, logit_tfrac_1946_coefs,
          type="text",
          title = "Replication of Table 1",
          dep.var.caption = "Civil Conflict Onset",
          covariate.labels = c("Divided group", "Fractionalization", 
                               "Frac. incr. since 1946", "Territory sq.km, log",
                               "Ongoing conflict, lag", "Exclusion",
                               "Downgraded", "Relative size", "War history",
                               "Distance to capital, log", "GDP, lag, log",
                               "Population, log","Colonial history",
                               "State age, log","Peace years",
                               "Peace years sq","Peace years cubed"))
                               
                               
                               
################################################################################
## Figure 7: Predicted probabilities from Model 3
################################################################################         
                               
                               
                               

################################################################################
## Table 2: Conflict Onset Logit
################################################################################

treat.vars.2 <- c("tfrac_incr", "tfrac_incr_pre1946")

# Create models 2.1 and 2.3
for(treat_var in treat.vars.2){
  full_lhs <- append(treat_var, vars_logit)
  onset_logit <- glm(onset_do_flag ~ ., 
                     data=an.df.sub.allyears[,full_lhs], family="binomial")
  assign(paste0("logit_2_",treat_var), onset_logit)
}

# Create model 2.2
combined_lhs <- append(c("tfrac_incr_pre1946","tfrac_incr_post1946"), vars_logit)
logit_2_prepost1946 <- glm(onset_do_flag ~ ., 
                      data = an.df.sub.allyears[,combined_lhs], family="binomial")                               
        
logit_2_tfrac_coefs <- coeftest(logit_2_tfrac_incr, 
                              vcov. = vcovCL(logit_2_tfrac_incr, 
                                             cluster = an.df.sub.allyears$ag_id))
logit_2_tfrac_pre1946_coefs <- coeftest(logit_2_tfrac_incr_pre1946, 
                                   vcov. = vcovCL(logit_2_tfrac_incr_pre1946, 
                                                  cluster = an.df.sub.allyears$ag_id))
logit_2_prepost1946_coefs <- coeftest(logit_2_prepost1946, 
                              vcov. = vcovCL(logit_2_prepost1946, 
                                             cluster = an.df.sub.allyears$ag_id))



stargazer(logit_2_tfrac_coefs, logit_2_prepost1946_coefs, logit_2_tfrac_pre1946_coefs,
          type="text",
          title = "Replication of Table 2",
          dep.var.caption = "Civil Conflict Onset",
          covariate.labels = c("Fractionalization incr. since 1886", 
                               "Frac. incr. before 1946", 
                               "Frac. incr. since 1946",
                               "Territory sq.km, log",
                               "Ongoing conflict, lag", "Exclusion",
                               "Downgraded", "Relative size", "War history",
                               "Distance to capital, log", "GDP, lag, log",
                               "Population, log","Colonial history",
                               "State age, log","Peace years",
                               "Peace years sq","Peace years cubed"))

                       

################################################################################
## Table 3: Conflict Onset Linear Probability Models
################################################################################

treat.vars.3 <- c("tfrac", "tfrac_incr_post1946")

# linear regression with Fixed Effect of AG (?)
# felm much more finnicky with use of variables in formula, 
# or pre-created formulas, so manually writing them out :(

for(treat_var in treat.vars.3){
  full_lhs <- append(treat_var, vars_logit)
  full_lhs <- append(full_lhs,"ag_id")
  
  # use to create formula again if need to, copy and paste
  #felm_formula <- as.formula(paste("onset_do_flag ~ ", 
  #                           paste(full_lhs[!full_lhs %in% c("onset_do_flag","ag_id")], 
  #                           collapse="+")))
  
  d <- an.df.sub.allyears[,full_lhs]
  if(treat_var == "tfrac"){
    onset_tfrac_fe_reg <- felm(onset_do_flag ~ tfrac + ln_ag_area_sqkm + ag_incidence_flag_lag + 
                                 status_excl + downgraded2 + rbal + warhist + ln_capdist + 
                                 ln_rgdppc_lag + ln_pop_lag + colonial_past + ln_state_age + 
                                 pys + pys2 + pys3 | ag_id | 0 | ag_id, data = d)
  }
  else{
    onset_tfrac_post1946_fe_reg <- felm(onset_do_flag ~ tfrac_incr_post1946 + ln_ag_area_sqkm + ag_incidence_flag_lag + 
                                          status_excl + downgraded2 + rbal + warhist + ln_capdist + 
                                          ln_rgdppc_lag + ln_pop_lag + colonial_past + ln_state_age + 
                                          pys + pys2 + pys3 | ag_id | 0 | ag_id, data = d)
  }
}

for(treat_var in treat.vars.3){
  full_lhs <- append(treat_var, vars_logit)
  full_lhs <- append(full_lhs,"ag_id")
  plm_formula <- as.formula(paste("onset_do_flag ~ ", 
                             paste(full_lhs[!full_lhs %in% c("onset_do_flag","ag_id")], 
                             collapse="+")))
  d <- an.df.sub.allyears[,full_lhs]
  plm(plm_formula, index = "ag_id", model = "within", data = d)
  assign(paste0("onset_",treat_var,"_fe_reg"), onset_logit)
}


fe_reg_tfrac_coefs <- coeftest(onset_tfrac_fe_reg, 
                                vcov. = vcovCL(onset_tfrac_fe_reg, 
                                               cluster = an.df.sub.allyears$ag_id))
fe_reg_tfrac_post1946_coefs <- coeftest(onset_tfrac_post1946_fe_reg, 
                                        vcov. = vcovCL(onset_tfrac_post1946_fe_reg, 
                                                       cluster = an.df.sub.allyears$ag_id))

stargazer(onset_tfrac_fe_reg, onset_tfrac_post1946_fe_reg,
          type="text",
          title = "Replication of Table 3",
          dep.var.caption = "Linear Probability Models: Civil Conflict Onset",
          covariate.labels = c("Fractionalization", 
                               "Frac. incr. since 1946",
                               "Territory sq.km, log",
                               "Ongoing conflict, lag", "Exclusion",
                               "Downgraded", "Relative size", "War history",
                               "Distance to capital, log", "GDP, lag, log",
                               "Population, log","Colonial history",
                               "State age, log","Peace years",
                               "Peace years sq","Peace years cubed"))




                
### Resources Used

https://statisticsglobe.com/name-variables-in-for-loop-dynamically-in-r

https://unc-libraries-data.github.io/R-Open-Labs/Extras/Stargazer/Stargazer.html

https://stackoverflow.com/questions/5251507/how-to-succinctly-write-a-formula-with-many-variables-from-a-data-frame



