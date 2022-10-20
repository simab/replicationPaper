## Function to run multiple GLM models (includes bias reduced glm option for rare events)
glmBatch <- function(f.list, df, cl.var, cl.var2 = NULL, family = "binomial", br = FALSE){
  ## Function runs through list of regression formulas and returns results with and 
  ## without clustered standard errors. 
  ## 
  ## f.list:  list of regression formulas
  ## df:      analysis dataframe
  ## cl.var:  variable used for standard error clustering
  ## cl.var2: second variable used for standard error clustering (default: NULL)
  ## family:  see R glm
  ## br:      use bias reduced GLM? default: FALSE (see package: "brglm")
  require(brglm)
  lapply(f.list, function(x){
    if(br == TRUE){
      m <- brglm(x, data = df, family = family)
    } else {
      m <- glm(x, data = df, family = family)
    }
    v <- names(m$model)
    v <- v[!grepl("factor", v)]
    m.cl <- coeftestCluster(df, m, v, cl.var, cl.var2)
    return(list(m, m.cl))
  })
}

## Function to build Formulas in 'felm' format (lfe package)
getFelmFormula <- function(lhs, rhs, fe = 0, iv = 0, cl = 0){
  ## lhs: dependent variable (character)
  ## rhs: vector of explanatory vars (character)
  ## fe:  vector of fixed effect terms (default: 0, i.e. ignored)
  ## iv:  vector of instrumental variable terms (default: 0, i.e. ignored)
  ## cl:  vector of cluster terms (default 0, i.e. ignored)
  ##
  ## Formula written as follows LHS ~ RHS | fe.terms | iv.terms | cl.terms
  ## Multiple fe, iv or cl terms separated by + sign.
  base.f <- paste(lhs, paste(rhs, collapse = " + "), sep = " ~ ")
  full.f <- paste(base.f, 
                  paste(fe, collapse = " + "),
                  paste(iv, collapse = " + "),
                  paste(cl, collapse = " + "), sep = "|")
  return(as.formula(full.f))
}

## Function to convert model outputs into texreg-compatible format, 
## needed to output latex tables (see package: "texreg")
  ## mod.list:  list of models
  ## omit:      variables to omit from table
  ## coeftest:  is model output a coeftest object (default = T, see package: "coeftest")
  ## stars:     significance stars shown in output table
texreg.cl <- function(mod.list, omit=NULL, coeftest=TRUE, 
                      strs = c(0.1, 0.05, 0.01, 0.001), ...){
  
  if(coeftest == TRUE){
    mod.list1 <- lapply(mod.list, "[[", 1)
    ll <- round(sapply(mod.list1, logLik),3)
    obs <- sapply(mod.list1, nobs)
    mod.list.cl <- lapply(mod.list, "[[", 2)
  } else {
  }
  
  if(is.null(omit)){
    texreg(mod.list.cl, 
           stars = strs, 
           digits = 3, 
           symbol = "+",
           ...)
  } else {
    texreg(mod.list.cl, 
           stars = strs, 
           omit.coef =  paste(c(omit, "Intercept"), collapse="|"), 
           digits = 3, 
           symbol = "+",
           ...)
  }
}

## Function to add information to texreg table
## Uses a model list created by "glmBatch" and adds Log likelihood, AIC and Obs.
texregAdd <- function(mod.list){
  mod.list <- lapply(mod.list, "[[", 1)
  ll <- round(sapply(mod.list, logLik), 3)
  aic <- round(sapply(mod.list, function(x){x$aic}), 3)
  obs <- sapply(mod.list, nobs)
  return(list("Log Likelihood" = ll, 
              "AIC" = aic,
              "Observations" = obs))
}

## Function to add custom notes to latex table
cl.info <- "AG and Country-clustered standard errors in parentheses. "
sign.info <- "$^{+}p<0.1$, $^{*}p<0.05$, $^{**}p<0.001$, $^{***}p<0.001$ }"

latex.notes.cs <-  function(width = .75){
  paste("\\parbox[t]{",width,"\\textwidth}{}",
        cl.info,
        sign.info,
        sep = " ")}