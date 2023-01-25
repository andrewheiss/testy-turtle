# Utility functions -------------------------------------------------------

cumprod_na <- function(x) {
  x[is.na(x)] <- 1
  return(cumprod(x))
}

cumsum_na <- function(x) {
  x[is.na(x)] <- 0
  return(cumsum(x))
}

# Via https://stackoverflow.com/a/55323097/120898
lhs <- function(x) {
  if (attr(terms(as.formula(x)), which = "response")) {
    all.vars(x)[1]
  } else {
    NULL
  }
}


# IPTWs -------------------------------------------------------------------

create_iptws <- function(dat, wt_model) {
  withr::with_seed(bayes_settings$seed$general, {
    # Numerator
    
    # Predictions
    # predict.brmsfit() is an alias of posterior_predict(). The main difference is
    # that it summarizes the predictions automatically and returns a matrix with
    # columns for the average, error, and credible interval bounds
    pred_num <- predict(wt_model$model_num, newdata = dat, re_formula = NULL)
    
    # Residuals
    # residuals.brmsfit() is an alias of predictive_error.brmsfit(). Like
    # predict.brmsfit(), it automatically summarizes the results *and* it allows you
    # to specify what kind of predictions to use. By default it uses epreds, but we
    # want regular full posterior predictions
    resid_num <- residuals(wt_model$model_num, newdata = dat, 
                           re_formula = NULL, method = "posterior_predict")
    
    lhs_num <- lhs(wt_model$model_num$formula$formula)
    
    num_actual <- dnorm(dat[[lhs_num]],
                        pred_num[,1],
                        sd(resid_num[,1], na.rm = TRUE))
    
    # Denominator
    pred_denom <- predict(wt_model$model_denom, newdata = dat, re_formula = NULL)
    resid_denom <- residuals(wt_model$model_denom, newdata = dat, 
                             re_formula = NULL, method = "posterior_predict")
    lhs_denom <- lhs(wt_model$model_denom$formula$formula)
    
    denom_actual <- dnorm(dat[[lhs_denom]],
                          pred_denom[,1],
                          sd(resid_denom[,1], na.rm = TRUE))
    
    dat <- dat %>% 
      mutate(weights_sans_time = num_actual / denom_actual) %>% 
      group_by(gwcode) %>% 
      mutate(iptw = cumprod_na(weights_sans_time)) %>% 
      ungroup()
  })

  return(dat)
}


# Marginal and conditional effects ----------------------------------------

f_mfx_cfx_multiple <- function(model_total, model_advocacy, model_entry, 
                               model_funding, model_ccsi, model_repress) {
  library(marginaleffects)
  
  set.seed(bayes_settings$seed$general)
  
  total <- marginaleffects(
    model_total, 
    newdata = datagrid(year_c = 0,
                       barriers_total = seq(0, 10, 1)),
    variables = "barriers_total",
    type = "response",
    re_formula = NA
  )
  
  advocacy <- marginaleffects(
    model_advocacy, 
    newdata = datagrid(year_c = 0,
                       advocacy = seq(0, 2, 1)),
    variables = "advocacy",
    type = "response",
    re_formula = NA
  )
  
  entry <- marginaleffects(
    model_entry, 
    newdata = datagrid(year_c = 0,
                       entry = seq(0, 3, 1)),
    variables = "entry",
    type = "response",
    re_formula = NA
  )
  
  funding <- marginaleffects(
    model_funding, 
    newdata = datagrid(year_c = 0,
                       funding = seq(0, 4, 1)),
    variables = "funding",
    type = "response",
    re_formula = NA
  )
  
  ccsi <- marginaleffects(
    model_ccsi,
    newdata = datagrid(year_c = 0,
                       v2xcs_ccsi = seq(0, 1, 0.5)),
    variables = "v2xcs_ccsi",
    type = "response",
    re_formula = NA
  )
  
  repress <- marginaleffects(
    model_repress,
    newdata = datagrid(year_c = 0,
                       v2csreprss = c(-2, 0, 2)),
    variables = "v2csreprss",
    type = "response",
    re_formula = NA
  )
  
  return(lst(total, advocacy, entry, funding, ccsi, repress))
}

f_mfx_cfx_single <- function(model_total, model_advocacy, model_entry, 
                             model_funding, model_ccsi, model_repress) {
  library(marginaleffects)
  
  set.seed(bayes_settings$seed$general)
  
  total <- marginaleffects(
    model_total, 
    newdata = datagrid(year_c = 0, barriers_total = 1),
    variables = "barriers_total",
    type = "response",
    re_formula = NA
  )
  
  advocacy <- marginaleffects(
    model_advocacy, 
    newdata = datagrid(year_c = 0, advocacy = 1),
    variables = "advocacy",
    type = "response",
    re_formula = NA
  )
  
  entry <- marginaleffects(
    model_entry, 
    newdata = datagrid(year_c = 0, entry = 1),
    variables = "entry",
    type = "response",
    re_formula = NA
  )
  
  funding <- marginaleffects(
    model_funding, 
    newdata = datagrid(year_c = 0, funding = 1),
    variables = "funding",
    type = "response",
    re_formula = NA
  )
  
  ccsi <- marginaleffects(
    model_ccsi,
    newdata = datagrid(year_c = 0, v2xcs_ccsi = 0.5),
    variables = "v2xcs_ccsi",
    type = "response",
    re_formula = NA
  )
  
  repress <- marginaleffects(
    model_repress,
    newdata = datagrid(year_c = 0, v2csreprss = 0),
    variables = "v2csreprss",
    type = "response",
    re_formula = NA
  )
  
  return(lst(total, advocacy, entry, funding, ccsi, repress))
}
