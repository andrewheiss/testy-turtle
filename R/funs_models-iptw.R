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
