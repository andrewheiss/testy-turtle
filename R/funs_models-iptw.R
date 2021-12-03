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
  pred_num <- predict(wt_model$model_num, newdata = dat, allow_new_levels = TRUE)
  resid_num <- residuals(wt_model$model_num, newdata = dat, allow_new_levels = TRUE)
  lhs_num <- lhs(wt_model$model_num$formula$formula)
  
  num_actual <- dnorm(dat[[lhs_num]],
                      pred_num[,1],
                      sd(resid_num[,1], na.rm = TRUE))
  
  pred_denom <- predict(wt_model$model_denom, newdata = dat, allow_new_levels = TRUE)
  resid_denom <- residuals(wt_model$model_denom, newdata = dat, allow_new_levels = TRUE)
  lhs_denom <- lhs(wt_model$model_denom$formula$formula)
  
  denom_actual <- dnorm(dat[[lhs_denom]],
                        pred_denom[,1],
                        sd(resid_denom[,1], na.rm = TRUE))
  
  dat <- dat %>% 
    mutate(weights_sans_time = num_actual / denom_actual) %>% 
    group_by(gwcode) %>% 
    mutate(iptw = cumprod_na(weights_sans_time)) %>% 
    ungroup()
  
  return(dat)
}
