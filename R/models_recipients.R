# Treatment models --------------------------------------------------------

f_recip_treatment_total_dom <- function(dat) {
  dat <- dat %>% filter(laws)
  
  # Numerator model
  priors_num <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                  prior(student_t(3, 0, 1.5), class = b),
                  prior(exponential(1), class = sigma),
                  prior(exponential(1), class = sd))
  
  model_num <- brm(
    bf(barriers_total ~ barriers_total_lag1 + 
         barriers_total_lag2_cumsum + (1 | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_num,
    control = list(adapt_delta = 0.95),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(barriers_total ~ barriers_total_lag1 + 
         barriers_total_lag2_cumsum + prop_ngo_dom_lag1 +
         # Human rights and politics
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         # Economics and development
         gdpcap_log_z + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         # Conflict and disasters
         internal_conflict_past_5 + natural_dis_count +
         # Time and country effects
         year_c + (1 + year_c | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_denom,
    control = list(adapt_delta = 0.95),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_recip_treatment_total_foreign <- function(dat) {
  dat <- dat %>% filter(laws)
  
  # Numerator model
  priors_num <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                  prior(student_t(3, 0, 1.5), class = b),
                  prior(exponential(1), class = sigma),
                  prior(exponential(1), class = sd))
  
  model_num <- brm(
    bf(barriers_total ~ barriers_total_lag1 + 
         barriers_total_lag2_cumsum + (1 | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_num,
    control = list(adapt_delta = 0.95),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(barriers_total ~ barriers_total_lag1 + 
         barriers_total_lag2_cumsum + prop_ngo_foreign_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log_z + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         year_c + (1 + year_c | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_denom,
    control = list(adapt_delta = 0.95),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_recip_treatment_advocacy_dom <- function(dat) {
  dat <- dat %>% filter(laws)
  
  # Numerator model
  priors_num <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                  prior(student_t(3, 0, 1.5), class = b),
                  prior(exponential(1), class = sigma),
                  prior(exponential(1), class = sd))
  
  model_num <- brm(
    bf(advocacy ~ advocacy_lag1 + advocacy_lag2_cumsum + (1 | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_num,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(advocacy ~ advocacy_lag1 + advocacy_lag2_cumsum + prop_ngo_dom_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log_z + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         year_c + (1 + year_c | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_denom,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_recip_treatment_advocacy_foreign <- function(dat) {
  dat <- dat %>% filter(laws)
  
  # Numerator model
  priors_num <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                  prior(student_t(3, 0, 1.5), class = b),
                  prior(exponential(1), class = sigma),
                  prior(exponential(1), class = sd))
  
  model_num <- brm(
    bf(advocacy ~ advocacy_lag1 + advocacy_lag2_cumsum + (1 | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_num,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(advocacy ~ advocacy_lag1 + advocacy_lag2_cumsum + prop_ngo_foreign_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log_z + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         year_c + (1 + year_c | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_denom,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_recip_treatment_entry_dom <- function(dat) {
  dat <- dat %>% filter(laws)
  
  # Numerator model
  priors_num <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                  prior(student_t(3, 0, 1.5), class = b),
                  prior(exponential(1), class = sigma),
                  prior(exponential(1), class = sd))
  
  model_num <- brm(
    bf(entry ~ entry_lag1 + entry_lag2_cumsum + (1 | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_num,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(entry ~ entry_lag1 + entry_lag2_cumsum + prop_ngo_dom_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log_z + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         year_c + (1 + year_c | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_denom,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_recip_treatment_entry_foreign <- function(dat) {
  dat <- dat %>% filter(laws)
  
  # Numerator model
  priors_num <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                  prior(student_t(3, 0, 1.5), class = b),
                  prior(exponential(1), class = sigma),
                  prior(exponential(1), class = sd))
  
  model_num <- brm(
    bf(entry ~ entry_lag1 + entry_lag2_cumsum + (1 | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_num,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(entry ~ entry_lag1 + entry_lag2_cumsum + prop_ngo_foreign_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log_z + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         year_c + (1 + year_c | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_denom,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_recip_treatment_funding_dom <- function(dat) {
  dat <- dat %>% filter(laws)
  
  # Numerator model
  priors_num <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                  prior(student_t(3, 0, 1.5), class = b),
                  prior(exponential(1), class = sigma),
                  prior(exponential(1), class = sd))
  
  model_num <- brm(
    bf(funding ~ funding_lag1 + funding_lag2_cumsum + (1 | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_num,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(funding ~ funding_lag1 + funding_lag2_cumsum + prop_ngo_dom_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log_z + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         year_c + (1 + year_c | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_denom,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_recip_treatment_funding_foreign <- function(dat) {
  dat <- dat %>% filter(laws)
  
  # Numerator model
  priors_num <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                  prior(student_t(3, 0, 1.5), class = b),
                  prior(exponential(1), class = sigma),
                  prior(exponential(1), class = sd))
  
  model_num <- brm(
    bf(funding ~ funding_lag1 + funding_lag2_cumsum + (1 | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_num,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(funding ~ funding_lag1 + funding_lag2_cumsum + prop_ngo_foreign_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log_z + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         year_c + (1 + year_c | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_denom,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_recip_treatment_ccsi_dom <- function(dat) {
  # Numerator model
  priors_num <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                  prior(student_t(3, 0, 1.5), class = b),
                  prior(exponential(1), class = sigma),
                  prior(exponential(1), class = sd))
  
  model_num <- brm(
    bf(v2xcs_ccsi ~ v2xcs_ccsi_lag1 + v2xcs_ccsi_lag2_cumsum + (1 | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_num,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(v2xcs_ccsi ~ v2xcs_ccsi_lag1 + v2xcs_ccsi_lag2_cumsum + prop_ngo_dom_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log_z + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         year_c + (1 + year_c | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_denom,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_recip_treatment_ccsi_foreign <- function(dat) {
  # Numerator model
  priors_num <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                  prior(student_t(3, 0, 1.5), class = b),
                  prior(exponential(1), class = sigma),
                  prior(exponential(1), class = sd))
  
  model_num <- brm(
    bf(v2xcs_ccsi ~ v2xcs_ccsi_lag1 + v2xcs_ccsi_lag2_cumsum + (1 | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_num,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(v2xcs_ccsi ~ v2xcs_ccsi_lag1 + v2xcs_ccsi_lag2_cumsum + prop_ngo_foreign_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log_z + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         year_c + (1 + year_c | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_denom,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_recip_treatment_repress_dom <- function(dat) {
  # Numerator model
  priors_num <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                  prior(student_t(3, 0, 1.5), class = b),
                  prior(exponential(1), class = sigma),
                  prior(exponential(1), class = sd))
  
  model_num <- brm(
    bf(v2csreprss ~ v2csreprss_lag1 + v2csreprss_lag2_cumsum + (1 | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_num,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(v2csreprss ~ v2csreprss_lag1 + v2csreprss_lag2_cumsum + prop_ngo_dom_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log_z + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         year_c + (1 + year_c | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_denom,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_recip_treatment_repress_foreign <- function(dat) {
  # Numerator model
  priors_num <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                  prior(student_t(3, 0, 1.5), class = b),
                  prior(exponential(1), class = sigma),
                  prior(exponential(1), class = sd))
  
  model_num <- brm(
    bf(v2csreprss ~ v2csreprss_lag1 + v2csreprss_lag2_cumsum + (1 | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_num,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(v2csreprss ~ v2csreprss_lag1 + v2csreprss_lag2_cumsum + prop_ngo_foreign_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log_z + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         year_c + (1 + year_c | gwcode),
       decomp = "QR"),
    data = dat,
    family = gaussian(),
    prior = priors_denom,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}


# Outcome models ----------------------------------------------------------

f_recip_outcome_total_dom <- function(dat) {
  dat <- dat %>% 
    filter(laws) %>% 
    mutate(prop_ngo_dom_lead1 = ifelse(prop_ngo_dom_lead1 == 1, 0.99, prop_ngo_dom_lead1))
  
  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model <- brm(
    bf(prop_ngo_dom_lead1 | weights(iptw) ~ barriers_total + 
         barriers_total_lag1_cumsum +
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model, priors))
}

f_recip_outcome_total_foreign <- function(dat) {
  dat <- dat %>% 
    filter(laws) %>% 
    mutate(prop_ngo_foreign_lead1 = ifelse(prop_ngo_foreign_lead1 == 1, 0.99, prop_ngo_foreign_lead1))
  
  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model <- brm(
    bf(prop_ngo_foreign_lead1 | weights(iptw) ~ barriers_total + 
         barriers_total_lag1_cumsum +
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model, priors))
}

f_recip_outcome_advocacy_dom <- function(dat) {
  dat <- dat %>% 
    filter(laws) %>% 
    mutate(prop_ngo_dom_lead1 = ifelse(prop_ngo_dom_lead1 == 1, 0.99, prop_ngo_dom_lead1))
  
  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model <- brm(
    bf(prop_ngo_dom_lead1 | weights(iptw) ~ advocacy + advocacy_lag1_cumsum +
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model, priors))
}

f_recip_outcome_advocacy_foreign <- function(dat) {
  dat <- dat %>% 
    filter(laws) %>% 
    mutate(prop_ngo_foreign_lead1 = ifelse(prop_ngo_foreign_lead1 == 1, 0.99, prop_ngo_foreign_lead1))
  
  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model <- brm(
    bf(prop_ngo_foreign_lead1 | weights(iptw) ~ advocacy + advocacy_lag1_cumsum +
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model, priors))
}

f_recip_outcome_entry_dom <- function(dat) {
  dat <- dat %>% 
    filter(laws) %>% 
    mutate(prop_ngo_dom_lead1 = ifelse(prop_ngo_dom_lead1 == 1, 0.99, prop_ngo_dom_lead1))
  
  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model <- brm(
    bf(prop_ngo_dom_lead1 | weights(iptw) ~ entry + entry_lag1_cumsum +
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model, priors))
}

f_recip_outcome_entry_foreign <- function(dat) {
  dat <- dat %>% 
    filter(laws) %>% 
    mutate(prop_ngo_foreign_lead1 = ifelse(prop_ngo_foreign_lead1 == 1, 0.99, prop_ngo_foreign_lead1))
  
  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model <- brm(
    bf(prop_ngo_foreign_lead1 | weights(iptw) ~ entry + entry_lag1_cumsum +
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model, priors))
}

f_recip_outcome_funding_dom <- function(dat) {
  dat <- dat %>% 
    filter(laws) %>% 
    mutate(prop_ngo_dom_lead1 = ifelse(prop_ngo_dom_lead1 == 1, 0.99, prop_ngo_dom_lead1))
  
  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model <- brm(
    bf(prop_ngo_dom_lead1 | weights(iptw) ~ funding + funding_lag1_cumsum +
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model, priors))
}

f_recip_outcome_funding_foreign <- function(dat) {
  dat <- dat %>% 
    filter(laws) %>% 
    mutate(prop_ngo_foreign_lead1 = ifelse(prop_ngo_foreign_lead1 == 1, 0.99, prop_ngo_foreign_lead1))
  
  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model <- brm(
    bf(prop_ngo_foreign_lead1 | weights(iptw) ~ funding + funding_lag1_cumsum +
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model, priors))
}

f_recip_outcome_ccsi_dom <- function(dat) {
  dat <- dat %>% 
    mutate(prop_ngo_dom_lead1 = ifelse(prop_ngo_dom_lead1 == 1, 0.99, prop_ngo_dom_lead1))
  
  dat_50 <- dat %>% mutate(iptw = ifelse(iptw > 50, 50, iptw))

  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model_50 <- brm(
    bf(prop_ngo_dom_lead1 | weights(iptw) ~ v2xcs_ccsi + v2xcs_ccsi_lag1_cumsum + 
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat_50,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model_50, priors))
}

f_recip_outcome_ccsi_foreign <- function(dat) {
  dat <- dat %>% 
    mutate(prop_ngo_foreign_lead1 = ifelse(prop_ngo_foreign_lead1 == 1, 0.99, prop_ngo_foreign_lead1))
  
  dat_50 <- dat %>% mutate(iptw = ifelse(iptw > 50, 50, iptw))
  
  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model_50 <- brm(
    bf(prop_ngo_foreign_lead1 | weights(iptw) ~ v2xcs_ccsi + v2xcs_ccsi_lag1_cumsum + 
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat_50,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model_50, priors))
}

f_recip_outcome_repress_dom <- function(dat) {
  dat <- dat %>% 
    mutate(prop_ngo_dom_lead1 = ifelse(prop_ngo_dom_lead1 == 1, 0.99, prop_ngo_dom_lead1))
  
  dat_50 <- dat %>% mutate(iptw = ifelse(iptw > 50, 50, iptw))
  
  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model_50 <- brm(
    bf(prop_ngo_dom_lead1 | weights(iptw) ~ v2csreprss + v2csreprss_lag1_cumsum + 
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat_50,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model_50, priors))
}

f_recip_outcome_repress_foreign <- function(dat) {
  dat <- dat %>% 
    mutate(prop_ngo_foreign_lead1 = ifelse(prop_ngo_foreign_lead1 == 1, 0.99, prop_ngo_foreign_lead1))
  
  dat_50 <- dat %>% mutate(iptw = ifelse(iptw > 50, 50, iptw))
  
  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model_50 <- brm(
    bf(prop_ngo_foreign_lead1 | weights(iptw) ~ v2csreprss + v2csreprss_lag1_cumsum + 
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat_50,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$recipients
  )
  
  return(lst(model_50, priors))
}


# Marginal and conditional effects ----------------------------------------

f_mfx_recip_cfx_multiple <- function(model_total, model_advocacy, model_entry, model_funding, model_ccsi, model_repress) {
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

f_mfx_recip_cfx_single <- function(model_total, model_advocacy, model_entry, model_funding, model_ccsi, model_repress) {
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
