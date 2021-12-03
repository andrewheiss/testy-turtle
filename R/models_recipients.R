# Settings ----------------------------------------------------------------

recip_setup <- function() {
  options(worker_options)
  
  # Settings
  CHAINS <- 4
  ITER <- 2000
  WARMUP <- 1000
  BAYES_SEED <- 4045  # From random.org
  
  # Priors
  prior_num <- c(set_prior("normal(0, 10)", class = "Intercept"),
                 set_prior("normal(0, 2.5)", class = "b"),
                 set_prior("cauchy(0, 1)", class = "sd"))
  
  prior_denom <- c(set_prior("normal(0, 10)", class = "Intercept"),
                   set_prior("normal(0, 2.5)", class = "b"),
                   set_prior("normal(0, 2.5)", class = "sd"))
  
  prior_out <- c(set_prior("normal(0, 10)", class = "Intercept"),
                 set_prior("normal(0, 3)", class = "b"),
                 set_prior("cauchy(0, 1)", class = "sd"),
                 set_prior("logistic(-0.5, 0.35)", class = "Intercept", dpar = "zi"),
                 set_prior("gamma(0.01, 0.01)", class = "phi"))
  
  return(list(chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
              prior_num = prior_num, prior_denom = prior_denom, prior_out = prior_out))
}


# Treatment models --------------------------------------------------------

f_recip_treatment_total_dom <- function(dat) {
  recip_settings <- recip_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(barriers_total ~ barriers_total_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = recip_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = recip_settings$chains, iter = recip_settings$iter,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  model_denom <- brm(
    bf(barriers_total ~ barriers_total_lag1 + prop_ngo_dom_lag1 +
         # Human rights and politics
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         # Economics and development
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         # Conflict and disasters
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = recip_settings$prior_num,
    control = list(adapt_delta = 0.9),
    chains = recip_settings$chains, iter = recip_settings$iter,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_recip_treatment_total_foreign <- function(dat) {
  recip_settings <- recip_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(barriers_total ~ barriers_total_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = recip_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = recip_settings$chains, iter = recip_settings$iter,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  model_denom <- brm(
    bf(barriers_total ~ barriers_total_lag1 + prop_ngo_foreign_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = recip_settings$prior_num,
    control = list(adapt_delta = 0.9),
    chains = recip_settings$chains, iter = recip_settings$iter,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_recip_treatment_advocacy_dom <- function(dat) {
  recip_settings <- recip_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(advocacy ~ advocacy_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = recip_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = recip_settings$chains, iter = recip_settings$iter,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  model_denom <- brm(
    bf(advocacy ~ advocacy_lag1 + prop_ngo_dom_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = recip_settings$prior_num,
    control = list(adapt_delta = 0.9),
    chains = recip_settings$chains, iter = recip_settings$iter,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_recip_treatment_advocacy_foreign <- function(dat) {
  recip_settings <- recip_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(advocacy ~ advocacy_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = recip_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = recip_settings$chains, iter = recip_settings$iter,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  model_denom <- brm(
    bf(advocacy ~ advocacy_lag1 + prop_ngo_foreign_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = recip_settings$prior_num,
    control = list(adapt_delta = 0.9),
    chains = recip_settings$chains, iter = recip_settings$iter,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_recip_treatment_entry_dom <- function(dat) {
  recip_settings <- recip_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(entry ~ entry_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = recip_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = recip_settings$chains, iter = recip_settings$iter,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  model_denom <- brm(
    bf(entry ~ entry_lag1 + prop_ngo_dom_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = recip_settings$prior_num,
    control = list(adapt_delta = 0.9),
    chains = recip_settings$chains, iter = recip_settings$iter,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_recip_treatment_entry_foreign <- function(dat) {
  recip_settings <- recip_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(entry ~ entry_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = recip_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = recip_settings$chains, iter = recip_settings$iter,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  model_denom <- brm(
    bf(entry ~ entry_lag1 + prop_ngo_foreign_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = recip_settings$prior_num,
    control = list(adapt_delta = 0.9),
    chains = recip_settings$chains, iter = recip_settings$iter,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_recip_treatment_funding_dom <- function(dat) {
  recip_settings <- recip_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(funding ~ funding_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = recip_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = recip_settings$chains, iter = recip_settings$iter,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  model_denom <- brm(
    bf(funding ~ funding_lag1 + prop_ngo_dom_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = recip_settings$prior_num,
    control = list(adapt_delta = 0.9),
    chains = recip_settings$chains, iter = recip_settings$iter,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_recip_treatment_funding_foreign <- function(dat) {
  recip_settings <- recip_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(funding ~ funding_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = recip_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = recip_settings$chains, iter = recip_settings$iter,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  model_denom <- brm(
    bf(funding ~ funding_lag1 + prop_ngo_foreign_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = recip_settings$prior_num,
    control = list(adapt_delta = 0.9),
    chains = recip_settings$chains, iter = recip_settings$iter,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  return(lst(model_num, model_denom))
}


# Outcome models ----------------------------------------------------------

f_recip_outcome_total_dom <- function(dat) {
  recip_settings <- recip_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(prop_ngo_dom_lead1 | weights(iptw) ~ 
         barriers_total + (1 | gwcode),
       zi ~ 1),
    data = dat,
    family = zero_inflated_beta(),
    prior = recip_settings$prior_out,
    chains = recip_settings$chains, iter = recip_settings$iter * 2,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  return(model)
}

f_recip_outcome_total_foreign <- function(dat) {
  recip_settings <- recip_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(prop_ngo_foreign_lead1 | weights(iptw) ~ 
         barriers_total + (1 | gwcode),
       zi ~ 1),
    data = dat,
    family = zero_inflated_beta(),
    prior = recip_settings$prior_out,
    chains = recip_settings$chains, iter = recip_settings$iter * 2,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  return(model)
}

f_recip_outcome_advocacy_dom <- function(dat) {
  recip_settings <- recip_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(prop_ngo_dom_lead1 | weights(iptw) ~ 
         advocacy + (1 | gwcode),
       zi ~ 1),
    data = dat,
    family = zero_inflated_beta(),
    prior = recip_settings$prior_out,
    chains = recip_settings$chains, iter = recip_settings$iter * 2,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  return(model)
}

f_recip_outcome_advocacy_foreign <- function(dat) {
  recip_settings <- recip_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(prop_ngo_foreign_lead1 | weights(iptw) ~ 
         advocacy + (1 | gwcode),
       zi ~ 1),
    data = dat,
    family = zero_inflated_beta(),
    prior = recip_settings$prior_out,
    chains = recip_settings$chains, iter = recip_settings$iter * 2,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  return(model)
}

f_recip_outcome_entry_dom <- function(dat) {
  recip_settings <- recip_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(prop_ngo_dom_lead1 | weights(iptw) ~ 
         entry + (1 | gwcode),
       zi ~ 1),
    data = dat,
    family = zero_inflated_beta(),
    prior = recip_settings$prior_out,
    chains = recip_settings$chains, iter = recip_settings$iter * 2,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  return(model)
}

f_recip_outcome_entry_foreign <- function(dat) {
  recip_settings <- recip_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(prop_ngo_foreign_lead1 | weights(iptw) ~ 
         entry + (1 | gwcode),
       zi ~ 1),
    data = dat,
    family = zero_inflated_beta(),
    prior = recip_settings$prior_out,
    chains = recip_settings$chains, iter = recip_settings$iter * 2,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  return(model)
}

f_recip_outcome_funding_dom <- function(dat) {
  recip_settings <- recip_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(prop_ngo_dom_lead1 | weights(iptw) ~ 
         funding + (1 | gwcode),
       zi ~ 1),
    data = dat,
    family = zero_inflated_beta(),
    prior = recip_settings$prior_out,
    chains = recip_settings$chains, iter = recip_settings$iter * 2,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  return(model)
}

f_recip_outcome_funding_foreign <- function(dat) {
  recip_settings <- recip_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(prop_ngo_foreign_lead1 | weights(iptw) ~ 
         funding + (1 | gwcode),
       zi ~ 1),
    data = dat,
    family = zero_inflated_beta(),
    prior = recip_settings$prior_out,
    chains = recip_settings$chains, iter = recip_settings$iter * 2,
    warmup = recip_settings$warmup, seed = recip_settings$seed
  )
  
  return(model)
}
