# Settings ----------------------------------------------------------------

purpose_setup <- function() {
  options(worker_options)
  
  # Settings
  CHAINS <- 4
  ITER <- 2000
  WARMUP <- 1000
  BAYES_SEED <- 3246  # From random.org
  
  # Priors
  prior_num <- c(set_prior("normal(0, 10)", class = "Intercept"),
                 set_prior("normal(0, 2.5)", class = "b"),
                 set_prior("cauchy(0, 1)", class = "sd"))
  
  prior_denom <- c(set_prior("normal(0, 10)", class = "Intercept"),
                   set_prior("normal(0, 2.5)", class = "b"),
                   set_prior("cauchy(0, 1)", class = "sd"))
  
  prior_out_logit <-  c(set_prior("normal(0, 10)", class = "Intercept"),
                        set_prior("normal(0, 2.5)", class = "b"),
                        set_prior("cauchy(0, 1)", class = "sd"))
  
  return(list(chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
              prior_num = prior_num, prior_denom = prior_denom, 
              prior_out_logit = prior_out_logit))
}


# Treatment models --------------------------------------------------------

f_purpose_treatment_total <- function(dat) {
  purpose_settings <- purpose_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(barriers_total ~ barriers_total_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = purpose_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = purpose_settings$chains, iter = purpose_settings$iter,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  model_denom <- brm(
    bf(barriers_total ~ barriers_total_lag1 + prop_contentious_logit_lag1 +
         # Human rights and politics
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         # Economics and development
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         # Conflict and disasters
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = purpose_settings$prior_denom,
    control = list(adapt_delta = 0.9),
    chains = purpose_settings$chains, iter = purpose_settings$iter,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_purpose_treatment_advocacy <- function(dat) {
  purpose_settings <- purpose_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(advocacy ~ advocacy_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = purpose_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = purpose_settings$chains, iter = purpose_settings$iter,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  model_denom <- brm(
    bf(advocacy ~ advocacy_lag1 + prop_contentious_logit_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = purpose_settings$prior_denom,
    control = list(adapt_delta = 0.9),
    chains = purpose_settings$chains, iter = purpose_settings$iter,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_purpose_treatment_entry <- function(dat) {
  purpose_settings <- purpose_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(entry ~ entry_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = purpose_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = purpose_settings$chains, iter = purpose_settings$iter,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  model_denom <- brm(
    bf(entry ~ entry_lag1 + prop_contentious_logit_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = purpose_settings$prior_denom,
    control = list(adapt_delta = 0.9),
    chains = purpose_settings$chains, iter = purpose_settings$iter,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_purpose_treatment_funding <- function(dat) {
  purpose_settings <- purpose_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(funding ~ funding_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = purpose_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = purpose_settings$chains, iter = purpose_settings$iter,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  model_denom <- brm(
    bf(funding ~ funding_lag1 + prop_contentious_logit_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = purpose_settings$prior_denom,
    control = list(adapt_delta = 0.9),
    chains = purpose_settings$chains, iter = purpose_settings$iter,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_purpose_treatment_ccsi <- function(dat) {
  purpose_settings <- purpose_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(v2xcs_ccsi ~ v2xcs_ccsi_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = purpose_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = purpose_settings$chains, iter = purpose_settings$iter,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  model_denom <- brm(
    bf(v2xcs_ccsi ~ v2xcs_ccsi_lag1 + prop_contentious_logit_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = purpose_settings$prior_denom,
    control = list(adapt_delta = 0.9,
                   max_treedepth = 13),
    chains = purpose_settings$chains, iter = purpose_settings$iter,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  return(lst(model_num, model_denom))
}


# Outcome models ----------------------------------------------------------

f_purpose_outcome_total <- function(dat) {
  purpose_settings <- purpose_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(prop_contentious_logit_lead1 | weights(iptw) ~ barriers_total + 
         (1 | gwcode) + (1 | year)),
    data = dat,
    family = gaussian(),
    prior = purpose_settings$prior_out_logit,
    chains = purpose_settings$chains, iter = purpose_settings$iter * 2,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  return(model)
}

f_purpose_outcome_advocacy <- function(dat) {
  purpose_settings <- purpose_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(prop_contentious_logit_lead1 | weights(iptw) ~ advocacy + 
         (1 | gwcode) + (1 | year)),
    data = dat,
    family = gaussian(),
    prior = purpose_settings$prior_out_logit,
    chains = purpose_settings$chains, iter = purpose_settings$iter * 2,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  return(model)
}

f_purpose_outcome_entry <- function(dat) {
  purpose_settings <- purpose_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(prop_contentious_logit_lead1 | weights(iptw) ~ entry + 
         (1 | gwcode) + (1 | year)),
    data = dat,
    family = gaussian(),
    prior = purpose_settings$prior_out_logit,
    chains = purpose_settings$chains, iter = purpose_settings$iter * 2,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  return(model)
}

f_purpose_outcome_funding <- function(dat) {
  purpose_settings <- purpose_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(prop_contentious_logit_lead1 | weights(iptw) ~ funding + 
         (1 | gwcode) + (1 | year)),
    data = dat,
    family = gaussian(),
    prior = purpose_settings$prior_out_logit,
    chains = purpose_settings$chains, iter = purpose_settings$iter * 2,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  return(model)
}

# Including (1 | year) here blows up the models and makes them not converge at
# all (see https://twitter.com/andrewheiss/status/1396545208163127297)
f_purpose_outcome_ccsi <- function(dat) {
  purpose_settings <- purpose_setup()
  
  # Temporary stuff bc models are blowing up with (1 | gwcode)
  purpose_settings$prior_out_logit <- c(set_prior("normal(0, 10)", class = "Intercept"),
                                        set_prior("normal(0, 2.5)", class = "b"))
  
  dat <- dat %>% filter(laws)
  
  dat_100 <- dat %>% mutate(iptw = ifelse(iptw > 100, 100, iptw))
  dat_500 <- dat %>% mutate(iptw = ifelse(iptw > 500, 500, iptw))
  dat_1000 <- dat %>% mutate(iptw = ifelse(iptw > 1000, 1000, iptw))
  dat_5000 <- dat %>% mutate(iptw = ifelse(iptw > 5000, 5000, iptw))
  
  model_100 <- brm(
    bf(prop_contentious_logit_lead1 | weights(iptw) ~ v2xcs_ccsi),# + 
         # (1 | gwcode)),
    data = dat,
    family = gaussian(),
    control = list(adapt_delta = 0.9,
                   max_treedepth = 13),
    prior = purpose_settings$prior_out_logit,
    chains = purpose_settings$chains, iter = purpose_settings$iter * 2,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  model_500 <- brm(
    bf(prop_contentious_logit_lead1 | weights(iptw) ~ v2xcs_ccsi),# + 
         # (1 | gwcode)),
    data = dat,
    family = gaussian(),
    control = list(adapt_delta = 0.9,
                   max_treedepth = 13),
    prior = purpose_settings$prior_out_logit,
    chains = purpose_settings$chains, iter = purpose_settings$iter * 2,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  model_1000 <- brm(
    bf(prop_contentious_logit_lead1 | weights(iptw) ~ v2xcs_ccsi),# + 
         # (1 | gwcode)),
    data = dat,
    family = gaussian(),
    control = list(adapt_delta = 0.9,
                   max_treedepth = 13),
    prior = purpose_settings$prior_out_logit,
    chains = purpose_settings$chains, iter = purpose_settings$iter * 2,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  model_5000 <- brm(
    bf(prop_contentious_logit_lead1 | weights(iptw) ~ v2xcs_ccsi),# + 
         # (1 | gwcode)),
    data = dat,
    family = gaussian(),
    control = list(adapt_delta = 0.9,
                   max_treedepth = 13),
    prior = purpose_settings$prior_out_logit,
    chains = purpose_settings$chains, iter = purpose_settings$iter * 2,
    warmup = purpose_settings$warmup, seed = purpose_settings$seed
  )
  
  return(lst(model_100, model_500, model_1000, model_5000))
}
