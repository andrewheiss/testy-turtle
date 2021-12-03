# Settings ----------------------------------------------------------------

oda_setup <- function() {
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
  
  prior_out <- c(set_prior("normal(0, 20)", class = "Intercept"),
                 set_prior("normal(0, 3)", class = "b"),
                 set_prior("cauchy(0, 1)", class = "sd"))
  
  return(list(chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
              prior_num = prior_num, prior_denom = prior_denom, prior_out = prior_out))
}


# Treatment models --------------------------------------------------------

f_oda_treatment_total <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(barriers_total ~ barriers_total_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  model_denom <- brm(
    bf(barriers_total ~ barriers_total_lag1 + total_oda_log_lag1 +
         # Human rights and politics
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         # Economics and development
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         # Conflict and disasters
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_denom,
    control = list(adapt_delta = 0.9),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_oda_treatment_advocacy <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(advocacy ~ advocacy_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  model_denom <- brm(
    bf(advocacy ~ advocacy_lag1 + total_oda_log_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_denom,
    control = list(adapt_delta = 0.9),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_oda_treatment_entry <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(entry ~ entry_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  model_denom <- brm(
    bf(entry ~ entry_lag1 + total_oda_log_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_denom,
    control = list(adapt_delta = 0.9),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_oda_treatment_funding <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model_num <- brm(
    bf(funding ~ funding_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  model_denom <- brm(
    bf(funding ~ funding_lag1 + total_oda_log_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_denom,
    control = list(adapt_delta = 0.9),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  return(lst(model_num, model_denom))
}

f_oda_treatment_ccsi <- function(dat) {
  oda_settings <- oda_setup()
  
  model_num <- brm(
    bf(v2xcs_ccsi ~ v2xcs_ccsi_lag1 + (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_num,
    control = list(adapt_delta = 0.99),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  model_denom <- brm(
    bf(v2xcs_ccsi ~ v2xcs_ccsi_lag1 + total_oda_log_lag1 +
         v2x_polyarchy + v2x_corr + v2x_rule + v2x_civlib + v2x_clphy + v2x_clpriv +
         gdpcap_log + un_trade_pct_gdp + v2peedueq + v2pehealth + e_peinfmor +
         internal_conflict_past_5 + natural_dis_count +
         (1 | gwcode)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_denom,
    control = list(adapt_delta = 0.9),
    chains = oda_settings$chains, iter = oda_settings$iter,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  return(lst(model_num, model_denom))
}


# Outcome models ----------------------------------------------------------

f_oda_outcome_total <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(total_oda_log_lead1 | weights(iptw) ~ barriers_total + 
         (1 | gwcode) + (1 | year)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_out,
    chains = oda_settings$chains, iter = oda_settings$iter * 2,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  return(model)
}

f_oda_outcome_advocacy <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(total_oda_log_lead1 | weights(iptw) ~ advocacy + 
         (1 | gwcode) + (1 | year)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_out,
    chains = oda_settings$chains, iter = oda_settings$iter * 2,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  return(model)
}

f_oda_outcome_entry <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(total_oda_log_lead1 | weights(iptw) ~ entry + 
         (1 | gwcode) + (1 | year)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_out,
    chains = oda_settings$chains, iter = oda_settings$iter * 2,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  return(model)
}

f_oda_outcome_funding <- function(dat) {
  oda_settings <- oda_setup()
  
  dat <- dat %>% filter(laws)
  
  model <- brm(
    bf(total_oda_log_lead1 | weights(iptw) ~ funding + 
         (1 | gwcode) + (1 | year)),
    data = dat,
    family = gaussian(),
    prior = oda_settings$prior_out,
    chains = oda_settings$chains, iter = oda_settings$iter * 2,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  return(model)
}

f_oda_outcome_ccsi <- function(dat) {
  oda_settings <- oda_setup()
  
  dat_100 <- dat %>% mutate(iptw = ifelse(iptw > 100, 100, iptw))
  dat_500 <- dat %>% mutate(iptw = ifelse(iptw > 500, 500, iptw))
  dat_1000 <- dat %>% mutate(iptw = ifelse(iptw > 1000, 1000, iptw))
  dat_5000 <- dat %>% mutate(iptw = ifelse(iptw > 5000, 5000, iptw))
  
  model_100 <- brm(
    bf(total_oda_log_lead1 | weights(iptw) ~ v2xcs_ccsi + 
         (1 | gwcode) + (1 | year)),
    data = dat_100,
    family = gaussian(),
    prior = oda_settings$prior_out,
    control = list(adapt_delta = 0.9,
                   max_treedepth = 13),
    chains = oda_settings$chains, iter = oda_settings$iter * 2,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  model_500 <- brm(
    bf(total_oda_log_lead1 | weights(iptw) ~ v2xcs_ccsi + 
         (1 | gwcode) + (1 | year)),
    data = dat_500,
    family = gaussian(),
    prior = oda_settings$prior_out,
    control = list(adapt_delta = 0.9,
                   max_treedepth = 13),
    chains = oda_settings$chains, iter = oda_settings$iter * 2,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  model_1000 <- brm(
    bf(total_oda_log_lead1 | weights(iptw) ~ v2xcs_ccsi + 
         (1 | gwcode) + (1 | year)),
    data = dat_1000,
    family = gaussian(),
    prior = oda_settings$prior_out,
    control = list(adapt_delta = 0.9,
                   max_treedepth = 13),
    chains = oda_settings$chains, iter = oda_settings$iter * 2,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  model_5000 <- brm(
    bf(total_oda_log_lead1 | weights(iptw) ~ v2xcs_ccsi + 
         (1 | gwcode) + (1 | year)),
    data = dat_5000,
    family = gaussian(),
    prior = oda_settings$prior_out,
    control = list(adapt_delta = 0.9,
                   max_treedepth = 13),
    chains = oda_settings$chains, iter = oda_settings$iter * 2,
    warmup = oda_settings$warmup, seed = oda_settings$seed
  )
  
  return(lst(model_100, model_500, model_1000, model_5000))
}
