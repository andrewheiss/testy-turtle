# Preliminary models ------------------------------------------------------

f_oda_prelim_time_only_total <- function(dat) {
  dat <- dat %>% filter(laws)
  
  priors <- c(prior(normal(20, 2.5), class = Intercept),
              prior(normal(0, 2), class = b),
              prior(exponential(1), class = sigma),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, -2, 1.5), class = Intercept, dpar = hu),
              prior(student_t(3, 0, 1.5), class = b, dpar = hu))
  
  # Technically we could just use sample_prior = "yes" to do both the prior and
  # posterior sampling simultaneously, but getting the draws out is annoyingly
  # tricky and doesn't (yet) work with tidybayes
  # (https://github.com/mjskay/tidybayes/issues/226), so it's easier to just run
  # two separate models
  model_prior_only <- brm(
    bf(total_oda ~ year_c + (1 + year_c | gwcode),
       hu ~ year_c,
       decomp = "QR"),
    data = dat,
    family = hurdle_lognormal(),
    prior = priors,
    sample_prior = "only",
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )
  
  model <- brm(
    bf(total_oda ~ year_c + (1 + year_c | gwcode),
       hu ~ year_c,
       decomp = "QR"),
    data = dat,
    family = hurdle_lognormal(),
    prior = priors,
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )
  
  return(lst(model, priors, model_prior_only))
}


# Treatment models --------------------------------------------------------

f_oda_treatment_total <- function(dat) {
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
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(barriers_total ~ barriers_total_lag1 + 
         barriers_total_lag2_cumsum + total_oda_z_lag1 +
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
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_oda_treatment_advocacy <- function(dat) {
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )

  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(advocacy ~ advocacy_lag1 + advocacy_lag2_cumsum + total_oda_z_lag1 +
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_oda_treatment_entry <- function(dat) {
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(entry ~ entry_lag1 + entry_lag2_cumsum + total_oda_z_lag1 +
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_oda_treatment_funding <- function(dat) {
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(funding ~ funding_lag1 + funding_lag2_cumsum + total_oda_z_lag1 +
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_oda_treatment_ccsi <- function(dat) {
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )

  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(v2xcs_ccsi ~ v2xcs_ccsi_lag1 + v2xcs_ccsi_lag2_cumsum + total_oda_z_lag1 +
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}


# Outcome models ----------------------------------------------------------

f_oda_outcome_total <- function(dat) {
  dat <- dat %>% filter(laws)
  
  priors <- c(prior(normal(20, 2.5), class = Intercept),
              prior(normal(0, 2), class = b),
              prior(exponential(1), class = sigma),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, -2, 1.5), class = Intercept, dpar = hu),
              prior(student_t(3, 0, 1.5), class = b, dpar = hu))
  
  model <- brm(
    bf(total_oda_lead1 | weights(iptw) ~ barriers_total + 
         barriers_total_lag1_cumsum +
         year_c + (1 + year_c | gwcode),
       hu ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = hurdle_lognormal(),
    prior = priors,
    control = list(adapt_delta = 0.95),
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )
  
  return(lst(model, priors))
}

f_oda_outcome_advocacy <- function(dat) {
  dat <- dat %>% filter(laws)
  
  priors <- c(prior(normal(20, 2.5), class = Intercept),
              prior(normal(0, 2), class = b),
              prior(exponential(1), class = sigma),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, -2, 1.5), class = Intercept, dpar = hu),
              prior(student_t(3, 0, 1.5), class = b, dpar = hu))
  
  model <- brm(
    bf(total_oda_lead1 | weights(iptw) ~ advocacy + advocacy_lag1_cumsum +
         year_c + (1 + year_c | gwcode),
       hu ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = hurdle_lognormal(),
    prior = priors,
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )
  
  return(lst(model, priors))
}

f_oda_outcome_entry <- function(dat) {
  dat <- dat %>% filter(laws)
  
  priors <- c(prior(normal(20, 2.5), class = Intercept),
              prior(normal(0, 2), class = b),
              prior(exponential(1), class = sigma),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, -2, 1.5), class = Intercept, dpar = hu),
              prior(student_t(3, 0, 1.5), class = b, dpar = hu))
  
  model <- brm(
    bf(total_oda_lead1 | weights(iptw) ~ entry + entry_lag1_cumsum +
         year_c + (1 + year_c | gwcode),
       hu ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = hurdle_lognormal(),
    prior = priors,
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )
  
  return(lst(model, priors))
}

f_oda_outcome_funding <- function(dat) {
  dat <- dat %>% filter(laws)
  
  priors <- c(prior(normal(20, 2.5), class = Intercept),
              prior(normal(0, 2), class = b),
              prior(exponential(1), class = sigma),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, -2, 1.5), class = Intercept, dpar = hu),
              prior(student_t(3, 0, 1.5), class = b, dpar = hu))
  
  model <- brm(
    bf(total_oda_lead1 | weights(iptw) ~ funding + funding_lag1_cumsum +
         year_c + (1 + year_c | gwcode),
       hu ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = hurdle_lognormal(),
    prior = priors,
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )
  
  return(lst(model, priors))
}

f_oda_outcome_ccsi <- function(dat) {
  dat_50 <- dat %>% mutate(iptw = ifelse(iptw > 50, 50, iptw))
  dat_500 <- dat %>% mutate(iptw = ifelse(iptw > 500, 500, iptw))
  
  priors <- c(prior(normal(20, 2.5), class = Intercept),
              prior(normal(0, 2), class = b),
              prior(exponential(1), class = sigma),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, -2, 1.5), class = Intercept, dpar = hu),
              prior(student_t(3, 0, 1.5), class = b, dpar = hu))
  
  model_50 <- brm(
    bf(total_oda_lead1 | weights(iptw) ~ v2xcs_ccsi + v2xcs_ccsi_lag1_cumsum + 
         year_c + (1 + year_c | gwcode),
       hu ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat_50,
    family = hurdle_lognormal(),
    prior = priors,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )
  
  model_500 <- brm(
    bf(total_oda_lead1 | weights(iptw) ~ v2xcs_ccsi + v2xcs_ccsi_lag1_cumsum +
         year_c + (1 + year_c | gwcode),
       hu ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat_500,
    family = hurdle_lognormal(),
    prior = priors,
    control = list(adapt_delta = 0.9, max_treedepth = 12),
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2,
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$oda
  )

  return(lst(model_50, model_500, priors))
}
