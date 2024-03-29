# Preliminary models ------------------------------------------------------

f_purpose_prelim_time_only_total <- function(dat) {
  dat <- dat %>% filter(laws)
  
  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model_prior_only <- brm(
    bf(prop_contentious_trunc ~ year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = zero_inflated_beta(),
    prior = priors,
    sample_prior = "only",
    init = "0",
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  model <- brm(
    bf(prop_contentious_trunc ~ year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = zero_inflated_beta(),
    prior = priors,
    init = "0",
    chains = bayes_settings$chains, iter = bayes_settings$iter, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  return(lst(model, priors, model_prior_only))
}


# Treatment models --------------------------------------------------------

f_purpose_treatment_total <- function(dat) {
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(barriers_total ~ barriers_total_lag1 + 
         barriers_total_lag2_cumsum + prop_contentious_lag1 +
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_purpose_treatment_advocacy <- function(dat) {
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(advocacy ~ advocacy_lag1 + advocacy_lag2_cumsum + prop_contentious_lag1 +
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_purpose_treatment_entry <- function(dat) {
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(entry ~ entry_lag1 + entry_lag2_cumsum + prop_contentious_lag1 +
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_purpose_treatment_funding <- function(dat) {
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(funding ~ funding_lag1 + funding_lag2_cumsum + prop_contentious_lag1 +
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_purpose_treatment_ccsi <- function(dat) {
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(v2xcs_ccsi ~ v2xcs_ccsi_lag1 + v2xcs_ccsi_lag2_cumsum + prop_contentious_lag1 +
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}

f_purpose_treatment_repress <- function(dat) {
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  # Denominator model
  priors_denom <- c(prior(student_t(3, 0, 1.5), class = Intercept),
                    prior(student_t(3, 0, 1.5), class = b),
                    prior(exponential(1), class = sigma),
                    prior(exponential(1), class = sd),
                    prior(lkj(2), class = cor))
  
  model_denom <- brm(
    bf(v2csreprss ~ v2csreprss_lag1 + v2csreprss_lag2_cumsum + prop_contentious_lag1 +
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
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  return(lst(model_num, priors_num, model_denom, priors_denom))
}


# Outcome models ----------------------------------------------------------

f_purpose_outcome_total <- function(dat) {
  dat <- dat %>% 
    filter(laws) %>% 
    mutate(prop_contentious_lead1 = ifelse(prop_contentious_lead1 == 1, 0.99, prop_contentious_lead1))
  
  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model <- brm(
    bf(prop_contentious_lead1 | weights(iptw) ~ barriers_total + 
         barriers_total_lag1_cumsum +
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  return(lst(model, priors))
}

f_purpose_outcome_advocacy <- function(dat) {
  dat <- dat %>% 
    filter(laws) %>% 
    mutate(prop_contentious_lead1 = ifelse(prop_contentious_lead1 == 1, 0.99, prop_contentious_lead1))
  
  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model <- brm(
    bf(prop_contentious_lead1 | weights(iptw) ~ advocacy + advocacy_lag1_cumsum +
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  return(lst(model, priors))
}

f_purpose_outcome_entry <- function(dat) {
  dat <- dat %>% 
    filter(laws) %>% 
    mutate(prop_contentious_lead1 = ifelse(prop_contentious_lead1 == 1, 0.99, prop_contentious_lead1))
  
  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model <- brm(
    bf(prop_contentious_lead1 | weights(iptw) ~ entry + entry_lag1_cumsum +
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  return(lst(model, priors))
}

f_purpose_outcome_funding <- function(dat) {
  dat <- dat %>% 
    filter(laws) %>% 
    mutate(prop_contentious_lead1 = ifelse(prop_contentious_lead1 == 1, 0.99, prop_contentious_lead1))
  
  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model <- brm(
    bf(prop_contentious_lead1 | weights(iptw) ~ funding + funding_lag1_cumsum +
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  return(lst(model, priors))
}

f_purpose_outcome_ccsi <- function(dat) {
  dat <- dat %>% 
    mutate(prop_contentious_lead1 = ifelse(prop_contentious_lead1 == 1, 0.99, prop_contentious_lead1))
  
  dat_50 <- dat %>% mutate(iptw = ifelse(iptw > 50, 50, iptw))

  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model_50 <- brm(
    bf(prop_contentious_lead1 | weights(iptw) ~ v2xcs_ccsi + v2xcs_ccsi_lag1_cumsum + 
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat_50,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  return(lst(model_50, priors))
}

f_purpose_outcome_repress <- function(dat) {
  dat <- dat %>% 
    mutate(prop_contentious_lead1 = ifelse(prop_contentious_lead1 == 1, 0.99, prop_contentious_lead1))
  
  dat_50 <- dat %>% mutate(iptw = ifelse(iptw > 50, 50, iptw))

  priors <- c(prior(student_t(3, 0, 1.5), class = Intercept),
              prior(student_t(3, 0, 1.5), class = b),
              prior(exponential(1), class = phi),
              prior(exponential(1), class = sd),
              prior(lkj(2), class = cor),
              prior(student_t(3, 0, 1.5), class = Intercept, dpar = zi),
              prior(student_t(3, 0, 1.5), class = b, dpar = zi))
  
  model_50 <- brm(
    bf(prop_contentious_lead1 | weights(iptw) ~ v2csreprss + v2csreprss_lag1_cumsum + 
         year_c + (1 + year_c | gwcode),
       zi ~ year_c + I(year_c^2),
       decomp = "QR"),
    data = dat_50,
    family = zero_inflated_beta(),
    prior = priors,
    init = 0,
    control = list(adapt_delta = 0.9),
    chains = bayes_settings$chains, iter = bayes_settings$iter * 2, 
    warmup = bayes_settings$warmup, seed = bayes_settings$seed$purpose
  )
  
  return(lst(model_50, priors))
}
