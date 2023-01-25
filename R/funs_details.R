# Running modelsummary() on Bayesian models takes a while because of all the
# calculations involved in creating the GOF statistics. With modelsummary 0.7+,
# though it's now possible to build the base model with modelsummary(..., output
# = "modelsummary_list"), save that as an intermediate object, and then feed it
# through modelsummary() again with whatever other output you want. The
# modelsummary_list-based object thus acts like an output-agnostic ur-model.

build_modelsummary <- function(models) {
  msl <- modelsummary::modelsummary(models,
                                    output = "modelsummary_list",
                                    statistic = "[{conf.low}, {conf.high}]",
                                    metrics = c("R2"))
  return(msl)
}

gofs <- tribble(
  ~raw,          ~clean,     ~fmt, ~omit,
  "nobs",        "N",        0,    FALSE,
  "r.squared",   "\\(R^2\\) (total)",    2,    FALSE,
  "r2.marginal", "\\(R^2\\) (marginal)", 2, FALSE
)

coefs_num <- list(
  "b_barriers_total_lag1" = "Treatment (t − 1)",
  "b_barriers_total_lag2_cumsum" = "Treatment history",
  "b_advocacy_lag1" = "Treatment (t − 1)",
  "b_advocacy_lag2_cumsum" = "Treatment history",
  "b_entry_lag1" = "Treatment (t − 1)",
  "b_entry_lag2_cumsum" = "Treatment history",
  "b_funding_lag1" = "Treatment (t − 1)",
  "b_funding_lag2_cumsum" = "Treatment history",
  "b_v2xcs_ccsi_lag1" = "Treatment (t − 1)",
  "b_v2xcs_ccsi_lag2_cumsum" = "Treatment history",
  "b_v2csreprss_lag1" = "Treatment (t − 1)",
  "b_v2csreprss_lag2_cumsum" = "Treatment history",
  "b_Intercept" = "Intercept",
  "sd_gwcode__Intercept" = "Between-country variability (\\(\\sigma_0\\) for \\(b_0\\) offsets)",
  "sigma" = "Model variability (\\(\\sigma_y\\))"
)

coefs_denom <- list(
  # Treatments
  "b_barriers_total_lag1" = "Treatment (t − 1)",
  "b_barriers_total_lag2_cumsum" = "Treatment history",
  "b_advocacy_lag1" = "Treatment (t − 1)",
  "b_advocacy_lag2_cumsum" = "Treatment history",
  "b_entry_lag1" = "Treatment (t − 1)",
  "b_entry_lag2_cumsum" = "Treatment history",
  "b_funding_lag1" = "Treatment (t − 1)",
  "b_funding_lag2_cumsum" = "Treatment history",
  "b_v2xcs_ccsi_lag1" = "Treatment (t − 1)",
  "b_v2xcs_ccsi_lag2_cumsum" = "Treatment history",
  "b_v2csreprss_lag1" = "Treatment (t − 1)",
  "b_v2csreprss_lag2_cumsum" = "Treatment history",
  
  # Lagged outcomes
  "b_total_oda_z_lag1" = "Total ODA (standardized; t − 1)",
  "b_prop_contentious_lag1" = "Proportion of contentious aid (t − 1)",
  "b_prop_ngo_dom_lag1" = "Proportion of USAID aid to domestic NGOs (t − 1)",
  "b_prop_ngo_foreign_lag1" = "Proportion of USAID aid to foreign NGOs (t − 1)",
  
  # Confounders
  "b_v2x_polyarchy" = "Polyarchy",
  "b_v2x_corr" = "Corruption index",
  "b_v2x_rule" = "Rule of law index",
  "b_v2x_civlib" = "Civil liberties index",
  "b_v2x_clphy" = "Physical violence index",
  "b_v2x_clpriv" = "Private civil liberties index",
  "b_gdpcap_log_z" = "Log GDP/capita (standardized)",
  "b_un_trade_pct_gdp" = "Percent of GDP from trade",
  "b_v2peedueq" = "Educational equality index",
  "b_v2pehealth" = "Health equality index",
  "b_e_peinfmor" = "Infant mortality rate",
  "b_internal_conflict_past_5TRUE" = "Internal conflict in past 5 years",
  "b_natural_dis_count" = "Count of natural disasters",
  
  # Other coefficients
  "b_Intercept" = "Intercept",
  "b_year_c" = "Year trend",
  
  # Multilevel and Bayesian things
  "sd_gwcode__Intercept" = "Between-country intercept variability (\\(\\sigma_0\\) for \\(b_0\\) offsets)",
  "sd_gwcode__year_c" = "Between-country year variability (\\(\\sigma_{17}\\) for \\(b_{17}\\) offsets)",
  "cor_gwcode__Intercept__year_c" = "Correlation between random intercepts and slopes (\\(\\rho\\))",
  "sigma" = "Model variability (\\(\\sigma_y\\))"
)

coefs_outcome <- list(
  # Treatments
  "b_barriers_total" = "Treatment (t − 1)",
  "b_barriers_total_lag1_cumsum" = "Treatment history",
  "b_advocacy" = "Treatment (t − 1)",
  "b_advocacy_lag1_cumsum" = "Treatment history",
  "b_entry" = "Treatment (t − 1)",
  "b_entry_lag1_cumsum" = "Treatment history",
  "b_funding" = "Treatment (t − 1)",
  "b_funding_lag1_cumsum" = "Treatment history",
  
  # Other coefficients
  "b_Intercept" = "Intercept",
  "b_year_c" = "Year",
  
  # Model-related things
  "b_hu_year_c" = "Hurdle part: Year",
  "b_hu_Iyear_cE2" = "Hurdle part: Year²",
  "b_hu_Intercept" = "Hurdle part: Intercept",
  
  "b_zi_year_c" = "Zero-inflated part: Year",
  "b_zi_Iyear_cE2" = "Zero-inflated part: Year²",
  "b_zi_Intercept" = "Zero-inflated part: Intercept",
  
  # Multilevel and Bayesian things
  "sd_gwcode__Intercept" = "Between-country intercept variability (\\(\\sigma_0\\) for \\(b_0\\) offsets)",
  "sd_gwcode__year_c" = "Between-country year variability (\\(\\sigma_3\\) for \\(b_3\\) offsets)",
  "cor_gwcode__Intercept__year_c" = "Correlation between random intercepts and slopes (\\(\\rho\\))",
  "sigma" = "Model variability (\\(\\sigma_y\\))",
  "phi" = "Model dispersion (\\(\\phi_y\\))"
)


create_vars_table <- function() {
  vars <- tribble(
    ~category, ~subcategory, ~format, ~term, ~term_clean, ~term_clean_table, ~source,
    "Outcome", "", "dollar", "total_oda", "Total aid", "Total aid (constant 2011 USD, millions)", "OECD and AidData",
    "Outcome", "", "percent", "prop_contentious", "Proportion of contentious aid", "Proportion of contentious aid", "OECD and AidData",
    "Outcome", "", "percent", "prop_ngo_dom", "Proportion of aid to domestic NGOs", "Proportion of aid to domestic NGOs", "USAID",
    "Outcome", "", "percent", "prop_ngo_foreign", "Proportion of aid to foreign NGOs", "Proportion of aid to foreign NGOs", "USAID",
    "Treatment", "", "number", "barriers_total", "Total legal barriers", "Total legal barriers", "@Chaudhry:2016",
    "Treatment", "", "number", "advocacy", "Barriers to advocacy", "Barriers to advocacy", "@Chaudhry:2016",
    "Treatment", "", "number", "entry", "Barriers to entry", "Barriers to entry", "@Chaudhry:2016",
    "Treatment", "", "number", "funding", "Barriers to funding", "Barriers to funding", "@Chaudhry:2016",
    "Treatment", "", "number", "v2xcs_ccsi", "Core civil society index", "Core civil society index", "@Chaudhry:2016",
    "Confounders", "Human rights and politics", "number", "v2x_polyarchy", "Electoral democracy index (polyarchy)", "Electoral democracy index (polyarchy)", "@vdem-v10",
    "Confounders", "Human rights and politics", "number", "v2x_corr", "Political corruption index", "Political corruption index", "@vdem-v10",
    "Confounders", "Human rights and politics", "number", "v2x_rule", "Rule of law index", "Rule of law index", "@vdem-v10",
    "Confounders", "Human rights and politics", "number", "v2x_civlib", "Civil liberties index", "Civil liberties index", "@vdem-v10",
    "Confounders", "Human rights and politics", "number", "v2x_clphy", "Physical violence index", "Physical violence index", "@vdem-v10",
    "Confounders", "Human rights and politics", "number", "v2x_clpriv", "Private civil liberties index", "Private civil liberties index", "@vdem-v10",
    "Confounders", "Economics and development", "dollar", "gdpcap_log", "GDP per capita", "GDP per capita (constant 2011 USD)", "UN",
    "Confounders", "Economics and development", "percent", "un_trade_pct_gdp", "Trade as % of GDP", "Trade as % of GDP", "UN",
    "Confounders", "Economics and development", "number", "v2peedueq", "Educational equality", "Educational equality", "@vdem-v10",
    "Confounders", "Economics and development", "number", "v2pehealth", "Health equality", "Health equality", "@vdem-v10",
    "Confounders", "Economics and development", "number", "e_peinfmor", "Infant mortality rate", "Infant mortality rate (deaths per 1,000 births)", "@vdem-v10",
    "Confounders", "Unexpected shocks", "number", "internal_conflict_past_5", "Internal conflict in last 5 years", "Internal conflict in last 5 years", "UCDP/PRIO",
    "Confounders", "Unexpected shocks", "number", "natural_dis_count", "Natural disasters", "Natural disasters", "EM-DAT"
  )
  
  return(vars)
}

create_ngo_index_table <- function() {
  ngo_index <- tribble(
    ~Index, ~Description, ~Coding,
    "Barriers to entry", "How burdensome is registration?", "Not burdensome = 0; Burdensome = 1",
    "Barriers to entry", "In law, can an NGO appeal if denied registration?", "Yes = 0; No = 1",
    "Barriers to entry", "Are barriers to entry different for NGOs receiving foreign funds?", "Less burdensome = -1; Same = 0; More burdensome = 1",
    "Barriers to funding", "Do NGOs need prior approval from the government to receive foreign funding?", "Yes = 1; No = 0",
    "Barriers to funding", "Are NGOs required to channel foreign funding through state-owned banks or government ministries?", "Yes = 1; No = 0",
    "Barriers to funding", "Are any additional restrictions on foreign support in place?", "Yes = 1; No = 0",
    "Barriers to funding", "Are all NGOs prohibited from receiving foreign funds?", "No = 0; Partially = 0.5; Yes = 1",
    "Barriers to funding", "Is a category of NGOs prohibited from receiving foreign funds?", "No = 0; Partially = 0.5; Yes = 1",
    "Barriers to advocacy", "Does the law restrict NGOs from engaging in political activities?", "No = 0; Partially = 0.5; Yes = 1",
    "Barriers to advocacy", "Are restrictions on political activities different for NGOs receiving foreign funds?", "Less restrictive = -1; Same = 0; More restrictive = 1"
  )
  
  ngo_index_clean <- ngo_index %>% 
    group_by(Index) %>% 
    mutate(Max = n()) %>% 
    ungroup() %>% 
    mutate(Description = fct_inorder(Description))
  
  return(ngo_index_clean)
}
