# Running modelsummary() on Bayesian models takes *forever* because of all the
# calculations involved in creating the confidence intervals and all the GOF
# statistics. With modelsummary 0.7, though it's now possible to build the base
# model with modelsummary(..., output = "modelsummary_list", estimate = "",
# statistic = ""), save that as an intermediate object, and then feed it through
# modelsummary() again with whatever other output you want. The
# modelsummary_list-based object thus acts like an output-agnostic ur-model.

build_modelsummary <- function(models) {
  msl <- modelsummary::modelsummary(models,
                                    output = "modelsummary_list",
                                    statistic = "[{conf.low}, {conf.high}]")
  return(msl)
}


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
