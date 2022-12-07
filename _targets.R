library(targets)
library(tarchetypes)
library(tibble)

# General variables
csl <- "pandoc/csl/apa.csl"
bibstyle <- "bibstyle-apa"

# Bayes-specific stuff
suppressPackageStartupMessages(library(brms))
options(mc.cores = 4,
        brms.backend = "cmdstanr",
        brms.threads = 2)

bayes_settings <- list(chains = 4, iter = 5000, warmup = 1000,
                       seed = list(general = 6817, oda = 4045,
                                   purpose = 3246, recipients = 7957))


options(tidyverse.quiet = TRUE,
        dplyr.summarise.inform = FALSE)

# By default, R uses polynomial contrasts for ordered factors in linear models
# options("contrasts")
# So make ordered factors use treatment contrasts instead
options(contrasts = rep("contr.treatment", 2))
# Or do it on a single variable:
# contrasts(df$x) <- "contr.treatment"

# Things that get set in options() are not passed down to workers in future (see
# https://github.com/HenrikBengtsson/future/issues/134), which means all these
# neat options we set here disappear when running tar_make_future() (like
# ordered treatment contrasts and the number of cores used, etc.). The official
# recommendation is to add options() calls to the individual workers.
#
# We do this by including options() in the functions where we define model
# priors and other settings (i.e. oda_setup()). But setting options there
# inside a bunch of files can get tedious, since the number of cores, workers,
# etc. depends on the computer we run this on (i.e. my 4-core personal laptop
# vs. my 16-core work laptop).

# Pass these options to workers using options(worker_options)
worker_options <- options()[c("mc.cores", "brms.backend", "brms.threads",
                              "contrasts", "tidyverse.quiet",
                              "dplyr.summarise.inform")]

set.seed(7305)  # From random.org

tar_option_set(packages = c("tidyverse", "here", "fs", "scales", "withr"))

source("R/funs_data-cleaning.R")
source("R/funs_details.R")
source("R/funs_models-iptw.R")
source("R/models_oda.R")
source("R/models_purpose.R")
source("R/models_recipients.R")
source("R/funs_notebook.R")

# here::here() returns an absolute path, which then gets stored in tar_meta and
# becomes computer-specific (i.e. /Users/andrew/Research/blah/thing.Rmd).
# There's no way to get a relative path directly out of here::here(), but
# fs::path_rel() works fine with it (see
# https://github.com/r-lib/here/issues/36#issuecomment-530894167)
here_rel <- function(...) {fs::path_rel(here::here(...))}

# lhs <- function(x) {
#   if (attr(terms(as.formula(x)), which = "response")) {
#     all.vars(x)[1]
#   } else {
#     NULL
#   }
# }

# Pipeline ----------------------------------------------------------------
list(
  ## Helper functions ----
  tar_target(graphic_functions, lst(theme_donors, theme_donors_map, 
                                    set_annotation_fonts, clrs, pts)),
  tar_target(misc_funs, lst(matrix_from_vector)),
  
  ## Raw data files ----
  tar_target(chaudhry_raw_file,
             here_rel("data", "raw_data", "Chaudhry restrictions", "SC_Expanded.dta"),
             format = "file"),
  tar_target(aiddata_raw_file, 
             get_aiddata(
               aiddata_url = "https://github.com/AidData-WM/public_datasets/releases/download/v3.1/AidDataCore_ResearchRelease_Level1_v3.1.zip",
               out_dir = here_rel("data", "raw_data", "AidData"),
               final_name = "AidDataCoreDonorRecipientYearPurpose_ResearchRelease_Level1_v3.1.csv"),
             format = "file"),
  # Current list is at https://webfs.oecd.org/crs-iati-xml/Lookup/DAC-CRS-CODES.xml
  # but the XML structure has changed and it's trickier to identify all the codes
  # systematically now. So instead we use a version from 2016:
  tar_target(dac_purposes_raw_file,
             get_dac_purposes(
               purposes_url = "https://web.archive.org/web/20160819123535/https://www.oecd.org/dac/stats/documentupload/DAC_codeLists.xml",
               out_dir = here_rel("data", "raw_data", "DAC CRS codes")),
             format = "file"),
  tar_target(dac_eligible_raw_file,
             here_rel("data", "manual_data", "oecd_dac_countries.csv")),
  tar_target(usaid_raw_file,
             # https://foreignassistance.gov/data
             get_usaid(
               usaid_url = "https://s3.amazonaws.com/files.explorer.devtechlab.com/us_foreign_aid_complete.csv",
               # If/when that URL breaks, this works, but it's super slow:
               # usaid_url = paste0("https://web.archive.org/web/20210318063824/",
               #                    "https://explorer.usaid.gov/prepared/us_foreign_aid_complete.csv"),
               out_dir = here_rel("data", "raw_data", "USAID")),
             format = "file"),
  tar_target(dcjw_raw_file, 
             here_rel(here("data", "raw_data", 
                           "DCJW NGO laws", "DCJW_NGO_Laws.xlsx")),
             format = "file"),
  tar_target(vdem_raw_file,
             here_rel("data", "raw_data", "Country_Year_V-Dem_Full+others_R_v10",
                      "V-Dem-CY-Full+Others-v10.rds"),
             format = "file"),
  tar_target(un_pop_raw_file,
             here_rel("data", "raw_data", "UN data",
                      "WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx"),
             format = "file"),
  tar_target(un_gdp_constant_raw_file,
             here_rel("data", "raw_data", "UN data",
                      "UNdata_Export_20210118_034054729.csv"),
             format = "file"),
  tar_target(un_gdp_current_raw_file,
             here_rel("data", "raw_data", "UN data",
                      "UNdata_Export_20210118_034311252.csv"),
             format = "file"),
  tar_target(ucdp_raw_file,
             here_rel("data", "raw_data", "UCDP PRIO", "ucdp-prio-acd-191.csv"),
             format = "file"),
  tar_target(disasters_raw_file,
             here_rel("data", "raw_data", "Disasters",
                      "emdat_public_2021_01_16_query_uid-ufBbE2.xlsx"),
             format = "file"),
  tar_target(naturalearth_raw_file,
             here_rel("data", "raw_data", "ne_110m_admin_0_countries",
                      "ne_110m_admin_0_countries.shp"),
             format = "file"),
  tar_target(civicus_raw_file,
             here_rel("data", "raw_data", "Civicus", "civicus_2021-03-19.json"),
             format = "file"),
  

  ## Process and clean data ----
  ### Skeletons and lookups ----
  tar_target(chaudhry_raw, load_chaudhry_raw(chaudhry_raw_file)),
  tar_target(democracies, create_consolidated_democracies()),
  tar_target(skeleton, create_panel_skeleton(democracies, chaudhry_raw)),
  tar_target(regulations, create_regulation_lookup()),

  ### AidData and USAID ----
  tar_target(aiddata_clean, clean_aiddata(aiddata_raw_file)),
  tar_target(aid_donors, build_aid_donors(aiddata_clean)),
  tar_target(aid_recipients, build_aid_recipients(aiddata_clean, skeleton)),
  tar_target(aid_purposes, build_aid_purposes(aiddata_clean)),
  tar_target(aid_purposes_manual, 
             build_aid_purposes_manual(
               dac_purposes_raw_file, 
               here_rel("data", "manual_data",
                        "purpose_codes_contention_WILL_BE_OVERWRITTEN.csv")),
             format = "file"),
  tar_target(aid_purposes_manual_edited, 
             here_rel("data", "manual_data",
                      "purpose_codes_contention.csv")),
  tar_target(aid_purpose_codes_contentiousness, 
             build_aid_contentiousness(aid_purposes_manual_edited)),
  tar_target(aiddata_final, 
             build_aiddata_final(aiddata = aiddata_clean, 
                                 donors = aid_donors, 
                                 recipients = aid_recipients,
                                 purpose_codes = aid_purpose_codes_contentiousness,
                                 skeleton = skeleton,
                                 dac_eligible_raw = dac_eligible_raw_file)),
  tar_target(donor_level_data, build_donor_aiddata(aiddata_final, skeleton)),
  tar_target(usaid_clean, clean_usaid(usaid_raw_file, skeleton)),
  tar_target(donor_level_data_usaid, fix_inflation_usaid(usaid_clean, skeleton)),
  tar_target(usaid_by_country_total, 
             build_usaid_by_country_total(donor_level_data_usaid)),
  tar_target(usaid_by_country_channel, 
             build_usaid_by_country_channel(donor_level_data_usaid)),
  
  ### NGO restrictions ----
  tar_target(dcjw_clean, load_clean_dcjw(dcjw_raw_file, regulations)),
  tar_target(chaudhry_clean, load_clean_chaudhry(chaudhry_raw, regulations)),
  
  ### Other data sources ----
  tar_target(vdem_clean, load_clean_vdem(vdem_raw_file)),
  tar_target(autocracies, build_autocracies(vdem_clean, skeleton)),
  tar_target(wdi_clean, load_clean_wdi(skeleton)),
  tar_target(un_pop, load_clean_un_pop(un_pop_raw_file, skeleton, wdi_clean)),
  tar_target(un_gdp, load_clean_un_gdp(un_gdp_constant_raw_file,
                                       un_gdp_current_raw_file, skeleton)),
  tar_target(ucdp_prio_clean, load_clean_ucdp(ucdp_raw_file)),
  tar_target(disasters_summarized, load_clean_disasters(disasters_raw_file, 
                                                        skeleton)),
  
  ### Combine and lag data ----
  tar_target(country_aid, 
             build_country_data(skeleton, chaudhry_clean, vdem_clean,
                                ucdp_prio_clean, disasters_summarized,
                                aiddata_final, democracies, un_gdp, un_pop,
                                donor_level_data, usaid_by_country_total,
                                usaid_by_country_channel)),
  tar_target(country_aid_complete, fix_country_data(country_aid)),
  tar_target(panel_with_extra_years, make_final_data(country_aid_complete)),
  tar_target(panel_lagged_extra_years, lag_data(panel_with_extra_years)),
  tar_target(country_aid_no_lags, trim_data(panel_with_extra_years)),
  tar_target(country_aid_final, trim_data(panel_lagged_extra_years)),
  tar_target(country_aid_final_winsor, winsorize_one(country_aid_final)),
  
  ### Map and Civicus ----
  tar_target(world_map, load_world_map(naturalearth_raw_file)),
  tar_target(civicus_clean, load_clean_civicus(civicus_raw_file)),
  tar_target(civicus_map_data, create_civicus_map_data(civicus_clean, world_map)),
  
  
  ## Variable details ----
  tar_target(var_details, create_vars_table()),
  tar_target(ngo_index_table, create_ngo_index_table()),

  
  ## Models ----
  ### Models for H1: total aid ----
  tar_target(m_oda_treatment_total, f_oda_treatment_total(trim_oecd(country_aid_final))),
  tar_target(df_oda_iptw_total, create_iptws(trim_oecd(country_aid_final), m_oda_treatment_total)),
  tar_target(m_oda_outcome_total, f_oda_outcome_total(df_oda_iptw_total)),

  tar_target(m_oda_treatment_advocacy, f_oda_treatment_advocacy(trim_oecd(country_aid_final))),
  tar_target(df_oda_iptw_advocacy, create_iptws(trim_oecd(country_aid_final), m_oda_treatment_advocacy)),
  tar_target(m_oda_outcome_advocacy, f_oda_outcome_advocacy(df_oda_iptw_advocacy)),

  tar_target(m_oda_treatment_entry, f_oda_treatment_entry(trim_oecd(country_aid_final))),
  tar_target(df_oda_iptw_entry, create_iptws(trim_oecd(country_aid_final), m_oda_treatment_entry)),
  tar_target(m_oda_outcome_entry, f_oda_outcome_entry(df_oda_iptw_entry)),

  tar_target(m_oda_treatment_funding, f_oda_treatment_funding(trim_oecd(country_aid_final))),
  tar_target(df_oda_iptw_funding, create_iptws(trim_oecd(country_aid_final), m_oda_treatment_funding)),
  tar_target(m_oda_outcome_funding, f_oda_outcome_funding(df_oda_iptw_funding)),

  tar_target(m_oda_treatment_ccsi, f_oda_treatment_ccsi(country_aid_final)),
  tar_target(df_oda_iptw_ccsi, create_iptws(trim_oecd(country_aid_final), m_oda_treatment_ccsi)),
  tar_target(m_oda_outcome_ccsi, f_oda_outcome_ccsi(df_oda_iptw_ccsi)),
  # 
  # ### Models for H2: aid contentiousness ----
  # tar_target(m_purpose_treatment_total, 
  #            f_purpose_treatment_total(country_aid_final_winsor)),
  # tar_target(df_purpose_iptw_total, 
  #            create_iptws(country_aid_final_winsor, m_purpose_treatment_total)),
  # tar_target(m_purpose_outcome_total, 
  #            f_purpose_outcome_total(df_purpose_iptw_total)),
  # 
  # tar_target(m_purpose_treatment_advocacy, 
  #            f_purpose_treatment_advocacy(country_aid_final_winsor)),
  # tar_target(df_purpose_iptw_advocacy, 
  #            create_iptws(country_aid_final_winsor, m_purpose_treatment_advocacy)),
  # tar_target(m_purpose_outcome_advocacy, 
  #            f_purpose_outcome_advocacy(df_purpose_iptw_advocacy)),
  # 
  # tar_target(m_purpose_treatment_entry, 
  #            f_purpose_treatment_entry(country_aid_final_winsor)),
  # tar_target(df_purpose_iptw_entry, 
  #            create_iptws(country_aid_final_winsor, m_purpose_treatment_entry)),
  # tar_target(m_purpose_outcome_entry, 
  #            f_purpose_outcome_entry(df_purpose_iptw_entry)),
  # 
  # tar_target(m_purpose_treatment_funding, 
  #            f_purpose_treatment_funding(country_aid_final_winsor)),
  # tar_target(df_purpose_iptw_funding, 
  #            create_iptws(country_aid_final_winsor, m_purpose_treatment_funding)),
  # tar_target(m_purpose_outcome_funding, 
  #            f_purpose_outcome_funding(df_purpose_iptw_funding)),
  # 
  # tar_target(m_purpose_treatment_ccsi, 
  #            f_purpose_treatment_ccsi(country_aid_final_winsor)),
  # tar_target(df_purpose_iptw_ccsi, 
  #            create_iptws(country_aid_final_winsor, m_purpose_treatment_ccsi)),
  # tar_target(m_purpose_outcome_ccsi, 
  #            f_purpose_outcome_ccsi(df_purpose_iptw_ccsi)),
  # 
  # ### Models for H3: aid recipients ----
  # tar_target(m_recip_treatment_total_dom, 
  #            f_recip_treatment_total_dom(country_aid_final_winsor)),
  # tar_target(df_recip_iptw_total_dom, 
  #            create_iptws(country_aid_final_winsor, m_recip_treatment_total_dom)),
  # tar_target(m_recip_outcome_total_dom, 
  #            f_recip_outcome_total_dom(df_recip_iptw_total_dom)),
  # 
  # tar_target(m_recip_treatment_total_foreign, 
  #            f_recip_treatment_total_foreign(country_aid_final_winsor)),
  # tar_target(df_recip_iptw_total_foreign, 
  #            create_iptws(country_aid_final_winsor, m_recip_treatment_total_foreign)),
  # tar_target(m_recip_outcome_total_foreign, 
  #            f_recip_outcome_total_foreign(df_recip_iptw_total_foreign)),
  # 
  # tar_target(m_recip_treatment_advocacy_dom, 
  #            f_recip_treatment_advocacy_dom(country_aid_final_winsor)),
  # tar_target(df_recip_iptw_advocacy_dom, 
  #            create_iptws(country_aid_final_winsor, m_recip_treatment_advocacy_dom)),
  # tar_target(m_recip_outcome_advocacy_dom, 
  #            f_recip_outcome_advocacy_dom(df_recip_iptw_advocacy_dom)),
  # 
  # tar_target(m_recip_treatment_advocacy_foreign, 
  #            f_recip_treatment_advocacy_foreign(country_aid_final_winsor)),
  # tar_target(df_recip_iptw_advocacy_foreign, 
  #            create_iptws(country_aid_final_winsor, m_recip_treatment_advocacy_foreign)),
  # tar_target(m_recip_outcome_advocacy_foreign, 
  #            f_recip_outcome_advocacy_foreign(df_recip_iptw_advocacy_foreign)),
  # 
  # tar_target(m_recip_treatment_entry_dom, 
  #            f_recip_treatment_entry_dom(country_aid_final_winsor)),
  # tar_target(df_recip_iptw_entry_dom, 
  #            create_iptws(country_aid_final_winsor, m_recip_treatment_entry_dom)),
  # tar_target(m_recip_outcome_entry_dom, 
  #            f_recip_outcome_entry_dom(df_recip_iptw_entry_dom)),
  # 
  # tar_target(m_recip_treatment_entry_foreign, 
  #            f_recip_treatment_entry_foreign(country_aid_final_winsor)),
  # tar_target(df_recip_iptw_entry_foreign, 
  #            create_iptws(country_aid_final_winsor, m_recip_treatment_entry_foreign)),
  # tar_target(m_recip_outcome_entry_foreign, 
  #            f_recip_outcome_entry_foreign(df_recip_iptw_entry_foreign)),
  # 
  # tar_target(m_recip_treatment_funding_dom, 
  #            f_recip_treatment_funding_dom(country_aid_final_winsor)),
  # tar_target(df_recip_iptw_funding_dom, 
  #            create_iptws(country_aid_final_winsor, m_recip_treatment_funding_dom)),
  # tar_target(m_recip_outcome_funding_dom, 
  #            f_recip_outcome_funding_dom(df_recip_iptw_funding_dom)),
  # 
  # tar_target(m_recip_treatment_funding_foreign, 
  #            f_recip_treatment_funding_foreign(country_aid_final_winsor)),
  # tar_target(df_recip_iptw_funding_foreign, 
  #            create_iptws(country_aid_final_winsor, m_recip_treatment_funding_foreign)),
  # tar_target(m_recip_outcome_funding_foreign, 
  #            f_recip_outcome_funding_foreign(df_recip_iptw_funding_foreign)),

  ## Model tables ----
  # Build tables here because they take forever
  # Note tibble::lst() instead of base::list(); lst() auto-names the elements by
  # their object names

  # H1
  tar_target(models_tbl_h1_treatment_num,
             build_modelsummary(lst(m_oda_treatment_total$model_num,
                                    m_oda_treatment_advocacy$model_num,
                                    m_oda_treatment_entry$model_num,
                                    m_oda_treatment_funding$model_num,
                                    m_oda_treatment_ccsi$model_num))),
  tar_target(models_tbl_h1_treatment_denom,
             build_modelsummary(lst(m_oda_treatment_total$model_denom,
                                    m_oda_treatment_advocacy$model_denom,
                                    m_oda_treatment_entry$model_denom,
                                    m_oda_treatment_funding$model_denom,
                                    m_oda_treatment_ccsi$model_denom))),
  tar_target(models_tbl_h1_outcome_dejure,
             build_modelsummary(lst(m_oda_outcome_total, m_oda_outcome_advocacy,
                                    m_oda_outcome_entry, m_oda_outcome_funding))),
  tar_target(models_tbl_h1_outcome_defacto,
             build_modelsummary(lst(m_oda_outcome_ccsi$model_100,
                                    m_oda_outcome_ccsi$model_500,
                                    m_oda_outcome_ccsi$model_1000,
                                    m_oda_outcome_ccsi$model_5000)))#,
  # 
  # # H2
  # tar_target(models_tbl_h2_treatment_num,
  #            build_modelsummary(lst(m_purpose_treatment_total$model_num,
  #                                   m_purpose_treatment_advocacy$model_num,
  #                                   m_purpose_treatment_entry$model_num,
  #                                   m_purpose_treatment_funding$model_num,
  #                                   m_purpose_treatment_ccsi$model_num))),
  # tar_target(models_tbl_h2_treatment_denom,
  #            build_modelsummary(lst(m_purpose_treatment_total$model_denom,
  #                                   m_purpose_treatment_advocacy$model_denom,
  #                                   m_purpose_treatment_entry$model_denom,
  #                                   m_purpose_treatment_funding$model_denom,
  #                                   m_purpose_treatment_ccsi$model_denom))),
  # tar_target(models_tbl_h2_outcome_dejure,
  #            build_modelsummary(lst(m_purpose_outcome_total,
  #                                   m_purpose_outcome_advocacy,
  #                                   m_purpose_outcome_entry,
  #                                   m_purpose_outcome_funding))),
  # tar_target(models_tbl_h2_outcome_defacto,
  #            build_modelsummary(lst(m_purpose_outcome_ccsi$model_100, 
  #                                   m_purpose_outcome_ccsi$model_500,
  #                                   m_purpose_outcome_ccsi$model_1000, 
  #                                   m_purpose_outcome_ccsi$model_5000))),
  # 
  # ## Analysis notebook ----
  # tar_notebook_pages()
  
  # This is only here to trigger a re-build of the R Markdown website's
  # supporting files in `_site`, which copies the files in `output` to
  # `_site/output`. I unfortunately haven't found a way to make it so that the
  # site building occurs independently of `rmarkdown::render()`, so this is the
  # workaround: re-knit index.Rmd
  # tar_target(supporting_files, 
  #            copy_notebook_supporting_files(here_rel("analysis", "index.Rmd"), 
  #                                           main_manuscript, html))
  
)
