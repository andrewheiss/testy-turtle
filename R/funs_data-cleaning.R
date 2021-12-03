library(states)
library(countrycode)
suppressPackageStartupMessages(library(lubridate))
library(haven)
library(httr)
library(xml2)
library(readxl)
library(WDI)
suppressPackageStartupMessages(library(sf))
library(jsonlite)


# Lookup tables -----------------------------------------------------------

create_consolidated_democracies <- function() {
  consolidated_democracies <- 
    tibble(country_name = c("Andorra", "Australia", "Austria", "Bahamas", 
                            "Barbados", "Belgium", "Canada", "Denmark", "Finland", 
                            "France", "Germany", "Greece", "Grenada", "Iceland", 
                            "Ireland", "Italy", "Japan", "Liechtenstein", "Luxembourg", 
                            "Malta", "Monaco", "Netherlands", "New Zealand", "Norway", 
                            "San Marino", "Spain", "Sweden", "Switzerland", 
                            "United Kingdom", "United States of America")) %>% 
    # Ignore these 5 microstates, since they're not in the panel skeleton
    filter(!(country_name %in% c("Andorra", "Grenada", "Liechtenstein", 
                                 "Monaco", "San Marino"))) %>% 
    mutate(iso3 = countrycode(country_name, "country.name", "iso3c"),
           gwcode = countrycode(country_name, "country.name", "gwn"))
  
  return(consolidated_democracies)
}

create_regulation_lookup <- function() {
  regulations <- tribble(
    ~question, ~barrier,       ~question_clean,                  ~ignore_in_index, ~question_display,
    "q1a",     "association",  "const_assoc",                    TRUE,             "Constitutional associational rights",
    "q1b",     "association",  "political_parties",              TRUE,             "Citizens form political parties",
    "q2a",     "entry",        "ngo_register",                   TRUE,             "NGO registration required",
    "q2b",     "entry",        "ngo_register_burden",            FALSE,            "NGO registration burdensome",
    "q2c",     "entry",        "ngo_register_appeal",            FALSE,            "NGO registration appealXXXnot allowed",
    "q2d",     "entry",        "ngo_barrier_foreign_funds",      FALSE,            "Registration barriers differentXXXif foreign funds involved",
    "q3a",     "funding",      "ngo_disclose_funds",             TRUE,             "Funds must be disclosed",
    "q3b",     "funding",      "ngo_foreign_fund_approval",      FALSE,            "Prior approval requiredXXXfor foreign funds",
    "q3c",     "funding",      "ngo_foreign_fund_channel",       FALSE,            "Foreign funds channeledXXXthrough government",
    "q3d",     "funding",      "ngo_foreign_fund_restrict",      FALSE,            "Foreign funds restricted",
    "q3e",     "funding",      "ngo_foreign_fund_prohibit",      FALSE,            "Foreign funds prohibited",
    "q3f",     "funding",      "ngo_type_foreign_fund_prohibit", FALSE,            "Foreign funds prohibitedXXXfor some types of NGOs",
    "q4a",     "advocacy",     "ngo_politics",                   FALSE,            "NGOs restricted from politics",
    "q4b",     "advocacy",     "ngo_politics_intimidation",      TRUE,             "NGOs intimidated from politics",
    "q4c",     "advocacy",     "ngo_politics_foreign_fund",      FALSE,            "Political barriers differentXXXif foreign funds involved"
  )
  
  return(regulations)
}


# Panel skeleton ----------------------------------------------------------

load_chaudhry_raw <- function(path) {
  # In this data Sudan (625) splits into North Sudan (626) and South Sudan (525)
  # in 2011, but in the other datasets regular Sudan stays 625 and South Sudan
  # becomes 626, so adjust the numbers here
  #
  # Also, Chad is in the dataset, but all values are missing, so we drop it
  chaudhry_raw <- read_dta(path) %>% 
    filter(ccode != 483) %>%  # Remove Chad
    mutate(ccode = case_when(
      scode == "SSU" ~ 626,
      scode == "SDN" ~ 625,
      TRUE ~ ccode
    )) %>% 
    mutate(gwcode = countrycode(ccode, origin = "cown", destination = "gwn",
                                custom_match = c("679" = 678L, "818" = 816L,
                                                 "342" = 345L, "341" = 347L,
                                                 "348" = 341L, "315" = 316L)))
  
  return(chaudhry_raw)
}

create_panel_skeleton <- function(consolidated_democracies, chaudhry_raw) {
  microstates <- gwstates %>%
    filter(microstate) %>% distinct(gwcode) %>% 
    as_tibble()
  
  chaudhry_countries <- chaudhry_raw %>% distinct(gwcode)
  
  # In both COW and GW codes, modern Vietnam is 816, but countrycode() thinks the
  # COW code is 817, which is old South Vietnam (see issue
  # https://github.com/vincentarelbundock/countrycode/issues/16), so we use
  # custom_match to force 816 to recode to 816
  #
  # Also, following Gleditsch and Ward, we treat Serbia after 2006 dissolution of
  # Serbia & Montenegro as 345 in COW codes (see
  # https://www.andybeger.com/states/articles/differences-gw-cow.html)
  #
  # Following V-Dem, we treat Czechoslovakia (GW/COW 315) and Czech Republic
  # (GW/COW 316) as the same continuous country (V-Dem has both use ID 157).
  #
  # Also, because the World Bank doesn't include it in the WDI, we omit
  # Taiwan (713). We also omit East Germany (265) and South Yemen (680).
  panel_skeleton_all <- state_panel(1980, 2018, partial = "any") %>% 
    # Remove microstates
    filter(!(gwcode %in% microstates$gwcode)) %>% 
    # Remove East Germany, South Yemen, Taiwan, the Bahamas, Belize, and Brunei
    filter(!(gwcode %in% c(265, 680, 713, 31, 80, 835))) %>%
    # Deal with Czechia
    mutate(gwcode = recode(gwcode, `315` = 316L)) %>% 
    mutate(cowcode = countrycode(gwcode, origin = "gwn", destination = "cown",
                                 custom_match = c("816" = 816L, "340" = 345L)),
           country = countrycode(cowcode, origin = "cown", destination = "country.name",
                                 custom_match = c("678" = "Yemen")),
           iso2 = countrycode(cowcode, origin = "cown", destination = "iso2c",
                              custom_match = c("345" = "RS", "347" = "XK", "678" = "YE")),
           iso3 = countrycode(cowcode, origin = "cown", destination = "iso3c",
                              custom_match = c("345" = "SRB", "347" = "XKK", "678" = "YEM")),
           # Use 999 as the UN country code for Kosovo
           un = countrycode(cowcode, origin = "cown", destination = "un",
                            custom_match = c("345" = 688, "347" = 999, "678" = 887)),
           region = countrycode(cowcode, origin = "cown", destination = "region"),
           un_region = countrycode(cowcode, origin = "cown", destination = "un.region.name",
                                   custom_match = c("345" = "Europe", 
                                                    "347" = "Europe", 
                                                    "678" = "Asia")),
           un_subregion = countrycode(cowcode, origin = "cown", 
                                      destination = "un.regionsub.name",
                                      custom_match = c("345" = "Eastern Europe", 
                                                       "347" = "Eastern Europe", 
                                                       "678" = "Western Asia"))) %>% 
    # There are two entries for "Yugoslavia" in 2006 after recoding 340 as 345;
    # get rid of one
    filter(!(gwcode == 340 & cowcode == 345 & year == 2006)) %>% 
    # Make Serbia 345 in GW codes too, for joining with other datasets
    mutate(gwcode = recode(gwcode, `340` = 345L)) %>%
    mutate(country = recode(country, `Yugoslavia` = "Serbia")) %>%
    arrange(gwcode, year)
  
  panel_skeleton <- panel_skeleton_all %>% 
    filter(gwcode %in% chaudhry_countries$gwcode) %>% 
    filter(!(gwcode %in% consolidated_democracies$gwcode)) %>% 
    as_tibble()
  
  skeleton_lookup <- panel_skeleton %>% 
    group_by(gwcode, cowcode, country, iso2, iso3, un, 
             region, un_region, un_subregion) %>% 
    summarize(years_included = n()) %>% 
    ungroup() %>% 
    arrange(country)
  
  return(lst(panel_skeleton, panel_skeleton_all, microstates, skeleton_lookup))
}


# AidData and OECD stuff --------------------------------------------------

get_aiddata <- function(aiddata_url, out_dir, final_name) {
  aiddata_zip_name <- basename(aiddata_url)  # .zip file only
  aiddata_name <- tools::file_path_sans_ext(aiddata_zip_name)  # .zip sans extension
  
  # Download .zip file
  aiddata_get <- GET(aiddata_url, 
                     write_disk(here_rel(out_dir, aiddata_zip_name),
                                overwrite = TRUE), 
                     progress())
  # Unzip it
  unzip(here_rel(out_dir, aiddata_zip_name), exdir = out_dir)
  
  # Clean up zip file and unnecessary CSV files
  delete_zip <- file.remove(here_rel(out_dir, aiddata_zip_name))
  delete_other_csvs <- list.files(out_dir, pattern = "csv", full.names = TRUE) %>%
    map(~ ifelse(str_detect(.x, "DonorRecipientYearPurpose"), 0,
                 file.remove(here_rel(.x))))
  
  return(here_rel(out_dir, final_name))
}

get_dac_purposes <- function(purposes_url, out_dir) {
  purposes_name <- basename(purposes_url)
  
  purposes_get <- GET(purposes_url, 
                      write_disk(here_rel(out_dir, purposes_name),
                                 overwrite = TRUE), 
                      progress())
  
  return(here_rel(out_dir, purposes_name))
}

clean_aiddata <- function(aiddata_raw) {
  aiddata_clean <- read_csv(aiddata_raw, col_types = cols()) %>% 
    # Get rid of non-country recipients
    filter(!str_detect(recipient,
                       regex("regional|unspecified|multi|value|global|commission", 
                             ignore_case = TRUE))) %>%
    filter(year < 9999) %>%
    mutate(purpose_code_short = as.integer(str_sub(coalesced_purpose_code, 1, 3)))
  
  return(aiddata_clean)
}

build_aid_donors <- function(aiddata) {
  # Donor, recipient, and purpose details
  # I pulled these country names out of the dropdown menu at OECD.Stat Table 2a
  # online: https://stats.oecd.org/Index.aspx?DataSetCode=Table2A
  dac_donors <- c("Australia", "Austria", "Belgium", "Canada", "Czech Republic",
                  "Denmark", "Finland", "France", "Germany", "Greece", "Iceland",
                  "Ireland", "Italy", "Japan", "Korea", "Luxembourg", "Netherlands",
                  "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic",
                  "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom",
                  "United States")
  
  non_dac_donors <- c("Bulgaria", "Croatia", "Cyprus", "Estonia", "Hungary",
                      "Israel", "Kazakhstan", "Kuwait", "Latvia", "Liechtenstein",
                      "Lithuania", "Malta", "Romania", "Russia", "Saudi Arabia",
                      "Chinese Taipei", "Thailand", "Timor Leste", "Turkey",
                      "United Arab Emirates")
  
  other_countries <- c("Brazil", "Chile", "Colombia", "India", "Monaco", "Qatar",
                       "South Africa", "Taiwan")
  
  donors_all <- aiddata %>%
    distinct(donor) %>%
    mutate(donor_type = case_when(
      donor %in% c(dac_donors, non_dac_donors, other_countries) ~ "Country",
      donor == "Bill & Melinda Gates Foundation" ~ "Private donor",
      TRUE ~ "Multilateral or IGO"
    ))
  
  donor_countries <- donors_all %>% 
    filter(donor_type == "Country") %>% 
    mutate(donor_gwcode = countrycode(donor, "country.name", "gwn",
                                      custom_match = c("Liechtenstein" = 223,
                                                       "Monaco" = 221)),
           donor_iso3 = countrycode(donor, "country.name", "iso3c"))
  
  donors <- bind_rows(filter(donors_all, donor_type != "Country"),
                      donor_countries)
  
  return(donors)
}

build_aid_recipients <- function(aiddata, skeleton) {
  recipients <- aiddata %>%
    distinct(recipient) %>%
    mutate(iso3 = countrycode(recipient, "country.name", "iso3c",
                              custom_match = c(`Korea, Democratic Republic of` = NA,
                                               `Netherlands Antilles` = NA,
                                               Kosovo = "XKK",
                                               `Serbia and Montenegro` = "SCG",
                                               Yugoslavia = "YUG"
                              ))) %>% 
    filter(iso3 %in% unique(skeleton$panel_skeleton$iso3)) %>%
    mutate(gwcode = countrycode(iso3, "iso3c", "gwn",
                                custom_match = c(XKK = 347,
                                                 YEM = 678)))
  
  return(recipients)
}

build_aid_purposes_manual <- function(dac_purposes_raw, out_file) {
  purpose_nodes <- read_xml(dac_purposes_raw) %>%
    xml_find_all("//codelist-item")
  
  purpose_codes <- tibble(
    code = purpose_nodes %>% xml_find_first(".//code") %>% xml_text(),
    category = purpose_nodes %>% xml_find_first(".//category") %>% xml_text(),
    # name = purpose_nodes %>% xml_find_first(".//name//narrative") %>% xml_text(),
    name = purpose_nodes %>% xml_find_first(".//name") %>% xml_text(),
    # description = purpose_nodes %>% xml_find_first(".//description//narrative") %>% xml_text()
    description = purpose_nodes %>% xml_find_first(".//description") %>% xml_text()
  )
  
  # Extract the general categories of aid purposes (i.e. the first three digits of the purpose codes)
  general_codes <- purpose_codes %>%
    filter(code %in% as.character(100:1000) & str_detect(name, "^\\d")) %>%
    mutate(code = as.integer(code)) %>%
    select(purpose_code_short = code, purpose_category_name = name) %>%
    mutate(purpose_category_clean = str_replace(purpose_category_name,
                                                "\\d\\.\\d ", "")) %>%
    separate(purpose_category_clean,
             into = c("purpose_sector", "purpose_category"), 
             sep = ", ") %>%
    mutate(across(c(purpose_sector, purpose_category), ~str_to_title(.))) %>%
    select(-purpose_category_name)
  
  # These 7 codes are weird and get filtered out inadvertently
  codes_not_in_oecd_list <- tribble(
    ~purpose_code_short, ~purpose_sector, ~purpose_category,
    100,                 "Social",        "Social Infrastructure",
    200,                 "Eco",           "Economic Infrastructure",
    300,                 "Prod",          "Production",
    310,                 "Prod",          "Agriculture",
    320,                 "Prod",          "Industry",
    420,                 "Multisector",   "Women in development",
    # NB: This actually is split between 92010 (domestic NGOs), 92020
    # (international NGOs), and 92030 (local and regional NGOs)
    920,                 "Non Sector",    "Support to NGOs"
  )
  
  purpose_codes_clean <- general_codes %>%
    bind_rows(codes_not_in_oecd_list) %>%
    arrange(purpose_code_short) %>%
    mutate(purpose_contentiousness = "")
  
  # Manually code contentiousness of purposes
  write_csv(purpose_codes_clean, out_file)
  return(out_file)
}

build_aid_purposes <- function(aiddata) {
  purposes <- aiddata %>%
    count(coalesced_purpose_name, coalesced_purpose_code)
  return(purposes)
}

build_aid_contentiousness <- function(path) {
  out <- read_csv(path, col_types = cols())
}

build_aiddata_final <- function(aiddata, donors, recipients, purpose_codes, skeleton, dac_eligible_raw) {
  aiddata_final <- aiddata %>%
    left_join(donors, by = "donor") %>%
    left_join(recipients, by = "recipient") %>%
    left_join(purpose_codes, by = "purpose_code_short") %>%
    mutate(donor_type_collapsed = ifelse(donor_type == "Country", "Country",
                                         "IGO, Multilateral, or Private")) %>%
    select(donor, donor_type, donor_type_collapsed,
           donor_gwcode, donor_iso3, year, gwcode, iso3,
           oda = commitment_amount_usd_constant_sum,
           purpose_code_short, purpose_sector, purpose_category,
           purpose_contentiousness,
           coalesced_purpose_code, coalesced_purpose_name) %>%
    arrange(gwcode, year)
  
  ever_dac_eligible <- read_csv(dac_eligible_raw, col_types = cols()) %>%
    # Ignore High Income Countries and More Advanced Developing Countries
    filter(!(dac_abbr %in% c("HIC", "ADC"))) %>%
    # Ignore countries that aren't in our skeleton panel
    filter(iso3 %in% skeleton$panel_skeleton$iso3) %>% 
    mutate(gwcode = countrycode(iso3, "iso3c", "gwn",
                                custom_match = c("YEM" = 678))) %>% 
    pull(gwcode) %>% unique()
  
  return(lst(aiddata_final, ever_dac_eligible))
}

build_donor_aiddata <- function(aiddata, skeleton) {
  donor_aidraw_data <- aiddata$aiddata_final %>% 
    filter(gwcode %in% unique(skeleton$panel_skeleton$gwcode)) %>%
    filter(year > 1980) %>%
    filter(oda > 0) %>%  # Only look at positive aid
    mutate(oda_log = log1p(oda))
  
  # Create fake country codes for non-country donors
  fake_codes <- donor_aidraw_data %>%
    distinct(donor, donor_type) %>%
    filter(donor_type != "Country") %>%
    arrange(donor_type) %>% select(-donor_type) %>%
    mutate(fake_donor_gwcode = 2001:(2000 + n()),
           fake_donor_iso3 = paste0("Z", str_sub(fake_donor_gwcode, 3)))
  
  donor_level_data <- donor_aidraw_data %>%
    left_join(fake_codes, by = "donor") %>%
    mutate(donor_gwcode = ifelse(is.na(donor_gwcode), 
                                 fake_donor_gwcode, 
                                 donor_gwcode),
           donor_iso3 = ifelse(is.na(donor_iso3), 
                               fake_donor_iso3, 
                               donor_iso3)) %>%
    select(-starts_with("fake"))
  
  return(donor_level_data)
}


# USAID stuff -------------------------------------------------------------

get_usaid <- function(usaid_url, out_dir) {
  usaid_name <- basename(usaid_url)  # filename only

  # Download data file
  usaid_get <- GET(usaid_url, 
                   write_disk(here_rel(out_dir, usaid_name),
                              overwrite = TRUE), 
                   progress())
  
  return(here_rel(out_dir, usaid_name))
}

clean_usaid <- function(path, skeleton) {
  usaid_raw <- read_csv(path, na = c("", "NA", "NULL"), col_types = cols())
  
  usaid_clean <- usaid_raw %>%
    filter(assistance_category_name == "Economic") %>%
    filter(transaction_type_name == "Obligations") %>%
    mutate(country_code = recode(country_code, `CS-KM` = "XKK")) %>%
    # Remove regions and World
    filter(!str_detect(country_name, "Region")) %>%
    filter(!(country_name %in% c("World"))) %>%
    # Ignore countries that aren't in our skeleton panel
    filter(country_code %in% skeleton$panel_skeleton$iso3) %>% 
    mutate(gwcode = countrycode(country_code, "iso3c", "gwn",
                                custom_match = c("YEM" = 678, "XKK" = 347))) %>%
    select(gwcode, year = fiscal_year, 
           implementing_agency_name, subagency_name, activity_name,
           channel_category_name, channel_subcategory_name, dac_sector_code,
           oda_us_current = current_amount, oda_us_2015 = constant_amount) %>%
    mutate(aid_deflator = oda_us_current / oda_us_2015 * 100) %>%
    mutate(channel_ngo_us = channel_subcategory_name == "NGO - United States",
           channel_ngo_int = channel_subcategory_name == "NGO - International",
           channel_ngo_dom = channel_subcategory_name == "NGO - Non United States")

  return(usaid_clean)
}

# USAID's conversion to constant 2015 dollars doesn't seem to take country
# differences into account—the deflator for each country in 2011 is essentially
# 96.65. When there are differences, it's because of floating point issues
# (like, if there are tiny grants of $3, there aren't enough decimal points to
# get the fraction to 96.65). So we just take the median value of the deflator
# for all countries and all grants and use that.
fix_inflation_usaid <- function(usaid, skeleton) {
  # Rescale the 2015 data to 2011 to match AidData
  #
  # Deflator = current aid / constant aid * 100
  # Current aid in year_t * (deflator in year_target / deflator in year_t)
  usaid_deflator_2011 <- usaid %>%
    filter(year == 2011) %>%
    summarise(deflator_target_year = median(aid_deflator, na.rm = TRUE)) %>%
    as.numeric()
  
  donor_level_data_usaid <- usaid %>%
    filter(gwcode %in% unique(skeleton$panel_skeleton$gwcode)) %>%
    filter(year > 1980) %>%
    filter(oda_us_current > 0) %>%
    mutate(oda_us_2011 = oda_us_current * (usaid_deflator_2011 / aid_deflator)) %>% 
    mutate(year = as.numeric(year))
  
  return(donor_level_data_usaid)
}

build_usaid_by_country_total <- function(usaid) {
  usaid_by_country_total <- usaid %>%
    group_by(gwcode, year) %>%
    summarise(oda_us = sum(oda_us_2011, na.rm = TRUE))
  
  return(usaid_by_country_total)
}

build_usaid_by_country_channel <- function(usaid) {
  usaid_by_country_channel <- usaid %>%
    pivot_longer(names_to = "key", values_to = "value", 
                 c(channel_ngo_us, channel_ngo_int, channel_ngo_dom)) %>%
    group_by(gwcode, year, key, value) %>%
    summarise(total_oda_us = sum(oda_us_2011, na.rm = TRUE)) %>%
    ungroup() %>%
    unite(channel, key, value) %>%
    filter(str_detect(channel, "TRUE")) %>%
    mutate(channel = str_replace(channel, "channel", "oda_us"),
           channel = str_replace(channel, "_TRUE", "")) %>%
    spread(channel, total_oda_us, fill = 0)
  
  return(usaid_by_country_channel)
}


# NGO restrictions --------------------------------------------------------

load_clean_dcjw <- function(path, regulations) {
  dcjw_orig <- read_excel(path) %>% 
    select(-c(contains("source"), contains("burden"), 
              contains("subset"), Coder, Date))
  
  dcjw_tidy <- dcjw_orig %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(names_to = "key", values_to = "value", -Country) %>% 
    separate(key, c("question", "var_name"), 4) %>%
    mutate(var_name = ifelse(var_name == "", "value", gsub("_", "", var_name))) %>%
    pivot_wider(names_from = "var_name", values_from = "value") %>% 
    # Remove underscore to match Chaudhry's stuff
    mutate(question = str_remove(question, "_")) %>% 
    mutate(value = as.numeric(value)) %>% 
    # Reverse values for q2c
    mutate(value = ifelse(question == "q2c", 1 - value, value)) %>% 
    # Rescale 2-point questions to 0-1 scale
    mutate(value = ifelse(question %in% c("q3e", "q3f", "q4a"),
                          rescale(value, to = c(0, 1), from = c(0, 2)),
                          value)) %>% 
    # q2d and q4c use -1 to indicate less restriction/burdensomeness. Since we're
    # concerned with an index of restriction, we make the negative values zero
    mutate(value = ifelse(question %in% c("q2d", "q4c") & value == -1,
                          0, value)) %>% 
    # Get rid of rows where year is missing and regulation was not imposed
    filter(!(is.na(year) & value == 0)) %>%
    # Some entries have multiple years; for now just use the first year
    mutate(year = str_split(year, ",")) %>% unnest(year) %>% 
    group_by(Country, question) %>% slice(1) %>% ungroup() %>%
    mutate(value = as.integer(value), year = as.integer(year)) %>% 
    mutate(Country = countrycode(Country, "country.name", "country.name"),
           gwcode = countrycode(Country, "country.name", "gwn",
                                custom_match = c("Yemen" = 678))) %>% 
    # If year is missing but some regulation exists, assume it has always already
    # existed (since 1950, arbitrarily)
    mutate(year = ifelse(is.na(year), 1950, year))
  
  potential_dcjw_panel <- dcjw_tidy %>%
    tidyr::expand(gwcode, question, 
                  year = min(.$year, na.rm = TRUE):2015)
  
  dcjw_clean <- dcjw_tidy %>%
    select(-Country) %>% 
    right_join(potential_dcjw_panel,
               by = c("gwcode", "question", "year")) %>% 
    arrange(gwcode, year) %>% 
    left_join(regulations, by = "question") %>% 
    filter(!ignore_in_index) %>% 
    group_by(gwcode) %>%
    mutate(all_missing = all(is.na(value))) %>%
    group_by(gwcode, question) %>% 
    # Bring most recent legislation forward in time
    fill(value) %>% 
    # For older NA legislation that can't be brought forward, set sensible
    # defaults. Leave countries that are 100% 0 as NA.
    mutate(value = ifelse(!all_missing & is.na(value), 0, value)) %>% 
    group_by(gwcode, year, barrier) %>%
    summarize(total = sum(value)) %>%
    ungroup() %>%
    pivot_wider(names_from = "barrier", values_from = "total") %>%
    filter(year > 1978) %>% 
    # Standardize barrier indexes by dividing by maximum number possible
    mutate(across(c(entry, funding, advocacy), 
                  list(std = ~ . / max(., na.rm = TRUE)))) %>% 
    mutate(barriers_total = advocacy + entry + funding,
           barriers_total_std = advocacy_std + entry_std + funding_std)
  
  return(dcjw_clean)
}

load_clean_chaudhry <- function(chaudhry_raw, regulations) {
  chaudhry_2014 <- expand_grid(gwcode = unique(chaudhry_raw$gwcode), 
                               year = 2014)
  
  chaudhry_long <- chaudhry_raw %>%
    # Bring in 2014 rows
    bind_rows(chaudhry_2014) %>%
    # Ethiopia and Czech Republic have duplicate rows in 1993 and 1994 respectively, 
    # but the values are identical, so just keep the first of the two
    group_by(gwcode, year) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(gwcode, year) %>%
    # Reverse values for q2c
    mutate(q2c = 1 - q2c) %>%
    # Rescale 2-point questions to 0-1 scale
    mutate_at(vars(q3e, q3f, q4a), ~rescale(., to = c(0, 1), from = c(0, 2))) %>%
    # q2d and q4c use -1 to indicate less restriction/burdensomeness. Since we're
    # concerned with an index of restriction, we make the negative values zero
    mutate_at(vars(q2d, q4c), ~ifelse(. == -1, 0, .)) %>%
    pivot_longer(cols = starts_with("q"), names_to = "question") %>%
    left_join(regulations, by = "question") %>%
    group_by(gwcode) %>%
    mutate(all_missing = all(is.na(value))) %>%
    group_by(gwcode, question) %>%
    # Bring most recent legislation forward in time
    fill(value) %>%
    # For older NA legislation that can't be brought forward, set sensible
    # defaults. Leave countries that are 100% 0 as NA.
    mutate(value = ifelse(!all_missing & is.na(value), 0, value)) %>%
    ungroup()
  
  chaudhry_registration <- chaudhry_long %>%
    select(gwcode, year, question_clean, value) %>%
    pivot_wider(names_from = "question_clean", values_from = "value")
  
  chaudhry_summed <- chaudhry_long %>%
    filter(!ignore_in_index) %>%
    group_by(gwcode, year, barrier) %>%
    summarize(total = sum(value)) %>%
    ungroup()
  
  chaudhry_clean <- chaudhry_summed %>%
    pivot_wider(names_from = barrier, values_from = total) %>%
    mutate_at(vars(entry, funding, advocacy),
              list(std = ~. / max(., na.rm = TRUE))) %>%
    mutate(barriers_total = advocacy + entry + funding,
           barriers_total_std = advocacy_std + entry_std + funding_std) %>%
    left_join(chaudhry_registration, by = c("gwcode", "year"))
  
  # In Suparna's clean data, due to post-Cold War chaos, Russia (365) is missing
  # for 1990-1991 and Serbia/Serbia and Montenegro/Yugoslavia (345) is missing
  # every thing pre-2006. DCJW don't include any data for Serbia, so we're out
  # of luck there—we're limited to Serbia itself and not past versions of it.
  # DCJW *do* include data for Russia, though, so we use that in our clean final
  # NGO laws data. Fortunately this is easy, since Russia's values are all 0 for
  # those two years. We just add two rows for Russia in 1990 and 1991 from DCJW
  early_russia <- tibble(gwcode = 365, year = c(1990, 1991),
                         advocacy = 0, entry = 0, funding = 0, 
                         entry_std = 0, funding_std = 0, advocacy_std = 0, 
                         barriers_total = 0, barriers_total_std = 0)
  
  chaudhry_clean <- chaudhry_clean %>% 
    bind_rows(early_russia) %>% 
    arrange(gwcode, year)
  
  return(chaudhry_clean)
}


# V-Dem -------------------------------------------------------------------

load_clean_vdem <- function(path) {
  vdem_raw <- read_rds(path) %>% as_tibble()
  
  vdem_clean <- vdem_raw %>%
    filter(year >= 1980) %>%
    select(country_name, year, cowcode = COWcode,
           
           # Civil society stuff
           v2cseeorgs,  # CSO entry and exit
           v2csreprss,  # CSO repression
           v2cscnsult,  # CSO consultation
           v2csprtcpt,  # CSO participatory environment
           v2csgender,  # CSO women's participation
           v2csantimv,  # CSO anti-system movements
           v2xcs_ccsi,  # Core civil society index (entry/exit, repression, participatory env)
           
           # Human rights and politics
           # Political corruption index (less to more, 0-1) (public sector +
           # executive + legislative + judicial corruption)
           v2x_corr,
           v2x_rule,  # Rule of law index
           # Rights indexes
           v2x_civlib,  # Civil liberties index
           v2x_clphy,  # Physical violence index
           v2x_clpriv,  # Private civil liberties index
           v2x_clpol,  # Political civil liberties index
           # Democracy
           e_polity2, v2x_polyarchy, v2x_regime_amb,
           # Economics and development
           v2peedueq,  # Educational equality
           v2pehealth,  # Health equality
           e_peinfmor  # Infant mortality rate
    ) %>%
    # Get rid of East Germany
    filter(cowcode != 265) %>%
    mutate(gwcode = countrycode(cowcode, origin = "cown", destination = "gwn",
                                custom_match = c("403" = 403L, "591" = 591L,
                                                 "679" = 678L, "935" = 935L,
                                                 "816" = 816L, "260" = 260L,
                                                 "315" = 316L))) %>%
    # Get rid of Hong Kong, Palestine (West Bank and Gaza), and Somaliland
    filter(!is.na(cowcode)) %>%
    select(-country_name, -cowcode)
  
  return(vdem_clean)
}

build_autocracies <- function(vdem, skeleton) {
  autocracies <- vdem %>% 
    group_by(gwcode) %>% 
    summarize(avg_row = mean(v2x_regime_amb, na.rm = TRUE)) %>% 
    ungroup() 
  
  autocracies_final <- skeleton$skeleton_lookup %>% 
    left_join(autocracies, by = "gwcode") %>% 
    mutate(autocracy = round(avg_row, 0) <= 4)
}


# WDI ---------------------------------------------------------------------

load_clean_wdi <- function(skeleton) {
  # World Bank World Development Indicators (WDI)
  # http://data.worldbank.org/data-catalog/world-development-indicators
  wdi_indicators <- c("NY.GDP.PCAP.PP.KD",  # GDP per capita, ppp (constant 2011 international $)
                      "NY.GDP.MKTP.PP.KD",  # GDP, ppp (constant 2010 international $)
                      "NE.TRD.GNFS.ZS",  # Trade (% of GDP)
                      "SP.POP.TOTL")     # Population, total
  
  wdi_raw <- WDI(country = "all", wdi_indicators, 
                 extra = TRUE, start = 1980, end = 2018)
  
  wdi_clean <- wdi_raw %>%
    filter(iso2c %in% unique(skeleton$panel_skeleton$iso2)) %>%
    mutate_at(vars(income, region), as.character) %>%  # Don't use factors
    mutate(gwcode = countrycode(iso2c, origin = "iso2c", destination = "gwn",
                                custom_match = c("YE" = 678L, "XK" = 347L, 
                                                 "VN" = 816L, "RS" = 345L))) %>% 
    mutate(region = ifelse(gwcode == 343, "Europe & Central Asia", region),
           income = ifelse(gwcode == 343, "Upper middle income", income)) %>% 
    select(country, gwcode, year, region, income, population = SP.POP.TOTL)
  
  return(wdi_clean)
}


# UN data -----------------------------------------------------------------

# Population
# Total Population - Both Sexes
# https://population.un.org/wpp/Download/Standard/Population/
load_clean_un_pop <- function(path, skeleton, wdi) {
  # The UN doesn't have population data for Kosovo, so we use WDI data for that
  kosovo_population <- wdi %>%
    select(gwcode, year, population) %>%
    filter(gwcode == 347, year >= 2008)
  
  un_pop_raw <- read_excel(path, skip = 16)
  
  un_pop <- un_pop_raw %>%
    filter((`Country code` %in% unique(skeleton$panel_skeleton$un))) %>%
    select(-c(Index, Variant, Notes, `Region, subregion, country or area *`,
              `Parent code`, Type),
           un_code = `Country code`) %>%
    pivot_longer(names_to = "year", values_to = "population", -un_code) %>%
    mutate(gwcode = countrycode(un_code, "un", "gwn",
                                custom_match = c("887" = 678, "704" = 816, "688" = 345))) %>%
    mutate(year = as.integer(year),
           population = as.numeric(population) * 1000) %>%  # Values are in 1000s
    select(gwcode, year, population) %>%
    bind_rows(kosovo_population)
  
  return(un_pop)
}

load_clean_un_gdp <- function(path_constant, path_current, skeleton) {
  # GDP by Type of Expenditure at constant (2015) prices - US dollars
  # http://data.un.org/Data.aspx?q=gdp&d=SNAAMA&f=grID%3a102%3bcurrID%3aUSD%3bpcFlag%3a0
  un_gdp_raw <- read_csv(path_constant, col_types = cols()) %>%
    rename(country = `Country or Area`) %>%
    mutate(value_type = "Constant")
  
  # GDP by Type of Expenditure at current prices - US dollars
  # http://data.un.org/Data.aspx?q=gdp&d=SNAAMA&f=grID%3a101%3bcurrID%3aUSD%3bpcFlag%3a0
  un_gdp_current_raw <- read_csv(path_current, col_types = cols()) %>%
    rename(country = `Country or Area`) %>%
    mutate(value_type = "Current")
  
  un_gdp <- bind_rows(un_gdp_raw, un_gdp_current_raw) %>%
    filter(Item %in% c("Gross Domestic Product (GDP)",
                       "Exports of goods and services",
                       "Imports of goods and services")) %>%
    filter(!(country %in% c("Former USSR", "Former Netherlands Antilles",
                            "Yemen: Former Democratic Yemen",
                            "United Republic of Tanzania: Zanzibar"))) %>%
    filter(!(country == "Yemen: Former Yemen Arab Republic" & Year >= 1989)) %>%
    filter(!(country == "Former Czechoslovakia" & Year >= 1990)) %>%
    filter(!(country == "Former Yugoslavia" & Year >= 1990)) %>%
    filter(!(country == "Former Ethiopia" & Year >= 1990)) %>%
    mutate(country = recode(country,
                            "Former Sudan" = "Sudan",
                            "Yemen: Former Yemen Arab Republic" = "Yemen",
                            "Former Czechoslovakia" = "Czechia",
                            "Former Yugoslavia" = "Serbia")) %>%
    mutate(iso3 = countrycode(country, "country.name", "iso3c",
                              custom_match = c("Kosovo" = "XKK"))) %>%
    left_join(select(skeleton$skeleton_lookup, iso3, gwcode), by = "iso3") %>%
    filter(!is.na(gwcode))
  
  un_gdp_wide <- un_gdp %>%
    select(gwcode, year = Year, Item, Value, value_type) %>%
    pivot_wider(names_from = c(value_type, Item), values_from = Value) %>%
    rename(exports_constant_2015 = `Constant_Exports of goods and services`,
           imports_constant_2015 = `Constant_Imports of goods and services`,
           gdp_constant_2015 = `Constant_Gross Domestic Product (GDP)`,
           exports_current = `Current_Exports of goods and services`,
           imports_current = `Current_Imports of goods and services`,
           gdp_current = `Current_Gross Domestic Product (GDP)`) %>%
    mutate(gdp_deflator = gdp_current / gdp_constant_2015 * 100) %>%
    mutate(un_trade_pct_gdp = (imports_current + exports_current) / gdp_current)
  
  # Rescale the 2015 data to 2011 to match AidData
  #
  # Deflator = current GDP / constant GDP * 100
  # Current GDP in year_t * (deflator in year_target / deflator in year_t)
  un_gdp_rescaled <- un_gdp_wide %>%
    left_join(select(filter(un_gdp_wide, year == 2011),
                     gwcode, deflator_target_year = gdp_deflator),
              by = "gwcode") %>%
    mutate(un_gdp_2011 = gdp_current * (deflator_target_year / gdp_deflator),
           un_trade_pct_gdp = (imports_current + exports_current) / gdp_current) 
  
  un_gdp_final <- un_gdp_rescaled %>%
    select(gwcode, year, un_trade_pct_gdp, un_gdp = un_gdp_2011)
  
  return(un_gdp_final)
}


# UCDP --------------------------------------------------------------------

load_clean_ucdp <- function(path) {
  ucdp_prio_raw <- read_csv(path, col_types = cols())
  
  ucdp_prio_clean <- ucdp_prio_raw %>%
    filter(type_of_conflict == 3) %>%
    mutate(gwcode_raw = str_split(gwno_a, pattern = ", ")) %>% 
    unnest(gwcode_raw) %>% 
    mutate(gwcode = as.integer(gwcode_raw)) %>% 
    group_by(gwcode, year) %>% 
    summarize(internal_conflict = n() > 0) %>% 
    ungroup()
  
  return(ucdp_prio_clean)
}


# EM-DAT disasters --------------------------------------------------------

load_clean_disasters <- function(path, skeleton) {
  disasters_raw <- read_excel(path, skip = 6)
  
  disasters <- disasters_raw %>%
    # Only look at countries in the main panel
    filter(ISO %in% unique(skeleton$panel_skeleton$iso3)) %>%
    filter(`Disaster Group` != "Complex Disasters") %>% 
    mutate(gwcode = countrycode(ISO, origin = "iso3c", destination = "gwn",
                                custom_match = c("YEM" = "678")),
           gwcode = as.numeric(gwcode)) %>% 
    select(country = Country, year = Year, iso3 = ISO, gwcode,
           type = `Disaster Type`, group = `Disaster Group`,
           subgroup = `Disaster Subgroup`,
           dis_deaths = `Total Deaths`, dis_injured = `No Injured`,
           dis_affected = `No Affected`, dis_homeless = `No Homeless`,
           dis_total_affected = `Total Affected`, dis_total_damage = `Total Damages ('000 US$)`)
  
  disasters_summarized <- disasters %>% 
    group_by(gwcode, year, group) %>% 
    summarize(across(starts_with("dis_"), ~sum(., na.rm = TRUE)),
              dis_count = n()) %>% 
    ungroup() %>% 
    filter(group == "Natural") %>% 
    pivot_longer(names_to = "name", values_to = "value", starts_with("dis_")) %>% 
    mutate(group = str_to_lower(group)) %>% 
    unite(name, group, name) %>% 
    pivot_wider(names_from = "name", values_from = "value") %>% 
    mutate(year = as.numeric(year)) %>% 
    filter(year > 1980)
  
  return(disasters_summarized)
}


# Combine,  clean,  and lag everything ------------------------------------

build_country_data <- function(skeleton, chaudhry_clean, vdem_clean,
                               ucdp_prio_clean, disasters_summarized,
                               aiddata, democracies, un_gdp, un_pop,
                               donor_level_data, usaid_by_country_total,
                               usaid_by_country_channel) {
  country_level_data <- skeleton$panel_skeleton %>% 
    mutate(ever_dac_eligible = gwcode %in% aiddata$ever_dac_eligible) %>% 
    filter(!(gwcode %in% democracies$gwcode)) %>% 
    left_join(un_gdp, by = c("gwcode", "year")) %>% 
    left_join(un_pop, by = c("gwcode", "year")) %>% 
    mutate(gdpcap = un_gdp / population,
           gdpcap_log = log(gdpcap),
           population_log = log(population)) %>% 
    left_join(chaudhry_clean, by = c("gwcode", "year")) %>% 
    # Indicator for Chaudhry data coverage
    # Chaudhry's Serbia data starts with 2006 and doesn't include pre-2006 stuff,
    # so we mark those as false. Also, Chaudhry starts in 1992 for Russia and 1993
    # for Czechia, so we mark those as false too
    mutate(laws = year %in% 1990:2014) %>% 
    mutate(laws = case_when(
      # Serbia, Czechia, and Russia
      gwcode == 345 & year <= 2005 ~ FALSE,
      gwcode == 316 & year <= 1992 ~ FALSE,
      gwcode == 365 & year <= 1991 ~ FALSE,
      TRUE ~ laws  # Otherwise, use FALSE
    )) %>% 
    left_join(vdem_clean, by = c("gwcode", "year")) %>%
    left_join(ucdp_prio_clean, by = c("gwcode", "year")) %>% 
    # Treat NAs in conflicts as FALSE
    mutate(internal_conflict = ifelse(is.na(internal_conflict),
                                      FALSE, internal_conflict)) %>%
    left_join(disasters_summarized, 
              by = c("gwcode", "year")) %>% 
    # NAs in disasters are really 0, especially when occurrence is 0
    mutate_at(vars(starts_with("natural_")), ~ifelse(is.na(.), 0, .)) %>%
    # Add indicator for post-Cold War, since all the former Soviet republics have
    # no GDP data before 1990
    mutate(post_1989 = year >= 1990)
  testthat::expect_equal(nrow(country_level_data), nrow(skeleton$panel_skeleton))
  
  # Combine country and donor data
  donor_country_data <- donor_level_data %>%
    left_join(select(country_level_data, -country, -iso3),
              by = c("year", "gwcode")) %>% 
    arrange(donor, year)
  testthat::expect_equal(nrow(donor_country_data), nrow(donor_level_data))
  
  # Calculate different versions of aid variables
  aid_by_country_total <- donor_country_data %>%
    group_by(gwcode, year) %>%
    summarise(total_oda = sum(oda, na.rm = TRUE)) %>% 
    ungroup()
  
  aid_by_country_purpose <- donor_country_data %>%
    group_by(gwcode, year, purpose_contentiousness) %>%
    summarise(total_oda = sum(oda, na.rm = TRUE)) %>%
    pivot_wider(names_from = "purpose_contentiousness", 
                values_from = "total_oda", values_fill = 0) %>% 
    rename(oda_contentious_high = High, 
           oda_contentious_low = Low) %>% 
    ungroup()
  
  country_aid <- country_level_data %>% 
    left_join(aid_by_country_total, by = c("year", "gwcode")) %>% 
    left_join(aid_by_country_purpose, by = c("year", "gwcode")) %>% 
    left_join(usaid_by_country_total, by = c("year", "gwcode")) %>% 
    left_join(usaid_by_country_channel, by = c("year", "gwcode")) %>% 
    mutate(across(contains("oda"), ~ifelse(is.na(.), 0, .)))
  
  testthat::expect_equal(nrow(country_aid), nrow(skeleton$panel_skeleton))
  
  return(country_aid)
}

fix_country_data <- function(country_aid) {
  # Infant mortality `e_peinfmor` is missing from Kosovo (2008–2014), and the
  # World Bank doesn't have data for it, but Eurostat does in their
  # `demo_minfind` indicator. Their data, however, is missing a couple years. To
  # fix this, we use linear interpolation to fill in 2013 and 2014
  kosovo_infant_mort <- tibble(
    year = 2007:2019,
    e_peinfmor = c(11.1, 9.7, 9.9, 8.8, 13.1, 11.4, 
                   NA, NA, 9.7, 8.5, 9.7, 10.6, 8.7)
  ) %>% 
    zoo::na.approx(.) %>% 
    as_tibble() %>% rename(e_peinfmor_interp = e_peinfmor) %>% 
    mutate(gwcode = 347)
  
  # `v2x_corr` is only missing data from Bahrain, which oddly has no data from
  # 1980–2004. Because corruption levels do not really change after 2005, we
  # impute the average corruption for the country in all previous years.
  
  # Find Bahrain's average corruption
  avg_corruption_bhr <- country_aid %>% 
    filter(iso3 == "BHR") %>% 
    summarize(avg_corr = mean(v2x_corr, na.rm = TRUE)) %>% 
    pull(avg_corr)
  
  # `v2x_polyarchy` is only missing in Mozambique from 1980–1993. To address
  # this, we calculate the average value of V-Dem's polyarchy index
  # (`v2x_polyarchy`) for each level of Polity (−8, −7, and −6 in the case of
  # Mozambique), and then use that corresponding average polyarchy
  
  # Find average polyarchy scores across different pre-1994 polity scores
  avg_polyarchy_polity <- country_aid %>% 
    filter(year < 1994) %>% 
    group_by(e_polity2) %>% 
    summarize(avg_polyarchy = mean(v2x_polyarchy, na.rm = TRUE),
              n = n())
  
  country_aid_complete <- country_aid %>% 
    # Get rid of pre-2006 Serbia stuff
    filter(!(gwcode == 345 & year < 2006)) %>% 
    # Fix Serbia name
    mutate(country = ifelse(gwcode == 345, "Serbia", country)) %>% 
    mutate(v2x_corr = ifelse(is.na(v2x_corr) & iso3 == "BHR", 
                             avg_corruption_bhr, v2x_corr)) %>% 
    mutate(imputed_corr = is.na(v2x_corr) & iso3 == "BHR") %>% 
    mutate(v2x_polyarchy = case_when(
      iso3 == "MOZ" & is.na(v2x_polyarchy) & e_polity2 == -6 ~ 
        filter(avg_polyarchy_polity, e_polity2 == -6)$avg_polyarchy,
      iso3 == "MOZ" & is.na(v2x_polyarchy) & e_polity2 == -7 ~ 
        filter(avg_polyarchy_polity, e_polity2 == -7)$avg_polyarchy,
      iso3 == "MOZ" & is.na(v2x_polyarchy) & e_polity2 == -8 ~ 
        filter(avg_polyarchy_polity, e_polity2 == -8)$avg_polyarchy,
      TRUE ~ v2x_polyarchy
    )) %>% 
    mutate(imputed_polyarchy = is.na(v2x_polyarchy) & iso3 == "MOZ") %>% 
    # Add Kosovo infant mortality
    left_join(kosovo_infant_mort, by = c("gwcode", "year")) %>% 
    mutate(e_peinfmor = coalesce(e_peinfmor, e_peinfmor_interp)) %>% 
    # Get rid of polity and RoW---we don't actually need them
    select(-e_polity2, -v2x_regime_amb, -e_peinfmor_interp)
  
  return(country_aid_complete)
}

make_final_data <- function(df) {
  # Determine if any of the values in the last k rows are TRUE
  check_last_k <- function(x, k) {
    # This creates a matrix with a column for each lag value (e.g. column 1 = lag
    # 0, column 2 = lag 1, etc.)
    all_lags <- sapply(0:k, FUN = function(k) lag(x, k))
    
    # Mark TRUE if any of the columns have TRUE in them
    any_true_in_window <- apply(all_lags, MARGIN = 1, FUN = any, na.rm = TRUE)
    return(any_true_in_window)
  }
  
  country_aid_final <- df %>% 
    # Proportion of contentious aid
    mutate(prop_contentious = oda_contentious_high / 
             (oda_contentious_low + oda_contentious_high),
           prop_contentious = 
             ifelse(oda_contentious_high == 0 & oda_contentious_low == 0, 
                    0, prop_contentious)) %>% 
    mutate(prop_contentious_logit = car::logit(prop_contentious, adjust = 0.001)) %>% 
    # Proportion of aid to NGOs
    mutate(prop_ngo_int = oda_us_ngo_int / oda_us,
           prop_ngo_us = oda_us_ngo_us / oda_us,
           prop_ngo_dom = oda_us_ngo_dom / oda_us,
           prop_ngo_foreign = (oda_us_ngo_int + oda_us_ngo_us) / oda_us) %>% 
    mutate(across(starts_with("prop_ngo"), ~ifelse(is.nan(.), 0, .))) %>% 
    mutate(across(starts_with("prop_ngo"), list(logit = ~car::logit(., adjust = 0.001)))) %>% 
    mutate(across(c(total_oda, oda_contentious_high, oda_contentious_low, oda_us),
                  list(log = ~log1p(.)))) %>% 
    group_by(gwcode) %>% 
    # Determine if there was conflict in the past 5 years
    mutate(internal_conflict_past_5 = check_last_k(internal_conflict, 5),
           natural_dis_past_5 = check_last_k(natural_dis_count >= 1, 5)) %>% 
    ungroup()
  
  return(country_aid_final)
}

lag_data <- function(df) {
  panel_lagged <- df %>% 
    # Lag/lead/diff things within countries
    group_by(gwcode) %>% 
    # Indicate changes in laws
    mutate(across(c(advocacy, entry, funding, barriers_total),
                  list(new = ~. - lag(.),
                       worse = ~(. - lag(.)) > 0,
                       cat = ~cut(. - lag(.),
                                  breaks = c(-Inf, -1, 0, Inf),
                                  labels = c("New better law", "No new laws",
                                             "New worse law"),
                                  ordered_result = TRUE)))) %>%
    # Lag and lead stuff
    # Treatment variables
    mutate(across(c(barriers_total, advocacy, entry, funding, 
                    barriers_total_new, advocacy_new, entry_new, funding_new,
                    v2xcs_ccsi, v2csreprss,
                    total_oda, total_oda_log, 
                    prop_contentious, prop_contentious_logit,
                    prop_ngo_dom, prop_ngo_foreign, 
                    prop_ngo_dom_logit, prop_ngo_foreign_logit),
                  list(lag1 = ~lag(., n = 1),
                       lag2 = ~lag(., n = 2)))) %>% 
    # Treatment history
    mutate(across(c(barriers_total_lag1, advocacy_lag1, entry_lag1, funding_lag1, 
                    barriers_total_new_lag1, advocacy_new_lag1, 
                    entry_new_lag1, funding_new_lag1,
                    v2xcs_ccsi_lag1, v2csreprss_lag1,
                    barriers_total_lag2, advocacy_lag2, entry_lag2, funding_lag2, 
                    barriers_total_new_lag2, advocacy_new_lag2, 
                    entry_new_lag2, funding_new_lag2,
                    v2xcs_ccsi_lag2, v2csreprss_lag2),
                  list(cumsum = ~cumsum_na(.)))) %>% 
    # Outcome variables
    mutate(across(c(total_oda, total_oda_log, 
                    prop_contentious, prop_contentious_logit,
                    prop_ngo_dom, prop_ngo_foreign,
                    prop_ngo_dom_logit, prop_ngo_foreign_logit),
                  list(lead1 = ~lead(., n = 1)))) %>% 
    ungroup()
  
  return(panel_lagged)
}

trim_data <- function(df) {
  df %>% filter(year >= 1990 & year < 2014)
}

winsorize_one <- function(df) {
  df <- df %>% 
    # Winsorize prop_contentious for the two cases that are exactly 1
    mutate(across(starts_with("prop_contentious"), list(orig = ~.))) %>% 
    mutate(across(c(starts_with("prop_contentious") & !contains("orig")), 
                  ~ifelse(. == 1, 0.999, .))) %>% 
    # Winsorize prop_ngo* for the few cases that are exactly 1
    mutate(across(starts_with("prop_ngo"), list(orig = ~.))) %>% 
    mutate(across(c(starts_with("prop_ngo") & !contains("orig")), 
                  ~ifelse(. == 1, 0.999, .)))
  
  return(df)
}


# World map ---------------------------------------------------------------

load_world_map <- function(path) {
  world_map <- read_sf(path) %>%
    filter(ISO_A3 != "ATA")
  
  return(world_map)
}


# Civicus Monitor ---------------------------------------------------------

# We downloaded the standalone embeddable widget
# (https://monitor.civicus.org/widgets/world/) as an HTML file with
# `wget https://monitor.civicus.org/widgets/world/` and saved it as index_2021-03-19.html
#
# We then extracted the COUNTRIES_DATA variable embedded in a <script> tag
# (xpath = /html/body/script[5]), which is JSON-ish, but not quite. jsonlite
# can't parse it for whatever reason, but some online JSON formatter and
# validator could, so we ran it through that and saved the resulting clean file
load_clean_civicus <- function(path) {
  civicus_raw <- read_json(path) %>% as_tibble() %>% slice(1)
  
  civicus_lookup <- tribble(
    ~value, ~category,
    1, "Closed",
    2, "Repressed",
    3, "Obstructed",
    4, "Narrowed",
    5, "Open"
  ) %>%
    mutate(category = fct_inorder(category, ordered = TRUE))
  
  civicus_clean <- civicus_raw %>%
    pivot_longer(everything(), names_to = "name", values_to = "value") %>%
    mutate(value = map_chr(value, ~.)) %>%
    mutate(value = parse_number(value, na = c("", "NA", "None"))) %>%
    mutate(country_name = countrycode(name, "iso3c", "country.name",
                                      custom_match = c("KOSOVO" = "XKK",
                                                       "SVT" = "VCT")),
           iso3c = countrycode(country_name, "country.name", "iso3c",
                               custom_match = c("XKK" = "Kosovo",
                                                "VCT" = "Saint Vincent and the Grenadines"))) %>%
    left_join(civicus_lookup, by = "value") %>%
    select(-name, -value, -country_name)
  
  return(civicus_clean)
}

create_civicus_map_data <- function(civicus, map) {
  map_with_civicus <- map %>%
    # Fix some Natural Earth ISO weirdness
    mutate(ISO_A3 = ifelse(ISO_A3 == "-99", as.character(ISO_A3_EH), as.character(ISO_A3))) %>%
    mutate(ISO_A3 = case_when(
      .$ISO_A3 == "GRL" ~ "DNK",
      .$NAME == "Norway" ~ "NOR",
      .$NAME == "Kosovo" ~ "XKK",
      TRUE ~ ISO_A3
    )) %>%
    left_join(civicus, by = c("ISO_A3" = "iso3c"))
  
  return(map_with_civicus)
}

