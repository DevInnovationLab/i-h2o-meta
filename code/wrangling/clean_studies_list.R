# Characteristics of included and excluded diarrhea studies

library(readxl)
library(tidyverse)
library(janitor)
library(here)

# Functions --------------------------------------------------------------------

#' Impute Standard Errors based on CIs
#' 
#' We're only given confidence intervals not SEs so need to impute the SE given 
#' the CI. However, since this is RR CIs are calculated like so (I think:)
#' 
#'  CI = exp(log(RR) +- Z_{1 - \alpha/2} se(log(RR)))
#' Therefore, we can only uncover the se of log(RR) since exp(se(log(RR))) \neq 
#' se(RR).
#' 
#' When we do this, we can use either the upper or lower CI - I do both and plot 
#' imputed SEs against each other. I'm not sure why they don't align exactly in 
#' Wolf et al - perhaps due to rounding issues?
#'
impute_se = function(ci_data, bound) {
  
  if (bound == "upper") {
    ln_upper = log(ci_data$upper_95_percent_confidence_interval)
    ln_point = log(ci_data$effect_estimate_on_diarrhea)
    se_ln_RR = (ln_upper - ln_point)/1.96
  }
  if (bound == "lower") {
    ci_data <- mutate(ci_data,
                      ifelse(lower_95_percent_confidence_interval == 0, 
                             0.01, 
                             lower_95_percent_confidence_interval))
    ln_lower = log(ci_data$lower_95_percent_confidence_interval)
    ln_point = log(ci_data$effect_estimate_on_diarrhea)
    se_ln_RR = (ln_lower - ln_point)/1.96*-1
  }
  return(se_ln_RR)
}


# Import data ------------------------------------------------------------------

df_studies <- 
  read_csv(
    here("data/raw/diarrhea_studies.csv"),
    locale = locale(encoding = "latin1")
  )

# Cleaning up ------------------------------------------------------------------

df_studies <-
  df_studies %>%
  clean_names %>%
  mutate(
    upper_95_percent_confidence_interval = 
      as.numeric(upper_95_percent_confidence_interval),
    compliance_rate =
      ifelse(
        compliance_rate %in% c("not reported", "Not reported", "NA"), 
        NA, 
        compliance_rate
      ) %>%
      str_remove_all("%") %>%
      as.numeric,
    included = ifelse(is.na(included), "Excluded", "Included"),
    included_dummy = ifelse(included == "Excluded", 0, 1),
    setting = ifelse(
      setting == "rural, urban, rural", 
      "mixed", 
      str_to_lower(setting)
    )
  )

# Grouping interventions -------------------------------------------------------

df_studies <-
  df_studies %>%
  mutate(
    intervention_group = case_when(
      str_detect(intervention, "chlorination") ~ "Chlorination",
      str_detect(intervention, "chlination") ~ "Chlorination",
      str_detect(intervention, "pasteurization") ~ "Pasteurization",
      str_detect(intervention, "safe storage") ~ "safe storage",
      str_detect(intervention, "filter") ~ "Filtration" ,
      str_detect(intervention, "filtration") ~ "Filtration" ,
      str_detect(intervention, "solar") ~ "Solar disinfection" ,
      str_detect(intervention, "piped") ~ "Piped water"  ,
      str_detect(intervention, "spring protection") ~ "Community improved water supply" 
    ),
    ref_first_word = str_extract(reference, "^\\w+"), 
    shorthand_ref = paste0(ref_first_word, "-", study_year),
    se_imp_upper = impute_se(., bound = "upper"),
    se_imp_lower = impute_se(., bound = "lower"),
    se_imp = (se_imp_upper + se_imp_lower)/2, 
    ln_RR = log(effect_estimate_on_diarrhea),
    chlor = str_detect(intervention, "chlor"),
    rural = setting == "rural",
    intervention_group = ifelse(is.na(intervention_group), "Spring protection", intervention_group),
    unimproved = baseline_water_water_in_control_group == "unimproved",
    chlorination = intervention_group  == "Chlorination",
    filtration = intervention_group == "Filtration",
    community = intervention_group == "Community improved water supply"
  ) %>%
  group_by(shorthand_ref) %>%
  mutate(shorthand_ref = paste0(shorthand_ref, "-", rank(shorthand_ref))) %>%
  ungroup() 

# Add income group -------------------------------------------------------------

income <-
  read_excel(
    here(
      "data/raw/weighted_mr/CLASS.xlsx"
    )
  ) %>%
  transmute(
    country = Economy,
    income_group = `Income group`
  )

df_studies <-
  df_studies %>%
  mutate(
    country = country %>%
      str_replace_all("Marocco", "Morocco") %>%
      str_replace_all("The Gambia", "Gambia, The") %>%
      str_replace_all("Egypt", "Egypt, Arab Rep.") %>%
      str_replace_all("Yemen", "Yemen, Rep.") %>%
      str_replace_all("Democratic Republic of the Kongo", "Congo, Dem. Rep.") %>%
      str_replace_all("Ivory Coast", "Côte d’Ivoire")
  ) %>%
  left_join(income)

# Are studies present in Wolf et al (2018)? -----------------------------------

wolf <-
  read_xlsx(
    here("data/raw/tmi13051-sup-0003-appendixs3.xlsx"),
    sheet = "water_studies", 
    skip = 2
  ) %>%
  mutate(
    reference =
      reference %>%
      str_replace_all("–", "-") %>%
      str_replace_all("’", "'")
  )

df_studies <-
  df_studies %>%
  mutate(in_wolf_et_al = reference %in% wolf$reference)

# Save data --------------------------------------------------------------------

df_studies %>%
  write_meta(
    path_data = "data/final/diarrhea_studies"
  )
