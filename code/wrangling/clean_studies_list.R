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
  read_xlsx(
    "data/raw/diarrhea_studies.xlsx",
    sheet = "Sheet 1 - diarrhea_studies", 
    skip = 3
  )

# Cleaning up ------------------------------------------------------------------

df_studies <-
  df_studies %>%
  clean_names %>%
  mutate(
    upper_95_percent_confidence_interval = 
      as.numeric(upper_95_percent_confidence_interval),
    across(
      compliance_rate,
      ~ ifelse(
        . %in% c("not reported", "Not reported", "NA"), NA, .
        ) %>%
        as.numeric
    ),
    included = ifelse(is.na(included), "Excluded", "Included"),
    included_dummy = ifelse(included == "Excluded", 0, 1),
    setting = ifelse(setting == "rural, urban, rural", "mixed", setting)
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
    chlor = str_detect(intervention, "chlor")
  ) %>%
  group_by(shorthand_ref) %>%
  mutate(tmp_id = 1:n()) %>%
  ungroup() %>%
  # Remove study with point estimate outside its own CI
  mutate(shorthand_ref = paste0(shorthand_ref, "-", tmp_id)) %>%
  dplyr::filter(
    !str_detect(
      reference,
      ". H. Humphrey, M. N. N. Mbuya, R. Ntozini, L. H. Moulton," 
    )
  ) %>%
  select(-tmp_id) %>%
  dplyr::filter( 
    !is.na(ln_RR),
    !is.na(se_imp), 
    !is.infinite(se_imp)
  ) %>% 
  mutate(study_number = 1:n())

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

# Save data --------------------------------------------------------------------

df_studies %>%
  write_rds(
    "data/final/diarrhea_studies.rds"
  )
