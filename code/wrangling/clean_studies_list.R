# Characteristics of included and excluded diarrhea studies

library(readxl)
library(tidyverse)
library(janitor)
library(here)
library(assertthat)

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
    
    # If lower bound is exactly zero, change it to 0.01 so we can take the log
    ci_data <- 
      ci_data %>%
      mutate(
        lower_95_percent_confidence_interval = if_else(
          lower_95_percent_confidence_interval == 0, 
          0.01, 
          lower_95_percent_confidence_interval)
      )
    
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
    compliance =
      ifelse(
        compliance %in% c("not reported", "Not reported", "NA"), 
        NA, 
        compliance
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
  )

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


income_hist <-
  read_excel(
    here(
      "data/raw/weighted_mr/OGHIST.xlsx"
    ),
    sheet = "Country Analytical History",
    range = "B12:AM229",
    na = c("",".."),
    col_names = c("country", 1987:2023)
  ) %>%
  pivot_longer(
    cols = !country,
    names_to = "cal_year",
    values_to = "group"
  ) %>%
  mutate(
    fy_start = as.numeric(cal_year) + 1,
    income_group_hist = case_when(
      group == "L" ~ "Low income",
      group == "LM" ~ "Lower middle income",
      group == "UM" ~ "Upper middle income",
      group == "H" ~ "High income"
    ),
    country = country %>%
      # different apostrophe in historical data
      str_replace_all("Côte d'Ivoire", "Côte d’Ivoire") %>%
      str_replace_all("Viet Nam", "Vietnam")
  ) %>%
  select(country, fy_start, income_group_hist)

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
  left_join(
    income_hist,
    by = join_by(
      country == country,
      intervention_start == fy_start
    ),
    keep = FALSE
  ) %>%
  left_join(income)

assert_that(
  df_studies %>% 
    dplyr::filter(is.na(income_group_hist), included == "Included") %>%
    nrow
  == 0
)

assert_that(
  df_studies %>% 
    dplyr::filter(is.na(income_group), included == "Included") %>%
    nrow
  == 0
)

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
