# Short script for loading Bayesian meta-analysis models
# (from fit_ma_bayes.R)
# and preparing them for plotting, tables etc etc
# Author: Witold Wiecek
# July 2023

library(tidyverse)
library(baggr)
library(here)
set.seed(1990)

# Prepare Bayesian models -----

load(here("output/stan/bayesian-ma-models.Rdata"))

bg_main <- bg_loo$full_model
bg_main_full <- bg_loo_full$full_model
bg_main_or_ppd <- effect_draw(bg_main, transform=exp)

load(here("output/stan/bayesian-mr-models.Rdata"))
load(here("data/final/ma_datasets.Rdata"))

# Compile their results using this function:
ctdf_helper <- function(x) {
  left_join(
    select(df_main_ma_adj, trial_name, intervention),
    #this is the only important line here:
    group_effects(x, s = T, transform = exp)[, , 1] %>%
      as.data.frame() %>%
      rownames_to_column() %>% rename(trial_name = rowname),
    by = "trial_name"
  ) %>%
    transmute(
      trial_name,
      intervention,
      lower = lci,
      mean = mean,
      upper = uci
    ) %>%
    mutate(CI = interval2(lower, upper)) %>%
    arrange(trial_name)
}


# Store results of Bayesian models in various handy objects -----

ind_est_bayes <- ctdf_helper(bg_main)
# ind_est_bayes_nopool <- ctdf_helper(bg_none)
overall_est_bayes <- oeb(bg_main)

sub_estimates_bayes <- do.call(rbind, lapply(bg_subsets, oeb)) %>%
  as.data.frame() %>%
  mutate(CI = interval2(lower, upper)) %>%
  mutate(pval = NA)

weights_bayes <- 100 * weights(bg_main)[2, , 1]
names(weights_bayes) <- bg_main$data$trial_name

loo_bayes <- data.frame(
  excluded_study = attr(bg_loo$full_model$inputs, "group_label"),
  do.call(rbind, lapply(bg_loo$models, oeb)) %>%
    as.data.frame()
) %>%
  mutate(CI_bayes = interval2(lower, upper)) %>%
  mutate(weights_bayes = weights_bayes)


# Calculate weighted prevalence -----

summarise_prevalence_compliance <- 
  df_main_ma_adj %>%
  select(
    trial_name,
    prevalence,
    compliance,
    takeup_control,
    year,
    Obs,
    weeks,
    intervention
  ) %>%
  mutate(
    person_wks = Obs * weeks,
    weights1 = weights_bayes / 100, # THIS IS SENSITIVE TO ORDERING OF ROWS, REMEMBER
    weights2 = person_wks / sum(person_wks)
  ) %>%
  arrange(trial_name) %>%
  mutate(
    compliance = 
      ifelse(
        is.na(compliance), 
        mean(compliance, na.rm = T), 
        compliance
      ),
    takeup_control = 
      ifelse(
        is.na(takeup_control), 
        mean(takeup_control, na.rm = T), 
        takeup_control
      ),
    prevalence = 
      ifelse(
        is.na(prevalence), 
        mean(prevalence, na.rm = T), 
        prevalence
      ),
    chlor = 1 * (intervention == "Chlorination")
  ) %>%
  mutate(
    prevalence1 = prevalence * weights1,
    prevalence2 = prevalence * weights2,
    year1 = year * weights1,
    compliance1 = compliance * weights1,
    compliance2 = compliance * weights2,
    compliance_chlor1 = compliance * weights1 * chlor / sum(weights1 * chlor),
    tkup_ctrl1  = takeup_control * weights1,
    tkup_ctrl2  = takeup_control * weights2
  ) %>%
  select(
    prevalence1,
    prevalence2,
    compliance1,
    compliance2,
    compliance_chlor1,
    tkup_ctrl1,
    tkup_ctrl2
  ) %>%
  summarise(
    across(
      where(is.numeric),
      ~ sum(.)
    )
  )

# Filter subsets of studies and create helper objects -----

chlori_studies_bayes <-
  dplyr::filter(ind_est_bayes, intervention == "Chlorination")
filter_studies_bayes <-
  dplyr::filter(ind_est_bayes, intervention == "Filtration")
spring_studies_bayes <-
  dplyr::filter(ind_est_bayes, intervention == "Spring protection")
sodis_studies_bayes <-
  dplyr::filter(ind_est_bayes, intervention == "SODIS")

save(
  loo_bayes,
  bg_main,
  bg_main_or_ppd,
  bg_main_full,
  overall_est_bayes,
  ind_est_bayes,
  sub_estimates_bayes,
  weights_bayes,
  chlori_studies_bayes,
  filter_studies_bayes,
  spring_studies_bayes,
  sodis_studies_bayes,
  summarise_prevalence_compliance,
  file = "output/stan/bayesian-models-for-exhibits.Rdata"
)

save(
  bg_subsets,
  file = "output/stan/bayesian-models-subsets.Rdata"
)
