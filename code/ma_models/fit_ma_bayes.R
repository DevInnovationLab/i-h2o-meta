# Building on exploration of different meta-analyses in exploration/ folder
# fit Bayesian meta-analysis models

set.seed(24)

library(baggr)
library(rstanarm)
library(tidybayes)
library(here)

load(here("data/final/ma_datasets.Rdata"))


# Fit all of the Bayesian models -----------------------------------------------
bg_loo <-  loocv(
  df_main_ma_adj,
  group = "trial_name",
  model = "rubin",
  effect = "logOR",
  prior = def_priors,
  iter = 20000,
  chains = 6,
  control = list(adapt_delta = 0.99),
  return_models = TRUE
)

bg_loo_full <-  loocv(
  df_main_ma_adj,
  group = "trial_name",
  pooling = "full",
  model = "rubin",
  effect = "logOR",
  prior = def_priors,
  iter = 6000,
  chains = 6,
  return_models = TRUE
)

bg_none <-  baggr(
  df_main_ma_adj,
  group = "trial_name",
  pooling = "none",
  model = "rubin",
  effect = "logOR",
  prior = def_priors,
  iter = 4000,
  chains = 6
)

# Sensitivity checks (fitting models to subsets of studies)
bg_subsets <- lapply(df_subsets_ma_adj,
                     function(x) {
                       if (nrow(x) < 3)
                         #don't run meta-analysis model for 1 or 2 studies!
                         return(NULL)
                       baggr(
                         x,
                         group = "trial_name",
                         model = "rubin",
                         effect = "logOR",
                         prior = def_priors,
                         chains = 6,
                         iter = 10000,
                         control = list(adapt_delta = 0.99)
                       )
                     })


# Fit models with priors of increasing precision (centered at 3.9%) -----

# These are slightly tighter than in the main model,
# in future we will use same priors ewerywhere.
set_new_prior <- function(sd) {
  list(
    hypermean = baggr::normal(log(1 - .039), sd),
    hypersd = baggr::normal(0, 5),
    # baseline risk of mortality, set SD to reach .25 as upper limit
    control = baggr::normal(log(.01), (log(.25) - log(.01)) / 1.96),
    # set 1SD to 10-fold increase in mortality, that is a lot of variation
    control_sd = baggr::normal(0, 2.5)
  )
}

# Fitting models
hyper_sds_prior <- c(seq(.01, .09, .01),
                     seq(.1, .5, .05),
                     seq(.6, 1, .1))
model_list <- lapply(as.list(hyper_sds_prior), function(sd) {
  baggr(
    df_main_ma_adj,
    group = "trial_name",
    effect = "logOR",
    prior = set_new_prior(sd),
    chains = 6,
    iter = 10000,
    model = "rubin"
  )
})
bg_priors <- setNames(model_list, hyper_sds_prior)


save(bg_loo, bg_loo_full, bg_none, bg_subsets, bg_priors,
     file = here("output/stan/bayesian-ma-models.Rdata"))


# Meta-regression (Bayesian) models -----

df_main_ma_mr <- 
  df_main_ma_adj %>%
  mutate(
    randomisation_unit = ifelse(cluster_rand == 1, "Cluster", "Household"),
    chlorination      = ifelse(water_intervention == "Chlorination", "Chlorination", "Other"),
    filtration        = ifelse(water_intervention == "Filtration", "Filtration", "Other"),
    spring            = ifelse(
      water_intervention == "Spring protection",
      "Spring protection",
      "Other"
    )
  ) %>%
  mutate(diarrhea_effects = log(diarrhea_effects)) %>%
  select(
    trial,
    trial_name,
    randomisation_unit,
    chlorination,
    filtration,
    spring,
    year,
    compliance,
    diarrhea_effects,
    prevalence,
    water_intervention,
    tau,
    se
  )

mr_variables <- c(
  "randomisation_unit",
  "chlorination",
  "filtration",
  "spring",
  "year",
  "compliance",
  "diarrhea_effects",
  "prevalence",
  "water_intervention"
)
mr_fits <- lapply(as.list(mr_variables), function(var) {
  dfc <- df_main_ma_mr[!is.na(df_main_ma_mr[[var]]), ]
  baggr(
    dfc,
    group = "trial_name",
    model = "rubin",
    effect = "logOR",
    prior = list_modify(def_priors,
                        prior_beta = normal(0, 2.5)),
    chains = 6,
    iter = 6000,
    control = list(adapt_delta = 0.99),
    covariates = var
  )
})
names(mr_fits) <- mr_variables

save(mr_fits,
     file = here("output/stan/bayesian-mr-models.Rdata"))