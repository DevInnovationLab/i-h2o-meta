# This is the script where various models with adjustments for clustering
# are fitted using Bayesian inference
# This is then used as inputs into meta-analyses

set.seed(244)
library(baggr)
library(rstanarm)
library(rstan)
library(tidyverse)
library(here)

options(mc.cores = 6)
rstan_options(auto_write = TRUE)

load(here("data/transformed/trials.Rdata"))


# Per-study ORs with individual-level data and cluster effects -----

model_re <- function(dt) {
  dt$cluster_id <- factor(dt$cluster_id)
  stan_glmer(
    death ~ wtreatment + (1 | cluster_id),
    data = dt,
    prior_intercept = rstanarm::normal(log(0.01), 10),
    prior = rstanarm::normal(0, 5),
    iter = 5000,
    cores = 6,
    family = binomial(link = "logit")
  )
}


# An alternative model that is coded by hand -----
sm_ncp <- stan_model(here("code/ma_models/logit_model.stan"))
model_stan <- function(dt) {
  dt$cluster_id <- as.numeric(factor(dt$cluster_id))
  sampling(
    sm_ncp,
    data = list(
      N = nrow(dt),
      K = max(dt$cluster_id),
      death = dt$death,
      cluster_id = dt$cluster_id,
      treatment = dt$wtreatment
    ),
    iter = 5000,
    cores = 6,
    control = list(adapt_delta = 0.99)
  )
}

res_stan <- df_ind_cluster %>%
  group_by(study) %>%
  nest() %>%
  mutate(model_re = map(data, model_stan))


fit_rstanarm_summary <- function(icc) {
  df_all_ma %>%
    adjust_for_icc(icc) %>%
    # Just a shorthand to rename columns:
    or_calculator(add = 0) %>%
    select(-tau,-se) %>%
    mutate_if(is.numeric, round) %>%
    binary_to_individual() %>% group_by(group) %>% nest() %>%
    mutate(
      glm = 
        map(
          data,
          function(x)
            stan_glm(
              outcome ~ treatment,
              data = x,
              prior_intercept = rstanarm::normal(log(0.01), 10),
              prior = rstanarm::normal(0, 5),
              iter = 5000,
              cores = 6,
              family = binomial(link = "logit")
            )
        )
    )
}

# WW: I fit ICC of 0.018 based on exploration done in se_ratio script

res_rstanarm_018  <- fit_rstanarm_summary(0.018)
res_rstanarm_0   <- fit_rstanarm_summary(0)

cluster_bayes_models <- list(
  "rstanarm_summary_0"   = res_rstanarm_0,
  "rstanarm_summary_018"  = res_rstanarm_018,
  "rstanarm_ind_stan"     = res_stan
)

save(cluster_bayes_models,
     file = here("output/stan/cluster_bayes_models.Rdata"))
