set.seed(24)
library(baggr)
library(rstanarm)
library(tidyverse)
library(tidybayes)

load("data/transformed/trials.Rdata")
load("output/stan/cluster_bayes_models.Rdata")

# Derivation of analysis dataset with clustering adjustments ===================

dt_or_rstanarm <- cluster_bayes_models$rstanarm_summary_018 %>%
  mutate(res = map(glm, spread_draws, treatment)) %>%
  select(-data, -glm) %>% unnest(res) %>%
  summarise(tau = mean(treatment), se = sd(treatment)) %>%
  rename(trial = group)

dt_or_stan <- cluster_bayes_models$rstanarm_ind_stan %>% 
  mutate(te = map_vec(model_re, rstan::extract, "trt_effect")) %>% 
  mutate(sigma = map_vec(model_re, rstan::extract, "cluster_sigma")) %>% 
  select(-data, -model_re) %>% 
  unnest(c(te, sigma)) %>% 
  # following https://lnalborczyk.github.io/post/icc/
  # mutate(icc = icc_f) %>% 
  group_by(study) %>% 
  summarise(tau = mean(te), se = sd(te), 
            sigma = mean(sigma)) %>% 
# icc_mean = mean(icc), icc_se = sd(icc)) %>% 
  rename(trial_name = study) %>%  
  left_join(select(df_main_ma, trial, trial_name), by = "trial_name") %>% 
  select(trial, tau, se)

dt_or_abcd0018   <- df_all_ma %>% adjust_for_icc(0.018) %>% 
  or_calculator(0.25)   %>% select(group, tau, se) %>% 
# Calculate using crude ORs ====================================================
  rename(trial = group)


# Combine preferred OR estimates with original data ============================

# We will combine ORs as follows: for cluster-randomised trials with
# individual-level data we use the Stan models.
# For everything else we will use a Bayesian logit model.
# See fit-cluster-models.R for fitting.
# See explore-cluster-candidates.R for comparison of different estimates.

df_all_ma_adj <- df_all_ma %>%
  left_join(
    rbind(
      dt_or_stan %>% ungroup() %>%
        select(trial, tau, se),
      dt_or_rstanarm %>%
        select(trial, tau, se) %>%
        dplyr::filter(!(trial %in% dt_or_stan$trial))
    ),
    by = "trial"
  )

df_main_ma_adj <- df_all_ma_adj %>%
  dplyr::filter(trial %in% trials_to_use)



# We also want to redo the main specification with continuity corrections =====

# We will combine ORs as follows: for cluster-randomised trials with
# individual-level data we use the Stan models.
# For everything else we will use a 0.25 correction and assume ICC of 1.8% in
# c-RCTs, then calculate ORs and their SE on log scale using typical formulas.

df_main_ma_abcd <- df_all_ma %>%
  left_join(
    rbind(
      dt_or_stan %>% ungroup() %>%
        select(trial, tau, se),
      dt_or_abcd0018 %>%
        select(trial, tau, se) %>%
        dplyr::filter(!(trial %in% dt_or_stan$trial))
    ),
    by = "trial"
  ) %>%
  dplyr::filter(trial %in% trials_to_use)


# A list of subsets of data (sensitivity analyses with different inputs) =======

df_subsets_ma_adj <- list(
  # (1)
  df_chlorination = df_main_ma_adj %>%
    dplyr::filter(water_intervention %in% "Chlorination") ,
  # (2)
  df_filtration = df_main_ma_adj %>%
    dplyr::filter(water_intervention %in% "Filtration"),
  # (3)
  df_spring = df_main_ma_adj %>%
    dplyr::filter(water_intervention %in% "Spring protection"),
  # (4)
  df_repChildMA = df_main_ma_adj %>%
    dplyr::filter(
      trial_name %in% c(
        "Luby et al., 2018",
        "Null et al., 2018",
        "Peletz et al., 2012",
        "Luby et al., 2006",
        "Crump et al., 2005",
        "Conroy et al., 1999",
        "Mengistie et al., 2013",
        "Morris et al., 2018"
      )
    ),
  # (5) Combining Null et al and Haushofer et al studies
  df_tableS6_related = df_all_ma_adj %>%
    dplyr::filter(
      trial %in% c(
        trials_to_use,
        "Null et al., 2018 (W vs. active + passive control) + Haushofer et al., 2020 (W vs. passive control)"
      )
    ) %>%
    dplyr::filter(
      !trial %in% c(
        "Null et al., 2018 (W vs. active + passive control)",
        "Haushofer et al., 2020 (W vs. passive control)"
      )
    ),
  
  # (6) Adding du Preez et al., 2011
  df_tableS6_withcontamctrl_1 = df_all_ma_adj %>%
    dplyr::filter(
      trial %in% c(trials_to_use,
                   "du Preez et al., 2011 (SODIS vs. control)")
    ),
  
  # (7) Adding Boisson et al. 2010
  df_tableS6_withcontamctrl_2 = df_all_ma_adj %>%
    dplyr::filter(
      trial %in% c(
        trials_to_use,
        "Boisson et al., 2010 (Filtration vs. control (w/placebo))"
      )
    ),
  
  # (9) Adding Null et al with only active control group
  df_tableS6_acctrl = df_all_ma_adj %>%
    dplyr::filter(
      trial %in% c(trials_to_use,
                   "Null et al., 2018 (W vs. active control)")
    ) %>%
    dplyr::filter(!trial %in% "Null et al., 2018 (W vs. active + passive control)"),
  
  # (9) Adding Kremer et al with spring protection in year 1 and 2
  df_tableS6_springproc = df_all_ma_adj %>%
    dplyr::filter(
      trial %in% c(
        trials_to_use,
        "Kremer et. al., 2011 (Year 1 Treatment vs control + Year 2 Treatment)"
      )
    ) %>%
    dplyr::filter(!trial %in% "Kremer et. al., 2011 (Year 1 Treatment vs control)"),
  
  #(10) Dropping studies that include hand washing interventions/ cookstoves
  df_noWASH = df_main_ma_adj %>%
    dplyr::filter(
      !trial %in% c(
        "Humphrey et al., 2019 (WASH vs. control)",
        "Kirby et al., 2019 (W vs. control)"
      )
    ),
  
  # Continuity corrections (see previous section for explanation)
  different_OR_def = df_main_ma_abcd,
  # (11) studies that report mortality outcomes
  df_report_mortality = df_main_ma_adj %>%
    dplyr::filter(
      !str_detect(mortality_reporting, "Not reported")
    )
)

# Save outputs =================================================================

map(
  c(
    "df_main_ma_adj",
    "df_all_ma_adj"
  ),
  ~ write_meta(
    get(.),
    meta_only = TRUE,
    path_data = paste0("data/transformed/", .)
  )
)

save(df_main_ma_adj,
     df_all_ma_adj,
     df_subsets_ma_adj,
     file = "data/final/ma_datasets.Rdata")
