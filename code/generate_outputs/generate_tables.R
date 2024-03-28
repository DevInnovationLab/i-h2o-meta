library(tidyverse)
library(here)
library(baggr)

source(here("code/ma_models/fit_ma_frequentist.R"))
load(here("output/stan/bayesian-models-for-exhibits.Rdata"))
load(here("output/stan/bayesian-models-subsets.Rdata"))

# Table Summary Estimates (main results) ======================================

summary_df <- 
  rbind(
    oef(fma_re, pval = FALSE),
    oeb(bg_main, mlu = TRUE),
    oef(fma_subsets[["df_chlorination"]], pval = FALSE),
    oeb(bg_subsets[["df_chlorination"]], mlu = TRUE)
  ) %>% 
  round(2) %>% 
  as.data.frame() %>% 
  mutate(CI = interval2(lower, upper)) %>% 
  select(-lower, -upper) %>% 
  mutate(
    Interventions = c("All", "All", "Chlorination", "Chlorination"),
    Studies = c(n_all, n_all, n_chlorination, n_chlorination)
  ) %>% 
  as.matrix() %>% t()

colnames(summary_df)<-c("Mean Freq  OR", "Mean Bayesian OR", 
                        "Mean Freq OR", "Mean Bayesian OR")
rownames(summary_df)<-c("ITT effect on child mortality", "CI 95%", 
                        "Interventions", "Studies")
write.csv(summary_df, "output/tables/freq-bayes-summary-mortality.csv")

# Numbers of events (deaths) and non-events in treatment and control groups ====

# indiv mortality summary
mortality_all_summary <- 
  df_all_ma_adj %>%
  # Remove different versions of the same study
  dplyr::filter(
    !(trial %in% c(
      "Null et al., 2018 (W vs. active control)",
      "Null et al., 2018 (W vs. active + passive control) + Haushofer et al., 2020 (W vs. passive control)",
      "Kremer et. al., 2011 (Year 1 Treatment vs control + Year 2 Treatment)"
    ))
  ) %>%
  mutate(
    robustness = trial_name_short %in% c("du Preez et al., 2011", "Boisson et al., 2010")
  ) %>%
  arrange(
    robustness,
    trial_name_short
  ) %>%
  dplyr::select(
    trial_name_short,
    tcases,
    tnoncases,
    ccases,
    cnoncases
  ) 

write_csv(
  mortality_all_summary, 
  here(
    "output/tables/mortality_all_summary.csv" 
  )
)

#  Sensitivity of main results to dropping each study ==========================

table_loo_study <-
  left_join(
    loo_bayes %>%
      select(excluded_study, mean, CI_bayes, weights_bayes),
    loo_freq %>%
      mutate(CI_freq = interval2(lower, upper),
             weights_freq = weights_freq) %>%
      select(excluded_study, mean, CI_freq, weights_freq),
    by = "excluded_study"
  )

right <-
  df_main_ma_adj %>%
  select(c(trial_name)) %>%
  dplyr::rename(excluded_study = trial_name)

table_loo_study <- left_join(table_loo_study, right)

table_loo_study <-
  table_loo_study %>%
  rename(mean_bayes = mean.x,
         mean_freq = mean.y)

table_loo_study <-
  table_loo_study %>% mutate(
    mean_bayes = round(mean_bayes, 2),
    weights_bayes = round(weights_bayes, 2),
    mean_freq = round(mean_freq, 2),
    weights_freq = round(weights_freq, 2)
  ) %>%
  arrange(desc(weights_freq))

write_csv(table_loo_study, "output/tables/table-loo-study.csv")

# Additional sensitivity checks ================================================

additional_sa_table <- c()

row_list <- c(
  "df_tableS6_related",
  "df_tableS6_withcontamctrl_1",
  "df_tableS6_withcontamctrl_2",
  "df_tableS6_acctrl",
  "df_tableS6_springproc",
  "df_noWASH",
  "different_OR_def"
)

for (row in row_list) {
  additional_sa_table <- 
    rbind(
      additional_sa_table,
      as.vector(sub_estimates_freq[row, c("mean", "CI", "pval")]),
      as.vector(c(sub_estimates_bayes[row, c("mean", "CI")], NA))
    )
}

additional_sa_table[, 1] <-
  sapply(additional_sa_table[, 1], function(x)
    round(x, 2))

additional_sa_table[, 3] <-
  sapply(additional_sa_table[, 3], function(x)
    round(x, 3))


additional_sa_table <- t(additional_sa_table)
additional_sa_table <-
  data.frame(apply(additional_sa_table, 2, as.character))
colnames(additional_sa_table) <-
  c(
    "Mean Freq OR (1)",
    "Mean Bayesian OR (2)",
    "Mean Freq OR (3)",
    "Mean Bayesian OR (4)",
    "Mean Freq OR (5)",
    "Mean Bayesian OR (6)",
    "Mean Freq OR (7)",
    "Mean Bayesian OR (8)",
    "Mean Freq OR (9)",
    "Mean Bayesian OR (10)",
    "Mean Freq OR (11)",
    "Mean Bayesian OR (12)",
    "Mean Freq OR (13)",
    "Mean Bayesian OR (14)"
  )

rownames(additional_sa_table) <-
  c("ITT effect on child mortality", "CI 95%", "p-value")

write_csv(
  additional_sa_table,
  "output/tables/additional-sa-results.csv"
)
