library(tidyverse)
library(metafor)

load(here("data/final/ma_datasets.Rdata"))
set.seed(566535) # Extracted from random.org on 2024-04-22 03:43:50 UTC

# Fit models ===================================================================

# We fit frequentist meta-analysis (fma) in three models:
# RE, FE, and RE in subsets of data
fma_re <-
  rma(
    yi = tau,
    sei = se,
    data = df_main_ma_adj,
    method = "REML"
  )

fma_fe <-
  rma(
    yi = tau,
    sei = se,
    data = df_main_ma_adj,
    method = "FE"
  )


fma_subsets <-
  lapply(df_subsets_ma_adj,
         function(x) {
           if (nrow(x) < 3)
             #don't run meta-analysis model for 1 or 2 studies!
             return(NULL)
           rma(
             yi = tau,
             sei = se,
             data = x,
             method = "REML",
             control = list(stepadj = 0.5, maxiter = 1000)
           )
         })

# Prepare objects for printing out =============================================

overall_est_freq <- 
  oef(fma_re) %>% round(3)
sub_estimates_freq <- do.call(rbind, lapply(fma_subsets, oef)) %>%
  as.data.frame() %>%
  mutate(CI = interval2(lower, upper))

# Individual study estimates
ind_est_freq <-
  data.frame(
    mean = exp(fma_re$yi),
    lower = exp(fma_re$yi - 1.96 * sqrt(fma_re$vi)),
    upper = exp(fma_re$yi + 1.96 * sqrt(fma_re$vi))
  ) %>%
  mutate_all(round, 3) %>%
  mutate(
    trial_name = df_main_ma_adj$trial_name,
    intervention = df_main_ma_adj$intervention,
    year = df_main_ma_adj$year
  ) %>%
  mutate(CI = interval2(lower, upper)) %>%
  mutate(CI = ifelse(CI == "(NA,NA)", NA, CI)) %>%
  arrange(trial_name)

# Leave-one-study-out estimates
loo_freq <- list()
for (i in 1:nrow(df_main_ma_adj)) {
  df_iteration <- dplyr::filter(df_main_ma_adj,!row_number() %in% i)
  fit <-
    rma(
      yi = tau,
      sei = se,
      data = df_iteration,
      method = "REML"
    )
  loo_freq[[i]] <- oef(fit)
}
loo_freq <- do.call(rbind, loo_freq) %>% as.data.frame()
loo_freq$excluded_study <- df_main_ma_adj$trial_name

# Weights
weights_freq <- weights(fma_re)

# Filter subsets of studies and create helper objects -----
chlori_studies_freq <-
  dplyr::filter(ind_est_freq, intervention == "Chlorination")
filter_studies_freq <-
  dplyr::filter(ind_est_freq, intervention == "Filtration")
spring_studies_freq <-
  dplyr::filter(ind_est_freq, intervention == "Spring protection")
sodis_studies_freq  <- dplyr::filter(ind_est_freq, intervention == "SODIS")

n_chlorination <- nrow(chlori_studies_freq)
n_all <- nrow(df_main_ma_adj)


# Freq Odds ====================================================================

# Monitoring periods
period <- c(104, 78, 65, 52, 37, 20, 13, 9.5)

# List of mean effects, lower and upper
freq_estimates_duration <- matrix(NA, ncol = 5, nrow = length(period))
colnames(freq_estimates_duration) <-
  c("mean", "lower", "upper", "p-value", "Weeks")

# --- looping over subsets of data to get subset specific odds --- #
for (i in 1:8) {
  # Creating data frame with studies longer than i weeks
  df_duration <- dplyr::filter(df_main_ma_adj, weeks >= period[i])
  
  # meta-analysis model
  monitoring_freq <-
    metafor::rma(
      yi = tau,
      sei = se,
      data = df_duration,
      method = "REML"
    )
  
  # Peto RE
  freq_estimates_duration[i, 1] <-
    round(exp(monitoring_freq$beta[1, 1]), 3)
  freq_estimates_duration[i, 2] <- round(exp(monitoring_freq$ci.lb), 3)
  freq_estimates_duration[i, 3] <- round(exp(monitoring_freq$ci.ub), 3)
  # p- values
  freq_estimates_duration[i, 4] <- monitoring_freq$pval
  # weeks cut-off
  freq_estimates_duration[i, 5] <- period[i]
}

freq_estimates_duration <-
  as.data.frame(freq_estimates_duration)
