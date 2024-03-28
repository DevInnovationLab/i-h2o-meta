library(here)
library(tidyverse)

df_summary_data <- 
  read_csv(here("data/raw/summary_data.csv")) %>%
  mutate(t_n = tcases + tnoncases,
         c_n = ccases + cnoncases)

# Select trials to be included -------------------------------------------------
# Trials to use are given in code/functions/settings.R
# filter everything else out

df_all_ma <- 
  df_summary_data %>%
  dplyr::filter(trial %in% c(trials_to_use, trials_for_aa))

df_main_ma <- 
  df_summary_data %>%
  dplyr::filter(trial %in% trials_to_use)

# Load additional datasets that support calculation of per-study ORs -----------

# Sizes of clusters
df_cluster <- 
  df_summary_data %>%
  dplyr::filter(
    trial %in% trials_to_use,
    cluster_rand == "yes"
  ) %>% 
  mutate(
    total_clusters = `n clust ctrl` + `n clust trt`,
    cluster_size = Obs/total_clusters
  ) %>% 
  rename(
    m_ctrl = `n clust ctrl`, 
    m_trt = `n clust trt`
  ) %>% 
  select(trial, total_clusters, cluster_size, m_ctrl, m_trt)

# Individual level data for c-RCTs (used to model TEs while 
# explicitly accounting for cluster effects)
df_ind_cluster <- 
  read_rds(here("data/final/individual_data_anonymised.rds")) %>% 
  select(study, cluster_id, death, wtreatment) %>% 
  dplyr::filter(!is.na(cluster_id))

# Save data --------------------------------------------------------------------

map(
  c(
    "df_all_ma",
    "df_main_ma",
    "df_cluster",
    "df_ind_cluster"
  ),
  ~ write_meta(
    get(.),
    meta_only = TRUE,
    path_data = paste0("data/transformed/", .)
  )
)

save(
  df_all_ma,
  df_main_ma,
  df_cluster,
  df_ind_cluster,
  file = "data/transformed/trials.Rdata"
)
