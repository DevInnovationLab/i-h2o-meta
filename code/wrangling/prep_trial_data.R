library(here)
library(tidyverse)

df_summary_data <- 
  read_csv(here("data/raw/summary_data.csv")) %>%
  mutate(t_n = tcases + tnoncases,
         c_n = ccases + cnoncases)

df_studies <- 
  read_rds(here("data/final/diarrhea_studies.rds"))

## Under 5 mortality rate at time of the study ---------------------------------

all_data <- read_csv(here("data/raw/weighted_mr/all_data.csv"))

mortality <-
  all_data %>% 
  dplyr::filter(
    SEX == "Total",
    INDICATOR == "Under-five mortality rate"
  ) %>%
  transmute(
    country = REF_AREA %>% str_replace_all("Bolivia [(]Plurinational State of[)]", "Bolivia"),
    year = floor(REF_DATE),
    OBS_VALUE
  ) %>%
  group_by(country, year) %>%
  summarize(u5mr = mean(OBS_VALUE)/1000)

# Select trials to be included -------------------------------------------------
# Trials to use are given in code/functions/settings.R
# filter everything else out

df_all_ma <- 
  df_summary_data %>%
  dplyr::filter(trial %in% c(trials_to_use, trials_for_aa)) %>%
  left_join(
    df_studies %>%
      select(
        trial_name, 
        year = intervention_start, 
        compliance,
        country,
        setting
      )
  ) %>%
  left_join(
    mortality, 
    by = c("country", "year"),
    relationship = "many-to-one"
  )

assert_that(
  df_all_ma %>%
    dplyr::filter(is.na(u5mr), trial %in% trials_to_use) %>%
    nrow
  ==
    0
)

df_all_ma <-
  df_all_ma %>%
  mutate(
    across(
      c(year, country, setting, compliance),
      ~ case_when(
        trial_name == "Null et al., 2018 + Haushofer et al., 2020" ~ 
          df_all_ma %>% dplyr::filter(trial_name == "Null et al., 2018") %>% pull(.) %>% unique,
        TRUE ~ .
      )
    )
  ) %>%
  relocate(year, .before = weeks) %>%
  relocate(compliance, .after = takeup_control) %>%
  relocate(c(country, setting), .after = trial_name)

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
    cluster_size = obs/total_clusters
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
