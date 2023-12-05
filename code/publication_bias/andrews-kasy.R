# Script for generating publication bias tests (Andrews & Kasy) 
# Author: Luiza Andrade
# July 2023

library(here)
library(tidyverse)

# Functions from https://github.com/maxkasy/MetaStudiesApp
# Commit hash: f5fcd16b8e81bf0f0919046d57ade95d1d0ff428
source(here("code/publication_bias", "RobustVariance.R"))
source(here("code/publication_bias", "metastudiesfunctions.R"))

# Load data ----------------------------

# Mortality
load(here("data/final/ma_datasets.Rdata"))

# Diarrhea outcome
df <- read_rds(here("data/final/diarrhea_studies.rds"))

# This is in a separate script as it requires some data processing
source(here("code/publication_bias/diarrhea-pub-bias.R"))

# Estimates ----------------------------

mortality <-
  metastudies_estimation(
    df_main_ma_adj$tau, 
    df_main_ma_adj$se, 
    cutoffs = 1.96, 
    symmetric = TRUE, 
    model = "normal"
  ) %>%
  bind_cols() %>%
  t()

diarrhea <-
  metastudies_estimation(
    df_diarrhea$ln_RR, 
    df_diarrhea$se_imp, 
    cutoffs = 1.96, 
    symmetric = TRUE, 
    model = "normal"
  ) %>%
  bind_cols() %>%
  t()

chlorine <-
  metastudies_estimation(
    df_diarrhea_chlor$ln_RR, 
    df_diarrhea_chlor$se_imp, 
    cutoffs = 1.96, 
    symmetric = TRUE, 
    model = "normal"
  ) %>%
  bind_cols() %>%
  t()

# Prepare table ------------------------------
pub_bias <-
  rbind(
    mortality,
    diarrhea,
    chlorine
  ) %>%
  as.data.frame() %>%
  setNames(c("mu", "tau", "beta_p"))

rownames(pub_bias) <-
  paste(
    map(
      c("mortality", "diarrhea", "chlorine"), 
      ~ rep(., 2)
    ) %>% unlist,
    rep(
      c("pe", "se"),
      3
    ),
    sep = "_"
  )
