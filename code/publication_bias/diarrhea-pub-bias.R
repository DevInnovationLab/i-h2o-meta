library(tidyverse)
library(readxl)
library(ggstance)
library(metafor)
library(meta)
library(janitor)

# Eggers tests -----------------------------------------------------------------

df_diarrhea <-
  read_rds(
    here(
      "data/final/diarrhea_studies.rds"
    )
  )

df_diarrhea_chlor = 
  df_diarrhea %>%
  dplyr::filter(str_detect(intervention, "chlor"))

## Now some meta-analysis stuff
clean_fit = metagen(
  TE = ln_RR,
  seTE = se_imp,
  data = df_diarrhea, 
  sm = "RR"
)

chlorine_fit = metagen(
  TE = ln_RR,
  seTE = se_imp,
  data = df_diarrhea_chlor,
  sm = "RR"
)

bias_clean_fit = metabias(clean_fit)
bias_chlorine_fit = metabias(chlorine_fit)

# Mortality reporting vs. diarrhea prevalence, z-values ------------------------

mr_rep_df <- 
  read_xlsx(
    here("data/raw/diarrhea_studies.xlsx"), 
    sheet = 1, 
    skip = 3
  ) %>% 
  mutate(
    z_I = ifelse(`upper 95% confidence interval` <= 1, 1, 0)
  )

logit_1 <- glm(
  `Mortality reported` ~ `effect estimate (on diarrhea)`, 
  family = "binomial", 
  data = mr_rep_df
)

logit_2 <- glm(
  `Mortality reported` ~ z_I, 
  family = "binomial", 
  data = mr_rep_df
)
