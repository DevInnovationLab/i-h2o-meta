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
  ) %>%
  dplyr::filter( 
    !is.na(ln_RR),
    !is.na(se_imp)
  ) %>% 
  mutate(
    z_I = abs((effect_estimate_on_diarrhea - mean(effect_estimate_on_diarrhea))/sd(effect_estimate_on_diarrhea)) > 1.96
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

logit_1 <- glm(
  mortality_reported ~ effect_estimate_on_diarrhea, 
  family = "binomial", 
  data = df_diarrhea
)

logit_2 <- glm(
  mortality_reported ~ z_I, 
  family = "binomial", 
  data = df_diarrhea
)
