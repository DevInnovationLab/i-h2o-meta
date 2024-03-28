# Characteristics of included and excluded diarrhea studies

library(here)
library(tidyverse)

# Import data ------------------------------------------------------------------

df_studies <-
  read_rds(
    here("data/final/diarrhea_studies.rds")
   )

# Table of p-values for t-tests 
df_studies_1 <-
  df_studies %>%
  dplyr::filter(!is.na(intervention))
  
df_compliance <- 
  df_studies_1 %>%
  dplyr::filter(!is.na(compliance_rate))

reg <- list()

reg[[1]]<-lm(effect_estimate_on_diarrhea ~ included_dummy, data = df_studies_1)
reg[[2]]<-lm(compliance_rate ~ included_dummy, data = df_compliance)
reg[[3]]<-lm(rural ~ included_dummy, data = df_studies_1)
reg[[4]]<-lm(unimproved ~ included_dummy, data = df_studies_1)

names(reg) <- c("effect on diarrhea", "compliance", "rural", "unimproved")






