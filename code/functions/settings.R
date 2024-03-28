library(baggr)
library(tidyverse)
library(readxl)
library(readr)

# Reduction in mortality from the "linear" model (see Discussion)
linear_model_reduction <- 0.039

# Select trials to analyse
trials_to_use <- c(
  "Luby et al., 2018 (W vs. control)",
  "Null et al., 2018 (W vs. active + passive control)",
  "Haushofer et al., 2020 (W vs. passive control)", 
  "Reller et al., 2003 (All 4 water treatments vs. control)", 
  "Boisson et al., 2013 (Chlor vs. control (placebo))", 
  "Peletz et al., 2012 (Filtration vs. control)", 
  "Kremer et. al., 2011 (Year 1 Treatment vs control)", 
  "Luby et al., 2006 (Chlor + Floc vs. control)", 
  "Semenza et al., 1998 (Chlor vs control (non-piped source))", 
  "Chiller et al., 2006 (Floc vs control)",
  "Crump et al., 2005 (Chlor + Floc vs. control)", 
  "Kirby et al., 2019 (W vs. control)",
  "Humphrey et al., 2019 (WASH vs. control)",
  "Dupas et al., 2021 (Coupons + Free Delivery vs control)",
  "Quick et al., 1999 (Chlor + safe storage + community education vs control)",
  "Conroy et al., 1999 (SODIS vs control)",
  "Mengistie et al., 2013 (Chlor vs control)",
  "Morris et al., 2018 (Filtration vs control)")

# Trials that will also be used for additional sensitivity analyses 
# (but not the main specification in the paper)
trials_for_aa <- c(
  "Kremer et. al., 2011 (Year 1 Treatment vs control + Year 2 Treatment)",
  "Null et al., 2018 (W vs. active control)",
  "Boisson et al., 2010 (Filtration vs. control (w/placebo))",
  "du Preez et al., 2011 (SODIS vs. control)",
  "Null et al., 2018 (W vs. active + passive control) + Haushofer et al., 2020 (W vs. passive control)"
  
)

# Bayes settings:
default_stan_model_to_use <- "may2023" #for stan_path()


bsl_pool <- "partial" #choose partial or none for alternative specification
#partial is preferable, but none could be good for debug

# These are slightly tighter than in the main model, 
# in future we will use same priors everywhere.
def_priors <- list(
  hypermean = baggr::normal(0, 5), #prevent masking by rstanarm
  hypersd = baggr::normal(0, 5),
  beta = baggr::normal(0, 2.5),
  # baseline risk of mortality, set SD to reach .25 as upper limit
  control = baggr::normal(log(.01), (log(.25) - log(.01))/1.96),
  # set 1SD to 10-fold increase in mortality, that is a lot of variation
  control_sd = baggr::normal(0, 2.5)
)
