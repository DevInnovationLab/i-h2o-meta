#==============================================================================#
#
# This code calculates a weighted average mortality rate for children under 5 
# in low-income and lower middle-income countries as per the World Bank.
#
# Author: Andreas Zeniou
#
#------------------------------------------------------------------------------#
#
# The general outline of the calculation is as follows:
# 1. compute the population, in each LIC/LMIC, without access to piped water.
#    Such individuals fall into three categories, according to water access:
#    - improved water source, that is unpiped
#    - unimproved water source
#    - surface water source
# 2. Use `all_data.csv` to derive the mortality rate, in each LIC/LMIC,
#    among children under five years of age.
# 3. Take a weighted average of of the national mortality rates in (2) with
#    weights according to the populations calculated in (1).
#
#
#==============================================================================#


# Load packages ================================================================

library(tidyverse)
library(readxl)
library(janitor)
library(here)

# Load data ====================================================================

#LA: where do these data sets come from? "all data" is not a very meaningful name
all_data <- read_csv(here("data/raw/weighted_mr/all_data.csv"))

# Drinking water data from https://washdata.org/data/household#!/table?geo0=region&geo1=sdg
washdash_download <- read_csv(here("data/raw/weighted_mr/washdash-download.csv"))

# Wash data from from https://washdata.org/data/country/WLD/household/download
wash_data <- read_excel(here("data/raw/weighted_mr/JMP_2021_WLD.xlsx"), 'wat')

# WPP from https://population.un.org/wpp/Download/Archive/Standard/
# (see 2019 revision)

WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES <-
  read_excel(
    here("data/raw/weighted_mr/WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.xlsx"),
    col_types = c(
      "numeric",
      "text",
      "text",
      "text",
      "numeric",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    ),
    skip = 16
  )

# GNI data from https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
# provides GNI class (i.e. LIC and LMIC) from world bank
gni_class <- read_excel(here("data/raw/weighted_mr/CLASS.xlsx"))


wb_regions <- 
  gni_class %>%
  select(
    Country = Economy,
    Region
  ) 

# Prepare washdash data ========================================================

#LA: what's the difference between this data the and "wash" data?

wash <-
  washdash_download %>% 
  select(
    Country, 
    `Service level`, 
    Population
  ) %>% 
  spread(
    `Service level`,
    Population,
    fill = NA,
    convert = FALSE
  ) %>%
  mutate(
    # LA: Why replacing NAs with zero ?
    across(
      everything(),
      ~ replace_na(., 0)
    )
  )

# Prepare wash data ============================================================

wash_data <- 
  wash_data %>% 
  dplyr::filter(year == 2019) %>% 
  select(
    c(
      name,
      iso3,
      year,
      pop_n,
      wat_bas_n,
      wat_lim_n,
      wat_sm_n,
      wat_unimp_n,
      wat_sur_n,
      wat_pip_n
    )
  ) %>%
  mutate(
    # From proportion of the population to number
    wat_bas_n = wat_bas_n * pop_n / 100,
    wat_lim_n = wat_lim_n * pop_n / 100,
    wat_unimp_n = wat_unimp_n * pop_n / 100,
    wat_sur_n = wat_sur_n * pop_n / 100,
    # wat_unpiped_n is calculated as the proportion of the population with access to improved, but unpiped water sources
    wat_unpiped_n = ((100 - wat_unimp_n / pop_n - wat_sur_n / pop_n)  / 100) * (100 - wat_pip_n) * pop_n / 100,
    # war_sm_n is calculated in a similar way, but calculates the raw
    # population with safely managed water among improved water sources
    # https://washdata.org/monitoring/drinking-water
    wat_sm_n = ((100 - wat_unimp_n / pop_n - wat_sur_n / pop_n)  / 100) * wat_sm_n / 100 * pop_n,
    wat_nsm_n = pop_n - wat_sm_n,
    # LA: Why replacing NAs with zero ?
    across(
      everything(),
      ~ replace_na(., 0)
    )
  ) %>%
  rename(
    Country = name,
    'Basic service' = wat_bas_n,
    'Limited service' = wat_lim_n,
    'Unimproved' = wat_unimp_n,
    'Surface water' = wat_sur_n
  )


# Prepare GNI data ============================================================

gni_class <- 
  gni_class %>% 
  rename(Country = Economy) %>%
  select(Country, `Income group`)

# Prepare WPP data =============================================================
# then find the share of the population under 5 years of age

pop <-
  WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES %>% 
  rename(
    date = `Reference date (as of 1 July)`,
    Country = `Region, subregion, country or area *`
    ) %>%
  dplyr::filter(
    date == 2020,
    Type == "Country/Area"
  ) %>%
  select(
    -c(
      "Index",
      "Variant",
      "Type",
      "Parent code",
      "Country code",
      "Notes"
    )
  )  %>%
  mutate(
    sumVar = rowSums(.[2:22]),
    # Share under 5
    share_under5 = `0-4` / sumVar
  )

# Prepare 'all data' ===========================================================
# find mean under-five mr, under-five deaths per country

mortality <-
  all_data %>% dplyr::filter(
    SEX == "Total",
    REF_DATE == 2018.5,
    INDICATOR == "Under-five mortality rate" |  INDICATOR == "Under-five deaths"
  ) %>% 
  group_by(INDICATOR, REF_AREA) %>%
  summarize(OBS_VALUE = mean(OBS_VALUE)) %>%
  ungroup() %>% 
  spread(
    INDICATOR, OBS_VALUE, 
    fill = NA, 
    convert = FALSE
  ) %>% 
  rename(Country = REF_AREA) %>%
  mutate(`Under-five mortality rate` = `Under-five mortality rate` / 1000)

# Combine datasets =============================================================
# observations now at the country level

joined <- 
  mortality %>%
  left_join(wash_data) %>%
  left_join(pop) %>%
  left_join(gni_class) %>%
  left_join(wb_regions)

# filter observations at the national level by income group
# keep only LICs and LMICs
joined <-
  joined %>%
  dplyr::filter(
    `Income group` %in% c("Low income", "Lower middle income")
  ) %>%
  select(-`Income group`) %>%
  remove_empty(which = "rows")

# calculate the population without access to piped water 
# as per https://washdata.org/monitoring/drinking-water
# recall - wat_unpiped_n reflects the pop w/o access to piped water among
# individuals with access to improved water sources
joined <-
  joined %>%
  mutate(
    pop_badwater = `wat_unpiped_n` + `Surface water` + `Unimproved`,
    # LA: Why replacing NAs with zero ?
    across(
      everything(),
      ~ replace_na(., 0)
    )
  ) %>%
  rename(
    mortality = `Under-five mortality rate`
  )

joined <-
  joined %>%
  mutate(
    PSI_chlorine = ifelse(
      Country %in% c("Zambia", "Madagascar", "Tanzania", "Rwanda", "Malawi",
                     "Kenya", "Afghanistan", "Burkina Faso", "India", 
                     "Uzbekistan", "Myanmar", "Mozambique", "Nigeria", "Uganda", 
                     "Nepal", "Vietnam", "Ethiopia", "Burundi", "Guinea", "Cameroon"), 
      1, 
      0
    )
  )

# Save under 5 weighted mortality rate =========================================
  
joined %>% 
  summarise(avg_mortality_rate = weighted.mean(mortality, w = pop_badwater)) %>% 
  write_csv(here("data/transformed/weighted_u5_mr.csv"))

# Save data ====================================================================

joined %>%
  write_rds(
    here(
      "data/final/mortality_rate.rds"
    )
  )
