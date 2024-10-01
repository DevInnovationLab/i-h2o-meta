#==============================================================================#
#
# This code calculates the average number of U5 children for households that 
# have at least one U5 child. The data for this comes from a collection of 
# household surveys from IPUMS for selected low- and middle-income countries.
#
# The data is stored in a compressed format as `idhs_00001.dat 1.gz` and has to 
# be unpacked to make the replication work (attention: filesize goes from 33MB to 1.6GB).
# 
# Author: Alex Lehner, Luiza Andrade
# ------------------------------------------------------------------------------
#
#   The final output that produces the (single) average number 
#   across all countries. This file is an input to the script that calculates Table S8
# 
#------------------------------------------------------------------------------#
#
# The general outline of the calculation is as follows:
#  1. Calculate the total population in the given year that we have data for 
#     (sum up the survey weights POPWT_HH, or equivalently one could do for any 
#     row in the dataset POPWT_HH / (HHWEIGHT / sum(all HHWEIGHTS of country))
#  2. Sum up the number of individuals in each household to get household size. 
#     Then take the mean of this number to get the average household size in the 
#     country in the given year we see the data for.
#  3. Assign a 0 or 1 whether the household does have a U5 child or not. Taking 
#     the mean of this variable gives the share of households that have a U5 
#     child in the given country in the given year.
#  4. Then we subset only these households that have a U5 child and take the 
#     mean of the number of U5 children over all of these households. This gives
#     the average number of U5 children given that there is at least one U5 child 
#     already in the household - in the given country in the given year.
#  5. Finally, we take a weighted mean of this number based on the population size 
#     of the country (so big countries influence this mean proportional to their 
#     size - otherwise the number for small countries like CAF or Eswatini count 
#     as much as for India). This is also the number that gets stored in `u5-per-hh.csv`
# 
#==============================================================================#

# Load packages ================================================================

library(ipumsr)
library(ids)
library(here)
library(tidyverse)

# Load and prepare data ========================================================
# please unpack the .gz file if you want to replicate the code (attention, 1.6GB in size)

ddi <- read_ipums_ddi(here("data/raw/u5-per-hh/idhs_00001.xml")) #IPUMS data
data <- read_ipums_micro(ddi) %>%
  select(
    SAMPLE, COUNTRY, YEAR, HHID, HHLINENO, HHWEIGHT, POPWT_HH, HHMWEIGHT, KIDLT5NO
  )

# Country-level data ===========================================================

df <- 
  data %>%
  group_by(HHID, COUNTRY) %>%
  summarise(
    population_weight = sum(POPWT_HH),
    KIDLT5NO = unique(KIDLT5NO) %>% na_if(0)
  ) %>%
  group_by(COUNTRY) %>%
  summarise(
    population = sum(population_weight),
    avg_u5_per_u5_hh = mean(KIDLT5NO, na.rm = TRUE)
  )

# assumption of the exercise was that all the surveys that were used were nationally representative
# ... for DHS and MICS this is certainly the case
# weighted mean to account for different country sizes
global_weighted_avg <- weighted.mean(df$avg_u5_per_u5_hh, w = df$population)


# Save output ==================================================================
write.csv(
  global_weighted_avg, 
  file = here("data/transformed/u5-per-hh.csv")
)

