#==============================================================================#
#
# This code calculates the average number of U5 children for households that 
# have at least one U5 child. The data for this comes from a collection of 
# household surveys from IPUMS for selected low- and middle-income countries.
#
# The data is stored in a compressed format as `idhs_00001.dat 1.gz` and has to 
# be unpacked to make the replication work (attention: filesize goes from 33MB to 1.6GB).
# 
# Author: Alex Lehner
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

ddi         <- read_ipums_ddi(here("data/raw/u5-per-hh/idhs_00001.xml")) #IPUMS data
data        <- read_ipums_micro(ddi)

# file with country codes:
un_csv      <- read_csv(here("data/raw/u5-per-hh/UNSD.csv")) #UN country code to country name

# select DDI columns needed:
data_subset <- data[ , c(1, 3, 4, 9, 10, 11, 12, 13, 110)] # adding 9, hhid, hhlineno

# unique countries
df          <- data.frame("COUNTRY" = unique(data_subset$COUNTRY)) 


# ============================================================================
# NOW DOING IT FOR EVERY COUNTRY

df$population       <- NA
df$hhsize           <- NA
df$share_u5hh       <- NA
df$avg_u5_per_u5_hh <- NA

# computationally not the best, a function and then an lapply + bind_rows would have been better
for (country in df$COUNTRY) {
 
  holdout <- data_subset[data_subset$COUNTRY == country, ]
  
  df[df$COUNTRY == country, ]$population <- sum(holdout$POPWT_HH) # total pop in country
  
  # here we collapse everything to the household level
  holdout.hhsumm                     <- holdout |> group_by(HHID) |> dplyr::filter(HHLINENO == max(HHLINENO))
  df[df$COUNTRY == country, ]$hhsize <- mean(holdout.hhsumm$HHLINENO) # average hhsize
  
  holdout.hhsumm$u5hh                     <- holdout.hhsumm$KIDLT5NO > 0
  df[df$COUNTRY == country, ]$share_u5hh  <-  mean(holdout.hhsumm$u5hh) # share of households with u5 child
  
  # now we select only the households with children U5
  holdout.hhsumm                               <- holdout.hhsumm |> dplyr::filter(KIDLT5NO > 0)
  # the mean over all non zero U5 households gives us the average we want:
  df[df$COUNTRY == country, ]$avg_u5_per_u5_hh <- mean(holdout.hhsumm$KIDLT5NO) 
  
}

# assumption of the exercise was that all the surveys that were used were nationally representative
# ... for DHS and MICS this is certainly the case
# weighted mean to account for different country sizes
df$global_weighted_avg <- weighted.mean(df$avg_u5_per_u5_hh, w = df$population)


# Save output ==================================================================
write.csv(
  df$global_weighted_avg[1], 
  file = here("data/transformed/u5-per-hh.csv")
)

