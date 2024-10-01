# Cost-effectiveness ===========================================================

library(here)
library(tidyverse)
library(common)
library(googlesheets4)

source(here("code/cea/cea-setup.R"))
load(here("data/final/ma_datasets.Rdata"))

## Labels ----------------------------------------------------------------------
cea_bigtab_cols <- c("Chlorine Dispensers in Western Kenya",
              "Chlorine Dispensers in Western Kenya (Alternative)",
              "Inline Chlorination in India",
              "Inline Chlorination in India (Alternative)",
              "Hypothetical Global MCH Delivery",
              "Hypothetical Global MCH Delivery (Alternative)")

cea_cols <- c("Chlorine Dispensers in Western Kenya",
              "Inline Chlorination in India",
              "Hypothetical Global MCH Delivery")

global_ben_cols <- c("Target Population",
                     "Population under five years of age (millions)",
                     "Proportion of population without access to piped water (p.p.)",
                     "# of <5y children without access to piped water (millions)",
                     "<5y mortality rate (p.p.)",
                     "Number of deaths among <5y without access to piped water per year (thousands)",
                     "Cost to serve full population without access to piped water per year ($ millions)",
                     "Total <5y lives saved per year (thousands)")

global_ben_nums <- c(" ", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)")

cea_bigtab_rows <- c(paste0("(1) <5y mortality rate (in p.p.)", supsc("a")),
                     paste0("(2) Posterior predictive mean (RR) of effect", supsc("b")),
                     paste0("(3) Average effective compliance in meta-analysis"),
                     paste0("(4) Effective take-up rate for intervention", supsc("c")),
                     paste0("(5) Expected deaths averted, per 1,000 served", supsc("d")),
                     paste0("(6) Expected DALYs averted, per 1,000 <5 children", supsc("e")),
                     paste0("(7) Cost of provision per <5 child, 5 years (USD)", supsc("f")),
                     paste0("(8) Cost per death of a <5 child averted (USD)", supsc("g")),
                     paste0("(9) Cost per DALY averted (USD)", supsc("h")),
                     paste0("(10) Net DALYs averted per 1,000 <5 children", supsc("i")),
                     paste0("(11) Net DALYs averted per 1,000 <5 children per USD 100 spent", supsc("j"))
                    )

cea_summary_rows <- c("Estimated mean OR effect of water treatment on child mortality, mean (95% CrI)",
                      "95% CrI",
                      "Posterior predictive estimate (RR) of effect, mean",
                      "Effective take-up",
                      "Expected deaths averted, per 1,000 served",
                      "Expected DALYs averted, per 1,000 eligible <5 child",
                      "Cost of provision per <5 child, 5 years (USD)",
                      "Cost per expected death of <5 child averted (USD)",
                      "Cost per expected DALY averted (USD)",
                      "Net DALYs averted per 1,000 <5 children",
                      "Net DALYs averted per 1,000 <5 children per USD 100 spent"
                      )

## Load auxiliary information --------------------------------------------------

### GDP pc ---------------------------------------------------------------------
# https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?locations=KE-IN
# weighted avg LIC and LMICs:
# https://data.worldbank.org/indicator/SP.POP.TOTL
# https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
default_gdp_pc <- c("coupons" = 2217, #calculated by Andreas P-Z
                    "ilc" = 2388, #India, 
                    "dsw" = 2099) #Kenya

default_gdp_pc %>%
  write_rds("data/transformed/default_gdp_pc.rds")
  
### DSW ------------------------------------------------------------------------
# From https://docs.google.com/spreadsheets/d/1yjVpBgZZhp5IANmGBD7yQ69203yKlR10pKEoDugdLMk/edit#gid=965674898
dsw_cost <- 161.64/14.4
dsw_u5_mr_untreated <- .0692

### Vouchers -------------------------------------------------------------------

# coupons (global program)
# Two pieces of this calculation are based on auxilliary scripts.
# We load in their outputs.
u5_mr <- read_csv(file = here("data/transformed/weighted_u5_mr.csv"))

#In Apr 2023 we calculated this as 1.82, but it has been updated since then
u5_per_hh_coupons <- read_csv(file = here("data/transformed/u5-per-hh.csv"))

# Cost of COUPONS: 
# 0.30 USD (Retail cost per bottle of chlorine) 
# * 12 months 
# * 0.37 (Average share of coupons redeemed across Dupas et al., 2016 (29) and Dupas et al., 2020 (28)) 
# * 1.5 (Assumption that for every two households with a child <5y without access to piped drinking water, 
# one untargeted household receives coupons) 
# * 2 (Assumption that administrative costs are as large as the price of chlorine bottles) 
# / 1.58 (number of <5 children per household with at least one 
# <5 child, calculated as a weighted average of the total number of <5 children over time divided by the 
# total number of households with at least one <5 child over time. We take a weighted average across all 
# countries for which household level data is available. All data is from IPUMS [add citation]);
cost_vouchers <- 0.3 * 12 * 0.37 * 1.5 * 2 #/1.58 done inside the function (u5_per_hh) 

### ILC ------------------------------------------------------------------------
ilc_u5_mr_untreated <- 0.039
ilc_cost <- 59.57/5

## Using observed take up from other sources -----------------------------------
df_cea <- 
  bind_rows(
    list(
      DSW = cea_dsw(default_or_ppd),
      ILC = cea_ilc(default_or_ppd),
      coupons = cea_cou(default_or_ppd)
    ),
    .id = "model"
  )

df_cea %>% 
  write_meta(
    path_data = "data/transformed/df_cea"
  )

# Formatting
tab_cea_base <- 
  df_cea %>%
  mutate_all(as.character) %>%
  mutate(
    bayes_OR = round(overall_est_bayes["mean"], 2),
    bayes_CI = paste0(
      "(", 
      round(overall_est_bayes["lower"],2), 
      ", ", 
      round(overall_est_bayes["upper"],2), ")"
    )
  )

## Conservative uptake estimates -----------------------------------------------

tab_cea_low <- 
  bind_rows(
    list(
      DSW = cea_dsw(default_or_ppd, tkup = 0.07, tkup_ctrl = 0),
      ILC = cea_ilc(default_or_ppd, tkup = 0.45, tkup_ctrl = 0),
      coupons = cea_cou(default_or_ppd, tkup =  0.1, tkup_ctrl = 0)
    ),
    .id = "model"
  ) %>%
  mutate_all(as.character) %>%
  mutate(
    bayes_OR = round(overall_est_bayes["mean"], 2),
    bayes_CI = paste0(
      "(", 
      round(overall_est_bayes["lower"],2), 
      ", ", 
      round(overall_est_bayes["upper"],2), ")"
    ),
    model = paste0(model, "_low")
  ) 

## Supplementary table ---------------------------------------------------------
cea_bigtab_full <- 
  bind_rows(tab_cea_base, tab_cea_low) %>% 
  transmute(
    p1, rr, t_ma, effective_tkup, 
    reduction, daly_reduction, 
    cost, cost_per_death, cost_per_daly,
    net_dalys_gdp, 
    net_dalys_100 = if_else(as.numeric(net_dalys_100) < 0, "0", net_dalys_100)
  ) %>%
  t()

colnames(cea_bigtab_full) <- c(tab_cea_base$model, tab_cea_low$model)

cea_bigtab_full %>% 
  write.csv("output/tables/table-cea-estimates.csv")

if (update_tables) {
  
  cea_bigtab_full <-
    bind_cols(
      cea_bigtab_rows,
      cea_bigtab_full
    ) %>%
    relocate(DSW_low, .after = DSW) %>%
    relocate(ILC_low, .after = ILC) %>%
    relocate(coupons_low, .after = coupons)
  
  sheet_write(cea_bigtab_full, gsheet, "table-cea-estimates_s7")
  
}

## Summary table in the main text ----------------------------------------------
df_cea_summary <- 
  tab_cea_base %>%
  transmute(
    bayes_OR, 
    bayes_CI,
    rr,
    effective_tkup,
    reduction,
    daly_reduction, 
    cost, 
    cost_per_death, 
    cost_per_daly,
    net_dalys_gdp,
    net_dalys_100 = if_else(as.numeric(net_dalys_100) < 0, "0", net_dalys_100)
  ) %>%
  t()

colnames(df_cea_summary) <- c("DSW", "ILC", "coupons")

df_cea_summary %>% 
  write.csv("output/tables/table-cea-summary.csv")

if (update_tables) {
  cea_summary_gsheet <- data.frame(cbind(cea_summary_rows, df_cea_summary)) %>%
    setNames(c("Stats", cea_cols)) %>%
    setNames(replace(names(.), names(.) == "Stats", ""))
  
  sheet_write(cea_summary_gsheet, gsheet, "table-cea-estimates")
}

# Global benefits --------------------------------------------------------------

# cost of provision, coupons
coeff <- 
  df_cea %>%
  dplyr::filter(model == "coupons") %>%
  pull(cost)

rr <- 
  df_cea %>%
  dplyr::filter(model == "coupons") %>%
  pull(rr)

# function to calculate weighted mean when there are na's present in both
# x (the series of which we are taking an average) and w (the weights)
# proposed solution from: 
# https://stackoverflow.com/questions/40269022/weighted-average-using-na-weights
weighted_mean = function(x, w, ..., na.rm=F){
  if(na.rm){
    keep = !is.na(x) & !is.na(w)
    w = w[keep]
    x = x[keep]
  }
  weighted.mean(x, w, ..., na.rm=F)
}

mortality_rate <-
  read_rds(
    here(
      "data/final/mortality_rate.rds"
    )
  )

globalbenefits <- mortality_rate %>%
  dplyr::filter(
    Region %in% c(
      "East Asia & Pacific", 
      "Sub-Saharan Africa", 
      "Middle East & North Africa", 
      "South Asia"
    )
  ) %>%
  slice(rep(1:n(), each = 2)) %>% 
  mutate(
    Region = case_when(
      duplicated(Country) ~ "Total",
      TRUE ~ Region
    )
  ) %>% 
  group_by(Region) %>%
  summarise(
    no_safe_managed = mean(wat_nsm_n, na.rm = TRUE),
    u5_mr = mean(mortality, na.rm=TRUE),
    u5_weighted_mr = weighted_mean(
      as.vector(mortality), 
      w = as.vector(pop_badwater), 
      na.rm = TRUE
    ),
    u5_PSI_mr = weighted_mean(
      as.vector(mortality), 
      w = as.vector(PSI_chlorine), 
      na.rm = TRUE
    ),
    pop = sum(sumVar, na.rm = T),
    no_safe_water_pop = sum(wat_nsm_n),
    no_piped_water_pop = sum(wat_unpiped_n),
    u5_share = weighted_mean(
      as.vector(share_under5),
      w = as.vector(pop_n), 
      na.rm = TRUE
    ),
    weighted_u5_share = weighted_mean(
      as.vector(share_under5),
      w = as.vector(wat_nsm_n), 
      na.rm = TRUE)
  ) %>%
  transmute(
    region = Region,
    pop_u5 = (pop * u5_share) / 1000,
    no_piped_water_pct = (no_piped_water_pop / pop) * 100,
    no_piped_water_u5 = (no_piped_water_pop * u5_share) / 1000,
    u5_mr = u5_weighted_mr * 100,
    u5_no_piped_water_mr = (1 - (1 - u5_weighted_mr)^(1/5)) * no_piped_water_u5 * 1000,
    coupons_cost = (coeff/5) * no_piped_water_u5,
    u5_saved = 0.32 * (1-rr) / 0.59 * u5_no_piped_water_mr
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(., 1)
    )
  )

globalbenefits %>%
  write_csv(here("output/tables/cea-globalbenefits.csv"))

if (update_tables) {
  globalbenefits_gsheet <- rbind(global_ben_nums, globalbenefits) %>%
    setNames(global_ben_cols)
  
  sheet_write(globalbenefits_gsheet, 
              gsheet,
              "cea-globalbenefits")
}

