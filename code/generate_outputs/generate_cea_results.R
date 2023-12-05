# Cost-effectiveness ===========================================================

library(here)
library(tidyverse)

source(here("code/cea/cea-setup.R"))

df_cea <- 
  bind_rows(
    list(
      DSW = cea_dsw(default_or_ppd),
      ILC = cea_ilc(default_or_ppd),
      coupons = cea_cou(default_or_ppd)
    ),
    .id = "model"
  )

df_cea_tab <- 
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

# Big table in the supplement (Table S7 at the time of writing) ----------------
df_cea_bigtab <- 
  df_cea_tab %>% 
  select(
    p1, rr, t_ma, tkup, 
    reduction, daly_reduction, 
    cost, cost_per_death, cost_per_daly, net_ben
  ) %>% 
  t()

colnames(df_cea_bigtab) <- df_cea[["model"]]
df_cea_bigtab %>% 
  write.csv("output/tables/table-cea-estimates.csv")

# Summary table in the main text -----------------------------------------------
df_cea_summary <- df_cea_tab %>% 
  select(c(bayes_OR, bayes_CI, daly_reduction, 
           cost, cost_per_death, cost_per_daly, net_ben)) %>% 
  t()
colnames(df_cea_summary) <- df_cea[["model"]]
df_cea_summary %>% write.csv("output/tables/table-cea-summary.csv")

# Some text --------------------------------------------------------------------

# Discussion: Ratio of GDP to cost per DALY reduction
default_gdp_pc / (df_cea_tab$cost_per_daly %>% as.numeric())


# Discussion: "we find that cost-effectiveness threshold
# is reached at 0.5% reduction in risk of under 5 mortality":
benken <- cea_v2(result = "table", 
                 input="rr", .995,
                 p1=.0692, 
                 tkup=0.51, 
                 cost=9.1, 
                 u5_per_hh=1,
                 gdp_pc=default_gdp_pc[["dsw"]])$cost_per_daly
benken/default_gdp_pc[["dsw"]]


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

mortality_rate %>%
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
  ) %>%
  write_csv(
    here("output/tables/cea-globalbenefits.csv")
  )