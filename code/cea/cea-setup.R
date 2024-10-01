library(here)

# Main Bayesian model is used for these calculations
# as well as m-a-weighted compliance
load(here("output/stan/bayesian-models-for-exhibits.Rdata"))

# default_takeup <- 0.59 #in the old meta-analysis, updated since
default_takeup_trt  <- summarise_prevalence_compliance$compliance1 #see load_bayes_ma.R
default_takeup_ctrl <- summarise_prevalence_compliance$tkup_ctrl1 
default_takeup <- default_takeup_trt - default_takeup_ctrl

default_or_ppd <- mean(bg_main_or_ppd)

gdp_multiplier <- 1

# Helper function that calculates cost per DALY -----
cea_v2 <- function(x, #either a baggr object, draws of ORs or RRs (see code)
                   p1, #<5y mortality rate in untreated
                   tkup, #takeup rate for the intervention 
                   cost, #cost of provision PER YEAR per household with <5 yo child
                   tkup_ctrl = 0, #takeup in control arm
                   u5_per_hh = 1,
                   years = 5,
                   result = "single",
                   input = "or",
                   gdp_pc
) {
  # Obtain the log(OR) or RR
  if(input == "or"){
    if(inherits(x, "baggr"))
      or <- exp(effect_draw(x))
    else
      or <- x
    o1 <- p1/(1-p1)
    o2 <- or*o1
    p2 <- o2/(o2+1)
    rr <- p2/p1
    if(inherits(x, "baggr"))
      x <- treatment_effect(x)[[1]]
    else
      x <- log(x) #Because we will be taking exp later
  } else if(input == "rr") {
    rr <- x
  }
  
  # What proportion of population will avoid death when trt introduced?
  # takeup_rate <- tkup / default_takeup
  # Some people in control arm may use intervention, hence this adjustment
  # (this does not work linearly, but for small tkup_ctrl it's close, so okay
  #  to keep it simple like this)
  effective_takeup_rate <- (tkup - tkup_ctrl)/default_takeup
  
  # Reduction scales proportionally to relative take-up
  reduction <- effective_takeup_rate*p1*(1-rr)
  # What is cost of the program, total, per household?
  total_cost <- years*cost
  cost_per_u5 <- total_cost / u5_per_hh
  # WHO method of accounting for DALYs lost:
  daly_lost <- 81.25 - 2 #let's assume average age of death is 2
  daly_reduction <- daly_lost*reduction
  net_benefits <- mean(daly_reduction) * gdp_pc * gdp_multiplier - cost_per_u5
  
  tab <- list(
    or = round(mean(exp(x)), 3),
    rr = round(mean(rr), 2),
    p1 = round(100*p1, 1),
    t_ma = default_takeup %>% round(2),
    effective_tkup = (tkup - tkup_ctrl) %>% round(2),
    reduction = round(mean(reduction) * 1000),
    daly_reduction = round(mean(daly_reduction), 2) * 1000,
    cost = round(cost_per_u5, 1),
    cost_per_death = round(cost_per_u5/mean(reduction)),
    cost_per_daly = round(cost_per_u5/mean(daly_reduction)),
    net_ben = round(net_benefits),
    net_dalys_gdp = ((daly_reduction - (cost_per_u5/gdp_pc)) * 1000) %>% round,
    net_dalys_100 = ((daly_reduction - (cost_per_u5/100)) * 1000) %>% round
  )
  
  if(result == "table") #output the rows of Table 2
    return(tab)
  else
    return(daly_reduction)
}

# Default settings for 3 cases: DSW, ILC, coupons

# Cost of DSW
# dsw_cost <- 9.1 #old assumption
# instead: data here:
# https://docs.google.com/spreadsheets/d/1yjVpBgZZhp5IANmGBD7yQ69203yKlR10pKEoDugdLMk/edit#gid=965674898
dsw_cost <- 161.64 / 14.4

# DSW
cea_dsw <- function(or, tkup = 0.438, tkup_ctrl = 0.083) {
  cea_v2(
    result = "table", 
    x = or, 
    p1 = .0692, 
    tkup = tkup,
    tkup_ctrl = tkup_ctrl,
    cost = dsw_cost, 
    u5_per_hh = 1,
    gdp_pc = default_gdp_pc[["dsw"]]
  )
}

# ILC (Kenya)
# cea_ilc <- function(or) cea_v2(result = "table", 
#                                x = or, 
#                                p1 = .0692, 
#                                tkup = 0.91, 
#                                cost = 11.63,
#                                u5_per_hh=1,
#                                gdp_pc=5211)

# ILC (India)
cea_ilc <- function(or, tkup = 0.75, tkup_ctrl = 0.06) {
  cea_v2(
    result = "table", 
    x = or, 
    p1 = 0.039, #Pickering et al ,
    tkup = tkup,
    tkup_ctrl = tkup_ctrl,
    cost = 59.57/5, 
    u5_per_hh=1, #The calculation works differently, so we set it to 1
    gdp_pc=default_gdp_pc[["ilc"]]
  )
}
  

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
cea_cou <- function(or, tkup = 0.3, tkup_ctrl = 0.04) {
  cea_v2(
    result = "table", 
    x = or, 
    p1 = u5_mr$avg_mortality_rate, 
    tkup = tkup, 
    tkup_ctrl = tkup_ctrl,
    cost = cost_vouchers,
    u5_per_hh = u5_per_hh_coupons[["x"]], 
    gdp_pc = default_gdp_pc[["coupons"]]
  )
} 