set.seed(1990)

load(here("data/transformed/ma_datasets.Rdata"))
# Main Bayesian model is used for these calculations
source(here("code/cea/cea-setup.R"))



# Calculate and plot cost per DALY averted as a function of OR -----

# Calculate USD/DALY saved cost for a given OR, 
# for each of the three methods we use
calculate_daly_cost <- function(x) {
  c("DSW" = cea_dsw(x)$cost_per_daly,
    "ILC" = cea_ilc(x)$cost_per_daly,
    "coupons" = cea_cou(x)$cost_per_daly
  )
}

# Calculation
x <- seq(.7, .99, .001)
y <- sapply(x, calculate_daly_cost) %>% t()
df_cost <- as.data.frame(y) %>% mutate(effect = x)

# Find value of OR at which we do not exceed particular cost -----
cost_cutoff <- 200 #200 USD per DALY
or_required_for_200daly <- apply(y, 2, function(z) 
  max(x[which(z < cost_cutoff)]))

# What kind of weight would you have to have on the "linear" model
or_m1 <- (oeb(bg_main)[["mean"]])
or_m2 <- (1 - linear_model_reduction)
or_avg <- (or_required_for_200daly)
weights_required_for_200daly <- (or_avg - or_m2)/(or_m1 - or_m2)

or_required_for_200daly %>% round(3) %>% print()
weights_required_for_200daly %>% round(2) %>% print()

# Plot this -----
df_cost %>% 
  # mutate(relative_prec = (1/(sd^2))/bg_prec) %>% 
  # mutate(weight = (1/(sd^2))/(bg_prec + (1/(sd^2)))) %>% 
  gather(key, value, -effect) %>% 
  ggplot(aes(x=effect, y=value, color =key)) +
  geom_hline(yintercept = 200, lty = "dashed") +
  geom_vline(xintercept = 0.961, lty = "dotted") +
  geom_line(linewidth = 1.1) +
  ylab("Cost per DALY") + xlab("Mean effect size (OR)") +
  scale_y_log10() +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") +
  scale_color_discrete(name = "Cost-effectiveness of ...")








# Examine the models with priors of increasing precision -----

bsl_pool <- "partial" #choose partial or none for alternative specification
#partial is preferable, but none could be good for debug

do.call(baggr_compare, bg_priors)
sds_prior <- names(bg_priors) %>% as.numeric()

hypermeans <- lapply(bg_priors, function(x) treatment_effect(x, s=T)[[1]])
hypersds <- lapply(bg_priors, function(x) treatment_effect(x, s=T)[[2]])
ppds <- lapply(bg_priors, function(x) effect_draw(x, s=T))

rbind(
  bind_rows(hypermeans, .id = "prior") %>% mutate(stat = "b) Posterior: mean effect in 15 studies"),
  bind_rows(hypersds, .id = "prior") %>% mutate(stat = "c) Posterior: Heterogeneity (hyper-SD) in 15 studies"),
  bind_rows(ppds, .id = "prior") %>% mutate(stat = "d) Posterior predictive distribution (new study)")
) %>%
  
  select(-sd) %>% 
  # mutate_if(is.numeric, log) %>%
  mutate(prior = as.numeric(prior)) %>% 
  setNames(c("prior", "lci", "mean", "uci", "median", "stat")) %>%
  rbind(
    data.frame(prior = sds_prior, 
               lci = log(1-linear_model_reduction) - 1.96*sds_prior, 
               mean = log(1-linear_model_reduction), 
               uci = log(1-linear_model_reduction) + 1.96*sds_prior, 
               median=NA, stat = "a) Prior")
  ) %>% 
  ggplot(aes(x = mean, xmin = lci, xmax = uci, y = prior)) +
  geom_point() + geom_errorbarh() + facet_wrap(~stat) +
  ggtitle("Impact of introducing more precise priors on mean effect",
          "Assume following priors: mean effect is Normal(log(1 - .039), y), hyper-SD is Normal(0, 5). 
          \n Each row is a different model, with precision increasing from top to bottom.
          \n Each point is mean, each bar is 95% interval. There is a bit of noise in results due to quick model runs.") +
  theme_minimal(base_size = 14) +
  # scale_y_continuous() +
  xlab("Effect on log(OR), mean and 95% interval") +
  ylab("SD of prior")
  

# How big are the mean effect sizes? -----

# Table with all posterior effect values
mean_effect_sizes <- bind_rows(hypermeans, .id = "prior") %>% 
  mutate(stat = "b) Posterior: mean effect in 15 studies") %>% 
  mutate_if(is.numeric, exp) %>%
  select(-median, -sd, -stat)

mean_effect_sizes

precision_main_model <- 1/var(treatment_effect(bg_main)[[1]])

# What weight on prior does this imply?
sapply(or_required_for_200daly, function(or) {
  p <- mean_effect_sizes %>% 
    dplyr::filter(mean <= or) %>% 
    pull(prior) %>% as.numeric() %>% 
    min()
  # 95% interval for this prior
  lci <- exp(log(1 - linear_model_reduction) - 1.96*p)
  uci <- exp(log(1 - linear_model_reduction) + 1.96*p)
  # Ratio of precisions
  
  
  precision_alt_model <- 1/(p^2)
  c(or, 
    p, 
    lci, 
    uci,
    pnorm(log(0.9), log(1 - linear_model_reduction), p),
    precision_alt_model,
    precision_alt_model/precision_main_model
  )
})
