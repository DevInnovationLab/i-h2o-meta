library(tidyverse)
library(scales)
library(forestplot)
library(ggpubr)
library(meta)
library(baggr)
library(here)

load(here("data/final/ma_datasets.Rdata"))
load(here("output/stan/bayesian-models-for-exhibits.Rdata"))
load(here("output/stan/bayesian-mr-models.Rdata"))
source(here("code/functions/helpers.R"))
source(here("code/ma_models/fit_ma_frequentist.R"))


# Forest plots #################################################################

## Prepare data ================================================================

# We have frequentist and Bayesian FPs so I use one function to prep data

data_for_fp <- function(chlori_studies_freq,
                        filter_studies_freq,
                        spring_studies_freq,
                        sodis_studies_freq,
                        sub_estimates_freq,
                        overall_est_freq) {
  data.frame(
    mean  = c(
      NA,
      NA,
      NA,
      pull(chlori_studies_freq, mean),
      sub_estimates_freq["df_chlorination", "mean"],
      NA,
      NA,
      pull(filter_studies_freq, mean),
      NA,
      NA,
      pull(spring_studies_freq, mean),
      NA,
      NA,
      pull(sodis_studies_freq, mean),
      NA,
      overall_est_freq[["mean"]]
    ),
    lower  = c(
      NA,
      NA,
      NA,
      pull(chlori_studies_freq, lower),
      sub_estimates_freq["df_chlorination", "lower"],
      NA,
      NA,
      pull(filter_studies_freq, lower),
      NA,
      NA,
      pull(spring_studies_freq, lower),
      NA,
      NA,
      pull(sodis_studies_freq, lower),
      NA,
      overall_est_freq[["lower"]]
    ),
    upper = c(
      NA,
      NA,
      NA,
      pull(chlori_studies_freq, upper),
      sub_estimates_freq["df_chlorination", "upper"],
      NA,
      NA,
      pull(filter_studies_freq, upper),
      NA,
      NA,
      pull(spring_studies_freq, upper),
      NA,
      NA,
      pull(sodis_studies_freq, upper),
      NA,
      overall_est_freq[["upper"]]
    ),
    ci = c(
      NA,
      NA,
      NA,
      pull(chlori_studies_freq, CI),
      interval2(sub_estimates_freq["df_chlorination", "lower"],
                sub_estimates_freq["df_chlorination", "upper"]),
      NA,
      NA,
      pull(filter_studies_freq, CI),
      NA,
      NA,
      pull(spring_studies_freq, CI),
      NA,
      NA,
      pull(sodis_studies_freq, CI),
      NA,
      oef_ci(overall_est_freq)
    ),
    labeltext = c(
      "Study",
      "",
      "Chlorine",
      chlori_studies_freq$trial_name,
      "Sub-group estimate",
      NA,
      "Filtration",
      filter_studies_freq$trial_name,
      NA,
      "Spring protection",
      spring_studies_freq$trial_name,
      NA,
      "Solar disinfection",
      sodis_studies_freq$trial_name,
      NA,
      "Overall estimate"
    )
    
  )
  
}

data_for_fp_freq <- data_for_fp(
  chlori_studies_freq,
  filter_studies_freq,
  spring_studies_freq,
  sodis_studies_freq,
  sub_estimates_freq,
  overall_est_freq
)

sub_estimates_bayes <-
  sub_estimates_bayes[c("mean", "lower", "upper", "pval", "CI")]

data_for_fp_bayes <- data_for_fp(
  chlori_studies_bayes,
  filter_studies_bayes,
  spring_studies_bayes,
  sodis_studies_bayes,
  sub_estimates_bayes,
  overall_est_bayes
)

## Frequentist plots ===========================================================

output <-
  data_for_fp_freq %>%
  select(mean, lower, upper) %>%
  as.matrix()

tabletext <-
  data_for_fp_freq %>%
  mutate(mean = round(mean, 2)) %>%
  select(labeltext, mean, ci) %>%
  as.matrix()

tabletext[1,] <- c("Study", "OR", "95% CI")

forest <- forestplot::forestplot(
  tabletext,
  output[, 1],
  output[, 2],
  output[, 3],
  new_page = FALSE,
  is.summary = c(
    TRUE,
    TRUE,
    TRUE,
    rep(FALSE, nrow(chlori_studies_freq)),
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    TRUE,
    TRUE,
    FALSE,
    TRUE,
    TRUE,
    FALSE,
    TRUE,
    TRUE
  ),
  clip = c(0.25, 4),
  xlog = T,
  col = fpColors(
    box = "black",
    line = "black",
    summary = "black"
  ),
  txt_gp = fpTxtGp(ticks = gpar(cex = 0.9)),
  hrzl_lines = list("2" = gpar(lwd = 1, columns = 1:3))
)

dev.off()

png(
  file = here('output/figures/freq-forest.png'),
  width = 8,
  height = 8,
  unit = "in",
  res = 300
)

print(forest)

dev.off()

pdf(
  file = here('output/figures/freq-forest.pdf'),
  width = 8,
  height = 8
)

print(forest)

dev.off()

# Bayes FP =====================================================================

output2 <-
  data_for_fp_bayes %>%
  select(mean, lower, upper) %>%
  as.matrix()

colnames(output2) <- c("coef", "low", "high")

tabletext <-
  data_for_fp_bayes %>%
  mutate(mean = round(mean, 2)) %>%
  select(labeltext, mean, ci) %>%
  as.matrix()

tabletext[1,] <- c("Study", "OR", "95% CI")

png(
  file = here('output/figures/bayes-forest.png'),
  width = 8,
  height = 8,
  unit = "in",
  res = 300
)

plot <- dev.cur()

pdf(file = here('output/figures/bayes-forest.pdf'), 
    width = 8, height = 8)

dev.control("enable")

forest <-
  forestplot::forestplot(
  tabletext,
  output2[, 1],
  output2[, 2],
  output2[, 3],
  new_page = FALSE,
  is.summary = c(
    TRUE,
    TRUE,
    TRUE,
    rep(FALSE, nrow(chlori_studies_freq)),
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    TRUE,
    TRUE,
    FALSE,
    TRUE,
    TRUE,
    FALSE,
    TRUE,
    TRUE
  ),
  clip = c(0.25, 4),
  xlog = T,
  col = fpColors(
    box = "black",
    line = "black",
    summary = "black"
  ),
  txt_gp = fpTxtGp(ticks = gpar(cex = 0.9)),
  hrzl_lines = list("2" = gpar(lwd = 1, columns = 1:3))
)

print(forest)
  
dev.copy(which = plot)
dev.off()
dev.off()

# Sensitivity to exclusion of shorter studies ##################################

freq_estimates_duration %>%
  ggplot(aes(x = Weeks)) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             linewidth = 0.5) +
  geom_point(aes(y = mean, colour = "Freq OR estimate")) +
  geom_line(aes(y = mean, colour = "Freq OR estimate"), linetype = "dashed") +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper,
      colour = "95% CI"
    ),
    width = 3,
    position = position_dodge(0.1)
  ) +
  scale_color_hue(l = 40, c = 35) +
  ylim(0.4, 1.3) +
  xlab("X weeks") +
  ylab("Freq OR estimates") +
  ggtitle("Sensitivity of meta-analysis estimates to weeks of monitoring") +
  default_theme +
  theme(legend.title = element_blank(),
        legend.position = "none")

ggsave(
  "output/figures/ma-week-plot.pdf",
  width = 14,
  height = 10,
  units = "cm"
)

ggsave(
  "output/figures/ma-week-plot.png",
  width = 14,
  height = 10,
  units = "cm"
)

# Funnel plot ##################################################################

pdf(
  file = "output/figures/funnel.pdf",
  width = 6,
  height = 6
)

fma_re %>% funnel()

dev.off()

png(file = "output/figures/funnel.png",
    width = 15,
    height = 15,
    units = "cm",
    res = 300)

fma_re %>% funnel()

dev.off()

# Bubble plots #################################################################

bubble(mr_fits[["year"]], covariate = "year", label = FALSE) +
  theme_minimal() +
  ggrepel::geom_text_repel(aes(x = .covariate, y = mean, label = group),
                           size = 2.5,
                           box.padding = 1) +
  xlab("Study year")

ggsave(
  "output/figures/bubble-plot-year.pdf",
  width = 14,
  height = 10,
  units = "cm"
)

ggsave(
  "output/figures/bubble-plot-year.png",
  width = 14,
  height = 10,
  units = "cm"
)

# Distribution of diarrhea prevalence (IHME data) ##############################

# prevalence avg in studies

df_main_ma_adj %>% 
  mutate(ccases_per_yr = 52 * ccases / weeks) %>%
  summarise(sum(ccases_per_yr) / sum(c_n))

# Graphing
df_diarrhea_prev <-
  read_csv("data/raw/IHME_GLOBAL_DIARRHEA_2000_2019_PREV_A1_S3_ADMIN_1_Y2020M08D31.CSV")

df_all_ma_adj_prev <- 
  df_all_ma_adj %>%
  select(trial_name, prevalence) %>%
  distinct() %>%
  rename(
    obs = trial_name, 
    mean = prevalence
  ) %>%
  mutate(group = "Studies")

df_diarrhea_prev <- 
  df_diarrhea_prev %>%
  select(ADM1_NAME, mean) %>%
  distinct() %>%
  rename(obs = ADM1_NAME) %>%
  # multiply by 5, since the data are entered as a rate
  # i.e. what is the prevalence of diarrhea among children of a given age, among u5s
  # rather than the prevalence of diarrhea among all children under 5
  mutate(
    mean = 5 * mean,
    group = "IHME"
  )

df_prev <- rbind(df_all_ma_adj_prev, df_diarrhea_prev)

ggplot(df_prev, aes(x = mean, fill = group)) +
  geom_histogram(
    data = subset(df_prev, group == 'Studies'),
    aes(y = after_stat(count), fill = group),
    alpha = 0.2,
    binwidth = 0.02,
    color = "red",
    fill = "red"
  ) +
  geom_density(
    data = subset(df_prev, group == 'IHME'),
    aes(y = ..density.., fill = group),
    alpha = 0.2,
    adjust = 1,
    color = "black",
    fill = "black"
  ) +
  geom_vline(xintercept = summarise_prevalence_compliance$prevalence1) +
  annotate(
    "text",
    x = summarise_prevalence_compliance$prevalence1 + 0.005,
    y = 7.5,
    label = "Weighted \naverage in \nmeta-analysis",
    hjust = 0
  ) +
  labs(x = "Diarrhea prevalence (%)",
       y = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave(
  "output/figures/dist-diarrhea-prevalence.pdf",
  width = 14,
  height = 10,
  units = "cm"
)

ggsave(
  "output/figures/dist-diarrhea-prevalence.png",
  width = 14,
  height = 10,
  units = "cm"
)

# Comparison of included vs excluded: effect on diarrhea and compliance ########

df_studies <-
  read_rds("data/final/diarrhea_studies.rds") %>%
  select(effect_estimate_on_diarrhea, compliance_rate, reference, included) %>%
  unique

hist_diarr_effect <- 
  df_studies %>%
  ggplot(aes(x = effect_estimate_on_diarrhea)) +
  geom_histogram() +
  facet_grid(included ~ .) +
  ylab("Number of studies") +
  xlab("Effect on diarrhea (OR)") +
  default_theme

hist_compliance <- 
  df_studies %>%
  ggplot(aes(x = compliance_rate)) +
  geom_histogram() +
  facet_grid(included ~ .) +
  ylab("Number of studies") +
  xlab("Compliance rate (%)") +
  default_theme

ggarrange(hist_diarr_effect, hist_compliance)

ggsave(
  "output/figures/fig-compliance-diarr-hist.pdf",
  width = 14,
  height = 8,
  units = "cm"
)

ggsave(
  "output/figures/fig-compliance-diarr-hist.png",
  width = 14,
  height = 8,
  units = "cm"
)

# Mortality vs. Baseline #######################################################

# or <- exp(effect_draw(bg_main, 1e05))
# p1 <- seq(0, .1, length = 100)
# m <- sapply(p1, function(p1) {
#   o1 <- p1 / (1 - p1)
#   o2 <- or * p1
#   p2 <- o2 / (1 + o2)
#   p2
# })
# 
# t(apply(m, 2, baggr::mint, int = 0.9)) %>%
#   as.data.frame() %>%
#   setNames(c("low", "mean", "high")) %>%
#   mutate(p1 = p1) %>%
#   ggplot(aes(
#     x = p1,
#     y = mean,
#     ymax = high,
#     ymin = low
#   )) +
#   geom_line() + geom_ribbon(alpha = .2) + theme_minimal() +
#   xlab("Control group mortality rate") + ylab("Treated group mortality rate") +
#   scale_x_continuous(labels = scales::percent, limits = c(0, .11)) +
#   scale_y_continuous(labels = scales::percent, limits = c(0, .11))
# 
# ggsave(
#   "output/figures/mortality-vs-baseline.pdf",
#   width = 14,
#   height = 10,
#   units = "cm"
# )
# 
# ggsave(
#   "output/figures/mortality-vs-baseline.png",
#   width = 14,
#   height = 10,
#   units = "cm"
# )

# Diarrhea publication bias funnel plot ########################################

df_diarrhea <-
  read_rds("data/final/diarrhea_studies.rds")

clean_fit = metagen(
  TE = ln_RR,
  seTE = se_imp,
  data = df_diarrhea,
  sm = "RR"
)

chlorine_fit = metagen(
  TE = ln_RR,
  seTE = se_imp,
  data = df_diarrhea %>% dplyr::filter(chlor),
  sm = "RR"
)

pdf(file = "output/figures/diarr-pub-bias-funnel.pdf",
    width = 6,
    height = 9)

plot_par = par(mfrow = c(2, 1))

funnel(clean_fit)
title(main = "All Diarrhea Interventions Funnel")

funnel(chlorine_fit)
title(main = "Chlorine Diarrhea Interventions Funnel")

dev.off()


png(file = "output/figures/diarr-pub-bias-funnel.png",
    width = 6,
    height = 9,
    units = "in",
    res = 300)

plot_par = par(mfrow = c(2, 1))

funnel(clean_fit)
title(main = "All Diarrhea Interventions Funnel")

funnel(chlorine_fit)
title(main = "Chlorine Diarrhea Interventions Funnel")

dev.off()

