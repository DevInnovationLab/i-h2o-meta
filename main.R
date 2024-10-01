# Settings =====================================================================
# Set update_tables to TRUE to update values in the paper manuscript. This will
# only work if you have write access to the google sheet
update_tables <- FALSE
update_dhs <- FALSE # only necessary if new DHS data was downloaded
data_wrangling <- TRUE
fit_bayes <- TRUE
recreate_outputs <- TRUE

if (update_tables) {
  library(googlesheets4)
  gsheet <- "https://docs.google.com/spreadsheets/d/1P-gC4qvXYU40_gQXW4-qGs4wJppKVWr56wv20xNNK6w/edit#gid=0"
  gs4_auth() # Authenticate via browser. You will need to manually select the option to allow R to modify spreadsheets
}

# Prepare data =================================================================
# Running all the scripts in this section will only recreate the data sets in data/final

## List of studies -------------------------------------------------------------
if (data_wrangling) {

# Inputs: 
# - data/raw/diarrhea_studies.xlsx
# - data/raw/weighted_mr/CLASS.xlsx
# Outputs: data/final/diarrhea_studies.rds
source("code/wrangling/clean_studies_list.R")

## Processes data from studies with individual-level data ----------------------
## (requires access to PII)

# Inputs: files in data/raw/mortality_counts
# Outputs: data/final/individual_data_anonymised.rds
source("code/wrangling/prep_individual_data.R")

# Inputs:
# - data/raw/summary_data.csv
# - data/final/individual_data_anonymised.rds
# - data/raw/weighted_mr/CLASS.xlsx
# Outputs: data/transformed/trials.Rda
source("code/wrangling/prep_trial_data.R")

## Study-level modeling of odds ratios -----------------------------------------
## (with corrections for clustering where needed)

# Inputs: 
# - data/transformed/trials.Rda
# - code/ma_models/logit_model.stan
# Outputs: output/stan/cluster_bayes_models.Rdata (this file is very heavy and is ignored in GitHub)
source("code/wrangling/prep_cluster_models.R") # Run time: ~30 min

## Merge inputs into a data frame with OR that can be meta-analysed ------------

# Inputs: 
# - data/transformed/trials.Rdata
# - output/stan/cluster_bayes_models.Rdata 
# Outputs: data/final/ma_datasets.Rdata
source("code/wrangling/prep_adjusted_data.R")

# Cost-effectiveness analysis inputs ===========================================

# Inputs:
# - data/raw/weighted_mr/all_data.csv
# - data/raw/weighted_mr/OGHIST.xlsx
# - data/raw/weighted_mr/JMP_2021_WLD.xlsx
# - data/raw/weighted_mr/washdash-download.csv
# - data/raw/weighted_mr/WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.xlsx
# Outputs:
# - data/transformed/weighted_u5_mr.csv
# - data/final/mortality_rate.rds
source("code/cea/weighted-mr.R")
}

# Inputs:
# - data/raw/u5-per-hh/idhs_00001.xml
# - data/raw/u5-per-hh/idhs_00001.dat.gz (needs to be unpacked before running the code)
# Outputs: data/transformed/u5-per-hh.csv
if (update_dhs) source("code/cea/u5-per-hh.R") # Sometimes breaks when running from the main script, in which case opening the R file directly should work

# Meta analysis models =========================================================

## Fit Bayesian models ---------------------------------------------------------

if (fit_bayes) {
  
# Inputs: 
# - data/final/ma_datasets.Rdata
# Outputs: 
# - output/stan/bayesian-ma-models.Rdata
# - output/stan/bayesian-mr-models.Rdata
source("code/ma_models/fit_ma_bayes.R") # Run time: ~1h

## Prepare Bayesian models for plots and tables --------------------------------
# Inputs: 
# - output/stan/bayesian-ma-models.Rdata
# - output/stan/bayesian-mr-models.Rdata
# Output:
# - output/stan/bayesian-models-for-exhibits.Rdata
source("code/ma_models/load_bayes_ma.R")

}

## Fit frequentist models ------------------------------------------------------
# (This code is called directly in the output scripts)
# Inputs: data/final/ma_datasets.Rdata
# source("code/ma_models/fit_ma_frequentist.R")

# Paper exhibits ===============================================================


## Output all paper figures ----------------------------------------------------

if (recreate_outputs) {
  
# Inputs:
# - data/final/ma_datasets.Rdata
# - output/stan/bayesian-models-for-exhibits.Rdata
# - data/raw/IHME_GLOBAL_DIARRHEA_2000_2019_PREV_A1_S3_ADMIN_1_Y2020M08D31.CSV
# - data/final/diarrhea_studies.rds
# Outputs:
# - output/figures/freq-forest
# - output/figures/bayes-forest
# - output/figures/ma-week-plot
# - output/figures/funnel
# - output/figures/bubble-plot-year
# - output/figures/dist-diarrhea-prevalence
# - output/figures/fig-compliance-diarr-hist
# - output/figures/mortality-vs-baseline
# - output/figures/diarr-pub-bias-funnel
source("code/generate_outputs/generate_figures.R")

## Output meta-analysis tables -------------------------------------------------

# Inputs:
# - code/analysis/fit_ma_frequentist.R
# - code/analysis/load_bayes_ma.R
# Outputs:
# - output/tables/freq-bayes-summary-mortality.csv
# - output/tables/table-loo-study.csv
# - output/tables/additional-sa-results.csv
# - output/tables/mortality_all_summary.csv
source("code/generate_outputs/generate_tables.R")

## Output cost-effectiveness analysis results ----------------------------------

# Inputs:
# - data/final/ma_datasets.Rdata
# - code/analysis/load_bayes_ma.R
# - data/transformed/weighted_u5_mr.csv
# - data/transformed/u5-per-hh.csv
# - code/cea/cea-setup.R
# - data/final/mortality_rate.rds
# Outputs:
# - output/tables/table-cea-estimates.csv
# - output/tables/table-cea-summary.csv
# - output/tables/cea-global-benefits.csv
source("code/generate_outputs/generate_cea_results.R")

# Additional analysis for Section 7 of the supplement
# (generates both a plot and some numbers cited in the text)
# Inputs:
# - code/cea/cea-setup.R
# Outputs:
# - output/figures/cea_or_relationship.pdf
source("code/cea/cea-priors.R")

## Numbers in text -------------------------------------------------------------

rmarkdown::render(
  here::here('code/generate_outputs/generate_text.Rmd'), 
  output_file = here::here('output/numbers-in-text.html')
)
  
}