# Water Meta-Analysis

Code for [Water Treatment and Child Mortality: A Meta-analysis and Cost-effectiveness Analysis](https://bfi.uchicago.edu/working-paper/2022-26/) by Kremer, Luby, Maertens, 
Tan, and Więcek.

## Computational requirements

All code runs in R (last run on R version 4.2.3 and RStudio version 2023.09). `renv.lock` lists all necessary packages and their respective versions. `main.R` last run on a **10-core Intel-based laptop with Windows 10 and 16.0 GB of RAM**. Approximate runtime was **95** minutes.

## Description of programs/code

To recreate the exhibits in the paper,

1. Open `water-ma.RProj`
2. To install all the necessary packages, run `renv::restore()` (this step requires the package `renv`)
3. Run `main.R`

The code used to create the exhibits in the paper is in `code/`. `main.R` tracks all the inputs and outputs for each script. Below is an overview of what they do.

- **Scripts in `code/functions` create functions that are used across multiple scripts**. The are sourced by `.Rprofile` when `water-ma.RProj` is launched.
- **Scripts in `code/wrangling` process the raw data and create the final data sets used across different analysis scripts.** The first section of `main.R` runs the scripts in this directory. Running this section is optional, since it will only recreate the data sets in `data/final`.
- **Scripts in `code/cea` prepare the data used for cost-effectiveness analysis and include auxiliary code used in the cost-effectiveness analysis.** The second section of `main.R` runs the scripts in this directory. Running this section is also optional. It will recreate `data/final/mortality_rate.rds` and the under-five mortality rates in `data/transformed/u5-per-hh.csv` and `data/transformed/weighted_u5_mr.csv`.
- **Scripts in `code/ma_models` fit meta-analysis models.** 
  - `fit_ma_bayes.R` uses `rstan` model in `logit_model.stan`. It takes a moment to run and will save outputs to `output/stan`. 
  - `load_bayes_ma.R` is called from other scripts to load the outputs of `fit_ma_bayes.R`.
  - `fit_ma_frequentist.R` is called from other scripts as fits frequentist meta-analysis models.
- **Scripts in `code/generate_outputs` recreate the results in the paper.**
  - `generate_figures.R` recreates all the figures.
  - `generate_tables.R` recreates the meta-analysis tables in the paper.
  - `generate_cea_results.R` recreates the cost-effectiveness results.
  - `generate_text.Rmd` for printing the values that are cited inline in main text and supplement
- **Scripts in `code/publication_bias` perform the tests mentioned in supplement section 5.** 

### Controlled Randomness

### License for code

Code is licensed under [![License: Unlicense](https://img.shields.io/badge/License-Unlicense-blue.svg)](http://unlicense.org/), with the exception of `code/publication_bias/RobustVariance.R` and `code/publication_bias/metastudiesfunctions.R` by Maximilian Kasy, which are licensed under [![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/).

## List of paper exhibits and programs

| Paper   | File                                         | Created by             |
|---------|----------------------------------------------|------------------------|
| Fig 2A  | output/figures/freq-forest.pdf               | generate_figures.R     |
| Fig 2B  | output/figures/bayes-forest.pdf              | generate_figures.R     |
| Tab 2   | output/figures/table-cea-estimates           | generate_cea_results.R |
| Fig S1  | output/figures/ma-week-plot.pdf              | generate_figures.R     |
| Fig S2  | output/figures/fig-compliance-diarr-hist.pdf | generate_figures.R     |
| Fig S3  | output/figures/funnel.pdf                    | generate_figures.R     |
| Fig S4  | output/figures/diarr-pub-bias-funnel.pdf     | generate_figures.R     |
| Fig S5  | output/figures/bubble-plot-year.pdf          | generate_figures.R     |
| Fig S6  | output/figures/mortality-vs-baseline.pdf     | generate_figures.R     |
| Fig S7  | output/figures/dist-diarrhea-prevalence.pdf  | generate_figures.R     |
| Tab S3  | output/tables/mortality_all_summary.csv      | generate_tables.R      |
| Tab S4  | output/tables/table-loo-study.csv            | generate_tables.R      |
| Tab S5  | output/tables/additional-sa-results.csv      | generate_tables.R      |
| Tab S6  | output/tables/cea-globalbenefits.csv         | generate_cea_results.R |
| Tab S7  | output/tables/table-cea-estimates.csv        | generate_cea_results.R |

## Data Availability and Provenance Statements

Some data used in this study **cannot be made** publicly available. The raw data files used by the code are listed below.

| Data              | File                              | Provided | Citation | License | Notes |
| ----------------- | --------------------------------- | -------- | -------- | ------- |----- |
| List of diarrhea studies | `data/raw/diarrhea_studies.xlsx`  | Yes |   | [![License: CC BY 3.0 IGO](https://img.shields.io/badge/License-CC_BY_3.0_IGO-lightgrey.svg)](http://creativecommons.org/licenses/by/3.0/igo/) | Contains data provided in the supplementary material Wolf et al., 2018. The original datset from extracted from the publication available [here](https://onlinelibrary.wiley.com/doi/10.1111/tmi.13051). The data was provided for 80 studies included in the meta-anlysis. We added the relevant data for studies included in the meta-anlysis but not included in the Wolf et al., 2018 study (Peletz et al., 2012, Null et al., 2018, 	Luby et al., 2018, Humphrey et al., 2019, Kirby et al., 2019, Haushofer et al., 2020, Dupas et al., 2021, and ucation vs control) Quick et al., 1999, Conroy et al. 1999, Morris et al. 2018). In addition to the existing data, we added the following information for each of the studies - 1. Compliance rate, and 2. How is compliance defined. Wolf et al., 2018 collects estimates for under-5 diarrhea morbidity in studies with any WaSH intervention. |
| Studies summary   | `data/raw/summary_data.csv`       | Yes      |          |         | Summary data on each RCT, number of cases in treat, number of cases in control etc.  |
| UN World Population Prospects, 2019 revision | `data/raw/weighted_mr/WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.xlsx` | Yes | United Nations, Department of Economic and Social Affairs, Population Division ([2019]). World Population Prospects [2019], archive. | Copyright © 1992-2022 by United Nations [![License: CC BY 3.0 IGO](https://img.shields.io/badge/License-CC_BY_3.0_IGO-lightgrey.svg)](http://creativecommons.org/licenses/by/3.0/igo/) | Downloaded from [https://population.un.org/](https://population.un.org/wpp/Download/Archive/Standard/) on on Apr 5, 2023|
| UNdata M49 Country Codes | `data/raw/u5-per-hh/UNSD.csv` | Yes | | Terms of use available at [https://data.un.org/](https://data.un.org/Host.aspx?Content=UNdataUse) | Copied from [https://unstats.un.org/](https://unstats.un.org/unsd/methodology/m49/) on on Apr 5, 2023 |
| Studies microdata | Files in `data/raw/mortality_counts` | No       |          |         | This data was acquired directly with the authors of each paper included in the study. |
| DHS data | `data/raw/u5-per-hh/idhs_00001` | No | Elizabeth Heger Boyle, Miriam King and Matthew Sobek. IPUMS-Demographic and Health Surveys: Version 9 [dataset]. IPUMS and ICF, 2022. https://doi.org/10.18128/D080.V9 | | Accessed through [https://dhsprogram.com/data/Access-Instructions.cfm] on Apr 5, 2023 |
| IHME Diarrhea Prevalence | `data/raw/IHME_GLOBAL_DIARRHEA_2000_2019_PREV_A1_S3_ADMIN_1_Y2020M08D31.CSV` | No | Institute for Health Metrics and Evaluation (IHME). Global Under-5 Diarrhea Incidence, Prevalence, and Mortality Geospatial Estimates 2000-2019. Seattle, United States of America: Institute for Health Metrics and Evaluation (IHME), 2020. |Terms of use can be found [here](https://www.healthdata.org/Data-tools-practices/data-practices/ihme-free-charge-non-commercial-user-agreement) | Downloaded from [https://ghdx.healthdata.org/](https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GLOBAL_DIARRHEA_2000_2019_DATA_INPUT_SOURCES_Y2020M08D31_0.XLSX)) on on Jul 28, 2023|
| World Bank region and income groups | `data/raw/weighted_mr/CLASS.xlsx` | Yes | World Bank: World Bank Country and Lending Groups. Downloaded on April 5, 2023. |[![License: CC BY 4.0 IGO](https://img.shields.io/badge/License-CC_BY_4.0_IGO-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/) Detailed terms of use for data is available [here](https://www.worldbank.org/en/about/legal/terms-of-use-for-datasets) | Downloaded from [https://datahelpdesk.worldbank.org](https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups) on Apr 5, 2023 |
| | `data/raw/weighted_mr/all_data.csv` | | | | |
| WHO/UNICEF Joint Monitoring Programme for Water Supply, Sanitation and Hygiene (JMP) - Household data | `data/raw/weighted_mr/JMP_2021_WLD.xlsx` | Yes | Progress on household drinking water, sanitation and hygiene 2000–2022: special focus on gender. New York: United Nations Children’s Fund (UNICEF) and World Health Organization (WHO), 2023. | [![License: CC BY 4.0 IGO](https://img.shields.io/badge/License-CC_BY_4.0_IGO-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)| Downloaded from [https://washdata.org](https://washdata.org/data/country/WLD/household/download) on on Apr 5, 2023 |
| WHO/UNICEF Joint Monitoring Programme for Water Supply, Sanitation and Hygiene (JMP) - Access to drinking water | `data/raw/weighted_mr/washdash-download.csv` | Yes | Progress on household drinking water, sanitation and hygiene 2000–2022: special focus on gender. New York: United Nations Children’s Fund (UNICEF) and World Health Organization (WHO), 2023. |[![License: CC BY 4.0 IGO](https://img.shields.io/badge/License-CC_BY_4.0_IGO-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/) | Downloaded from [https://washdata.org/](https://washdata.org/data/household#!/table?geo0=region&geo1=sdg) on Apr 5, 2023 |

 Intermediate data sets created during data processing are stored in `data/transformed`. The final data sets used for analysis or to create exhibits in the paper are stored in `data/final` and listed below

| File                        | Created by | Provided | Citation |
| --------------------------- | ---------- | -------- | -------- |
| `data/final/diarrhea_studies.rds` | `code/wrangling/clean_studies_list.R` | Yes | |
| `data/final/ma_datasets.Rdata`    | `code/wrangling/prep_adjusted_data.R` | Yes | |
| `data/final/mortality_rate.rds`   | `code/cea/weighted-mr.R` | Yes | |
| `data/final/individual_data_anonymised.rds`  | `code/individual_data_anonymised/pre_individual_data.R` | Yes | |
