
- Number of observations: 90
- Number of variables: 42
- Disk size: 110 KB
- Last saved: 2024-04-02 00:14:26.103819


## Variable type: character
|skim_variable                                                    | n_missing| complete_rate| min| max| empty| n_unique| whitespace|
|:----------------------------------------------------------------|---------:|-------------:|---:|---:|-----:|--------:|----------:|
|reference                                                        |         0|     1.0000000|  19| 574|     0|       83|          0|
|trial_name                                                       |        70|     0.2222222|  17|  22|     0|       20|          0|
|country                                                          |         0|     1.0000000|   4|  21|     0|       43|          0|
|setting                                                          |         0|     1.0000000|   5|   5|     0|        3|          0|
|study_year                                                       |         7|     0.9222222|   1|   9|     0|       37|          0|
|participants                                                     |         9|     0.9000000|   8|  16|     0|        4|          0|
|intervention                                                     |         1|     0.9888889|  12|  95|     0|       36|          0|
|duration_of_intervention                                         |         1|     0.9888889|   1|   3|     0|       30|          0|
|intervention_combined_with_hygiene_and_or_sanitation_component   |        10|     0.8888889|   2|   3|     0|        2|          0|
|baseline_water_water_in_control_group                            |         1|     0.9888889|   8|  10|     0|        2|          0|
|provided_safe_water_storage                                      |        10|     0.8888889|   2|   3|     0|        2|          0|
|how_is_compliance_defined                                        |        46|     0.4888889|   3| 540|     0|       40|          0|
|study_evaluating_a_specific_intervention_vs_survey_data_analyses |         9|     0.9000000|  20|  40|     0|        2|          0|
|transition_according_to_figure_1_and_table_2                     |        10|     0.8888889|   1|   1|     0|       11|          0|
|number_of_participants                                           |         3|     0.9666667|  12| 226|     0|       85|          0|
|notes                                                            |        66|     0.2666667|  46| 127|     0|       24|          0|
|diarrhea_prevalence_in_baseline_control_arm                      |        60|     0.3333333|   4| 184|     0|       30|          0|
|baseline_contamination_of_water                                  |        76|     0.1555556|  16|  74|     0|       13|          0|
|notes_2                                                          |        78|     0.1333333|  18| 213|     0|       12|          0|
|included                                                         |         0|     1.0000000|   8|   8|     0|        2|          0|
|intervention_group                                               |         0|     1.0000000|  10|  31|     0|        8|          0|
|ref_first_word                                                   |         0|     1.0000000|   2|  16|     0|       69|          0|
|shorthand_ref                                                    |         0|     1.0000000|   9|  23|     0|       83|          0|
|income_group                                                     |         1|     0.9888889|  10|  19|     0|        4|          0|

## Variable type: logical
|skim_variable | n_missing| complete_rate|      mean|count            |
|:-------------|---------:|-------------:|---------:|:----------------|
|chlor         |         1|     0.9888889| 0.3033708|FAL: 62, TRU: 27 |
|rural         |         0|     1.0000000| 0.6666667|TRU: 60, FAL: 30 |
|unimproved    |         1|     0.9888889| 0.7078652|TRU: 63, FAL: 26 |
|chlorination  |         0|     1.0000000| 0.3111111|FAL: 62, TRU: 28 |
|filtration    |         0|     1.0000000| 0.2111111|FAL: 71, TRU: 19 |
|community     |         0|     1.0000000| 0.0111111|FAL: 89, TRU: 1  |
|in_wolf_et_al |         0|     1.0000000| 0.8888889|TRU: 80, FAL: 10 |

## Variable type: numeric
|skim_variable                        | n_missing| complete_rate|       mean|        sd|         p0|        p25|        p50|        p75|       p100|hist  |
|:------------------------------------|---------:|-------------:|----------:|---------:|----------:|----------:|----------:|----------:|----------:|:-----|
|mortality_reported                   |         0|     1.0000000|  0.2222222| 0.4180688|  0.0000000|  0.0000000|  0.0000000|  0.0000000|  1.0000000|▇▁▁▁▂ |
|effect_estimate_on_diarrhea          |         1|     0.9888889|  0.6964901| 0.2688445|  0.1600000|  0.5200000|  0.7000000|  0.9000000|  1.3700000|▃▇▆▅▂ |
|compliance_rate                      |        44|     0.5111111|  0.6708913| 0.2462765|  0.1400000|  0.4075000|  0.7250000|  0.8700000|  1.0000000|▂▃▁▅▇ |
|lower_95_percent_confidence_interval |         1|     0.9888889|  0.4908771| 0.2531450|  0.0000000|  0.2900000|  0.4800000|  0.6500000|  1.2800000|▅▇▆▃▁ |
|upper_95_percent_confidence_interval |         1|     0.9888889|  1.2574363| 1.4177010|  0.3000000|  0.8000000|  0.9900000|  1.1800000| 10.9700000|▇▁▁▁▁ |
|quality_rating                       |        10|     0.8888889|  0.3666250| 0.1687703|  0.0000000|  0.2500000|  0.3300000|  0.5000000|  0.8300000|▁▇▃▅▁ |
|included_dummy                       |         0|     1.0000000|  0.2000000| 0.4022409|  0.0000000|  0.0000000|  0.0000000|  0.0000000|  1.0000000|▇▁▁▁▂ |
|se_imp_upper                         |         1|     0.9888889|  0.2413275| 0.2653318|  0.0221863|  0.1165992|  0.1729466|  0.2798806|  2.1570131|▇▁▁▁▁ |
|se_imp_lower                         |         1|     0.9888889|        Inf|          | -0.0415029|  0.1155753|  0.1681016|  0.2816676|        Inf|▇▅▂▁▁ |
|se_imp                               |         1|     0.9888889|        Inf|          |  0.0226907|  0.1204481|  0.1739729|  0.2802582|        Inf|▇▅▁▁▁ |
|ln_RR                                |         1|     0.9888889| -0.4503875| 0.4528215| -1.8325815| -0.6539265| -0.3566749| -0.1053605|  0.3148107|▁▁▅▇▅ |
