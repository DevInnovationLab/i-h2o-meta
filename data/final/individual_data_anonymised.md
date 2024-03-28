
- Number of observations: 22,136
- Number of variables: 13
- Disk size: 3 MB
- Last saved: 2024-03-27 20:13:18


## Variable type: character
|skim_variable        | n_missing| complete_rate| min| max| empty| n_unique| whitespace|
|:--------------------|---------:|-------------:|---:|---:|-----:|--------:|----------:|
|age_measure_original |         0|     1.0000000|   4|   6|     0|        4|          0|
|study                |         0|     1.0000000|  17|  22|     0|       11|          0|
|precision_trt        |         7|     0.9996838|   3|   7|     0|        4|          0|
|precision_evt        |         0|     1.0000000|   3|   7|     0|        5|          0|
|precision_fut        |         7|     0.9996838|   3|   7|     0|        4|          0|

## Variable type: Date
|skim_variable  | n_missing| complete_rate|min        |max        |median     | n_unique|
|:--------------|---------:|-------------:|:----------|:----------|:----------|--------:|
|treatment_time |       261|     0.9882093|2001-08-01 |2019-07-23 |2013-07-01 |      252|
|event_time_lb  |     21817|     0.0144109|2001-08-01 |2018-12-01 |2012-11-26 |      105|
|follow_up_time |       462|     0.9791290|2001-09-19 |2019-06-30 |2015-09-07 |      413|

## Variable type: numeric
|skim_variable | n_missing| complete_rate|         mean|           sd|      p0|        p25|        p50|        p75|       p100|hist  |
|:-------------|---------:|-------------:|------------:|------------:|-------:|----------:|----------:|----------:|----------:|:-----|
|death         |         0|     1.0000000| 1.441090e-02| 1.191800e-01|       0|          0|          0|          0|          1|▇▁▁▁▁ |
|wtreatment    |         0|     1.0000000| 4.555023e-01| 4.980273e-01|       0|          0|          0|          1|          1|▇▁▁▁▇ |
|crossed_five  |        15|     0.9993224| 7.345960e-02| 2.608953e-01|       0|          0|          0|          0|          1|▇▁▁▁▁ |
|cluster_id    |      7781|     0.6484911| 2.101452e+09| 1.218895e+09| 3231856| 1084261665| 2052755516| 3158532144| 4292269349|▇▇▇▇▇ |

## Variable type: POSIXct
|skim_variable | n_missing| complete_rate|min        |max        |median     | n_unique|
|:-------------|---------:|-------------:|:----------|:----------|:----------|--------:|
|event_time_ub |     21817|     0.0144109|2001-11-10 |2018-12-31 |2015-01-19 |      126|
