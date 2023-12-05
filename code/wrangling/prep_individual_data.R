library(readxl)
library(haven)
library(tidyverse)
library(digest)
library(here)

# (0) Functions for data processing and checks =================================

# Generate variables for final data-set
gen_vars <- function(df, study, age_original) {
  df_gen_vars <- df %>%
    mutate(age_measure_original = age_original,
           study = study)
  return(df_gen_vars)
}

# Functions for generating a unique cluster id

# Convert from base-16 to base-10
# see: https://stackoverflow.com/questions/27442991/how-to-get-a-hash-code-as-integer-in-r
hex_to_int <- function(h) {
  xx = strsplit(tolower(h), "")[[1L]]
  pos = match(xx, c(0L:9L, letters[1L:6L]))
  sum((pos - 1L) * 16 ^ (rev(seq_along(xx) - 1)))
}

# Use hash function to map papers + cluster number string to a unique
# cluster number
get_cluster <- function(str) {
  return(hex_to_int(digest(str, algo = 'xxhash32')))
}


check_duplicate_ids = function(data, ...) {
  n_non_unique = data %>%
    group_by(...) %>%
    count() %>%
    dplyr::filter(n > 1) %>%
    nrow()
  
  if (n_non_unique > 0) {
    stop("There are duplicate IDs in the data.")
  }
}

# (1) Reading all papers =======================================================

## Boisson et al ---------------------------------------------------------------
deaths_boisson <-
  read_xlsx(
    "data/raw/mortality_counts/Boisson et al. 2013/Aquatab deaths.xlsx",
    sheet = "deaths"
  )

data_boisson <-
  read_dta("data/raw/mortality_counts/Boisson et al. 2013/Aquatab_INDIA10.dta")

## Dupas et al ---------------------------------------------------------------
data_dupas <-
  read_dta("data/raw/mortality_counts/Dupas et al. 2021/child_analysis_nopii.dta")

dupas_child_key_cw_df = read_dta("data/raw/mortality_counts/Dupas et al. 2021/dupas_crosswalk_child_key_id.dta")

## Kirby et al ---------------------------------------------------------------
deaths_kirby <-
  read_xlsx("data/raw/mortality_counts/Kirby et al. 2019/mortality_data.xlsx")

data_kirby <-
  read_xlsx("data/raw/mortality_counts/Kirby et al. 2019/Rwanda_TN_cRCT_blfu_child.xlsx")

## Kremer et al. 2012 ----------------------------------------------------------

data_kremer <-
  read_dta("data/raw/mortality_counts/Kremer et al. 2011/data_clean.dta")

## Chiller et al ---------------------------------------------------------------

data_chiller_id <-
  read_xls("data/raw/mortality_counts/Chiller et al. 2006/listaControl.xls",
           sheet = "listaControl")

data_chiller <-
  read_xlsx("data/raw/mortality_counts/Chiller et al. 2006/Chiller incidence data for Ricardo.xlsx",
            sheet = "Vigfinal")
## Crump et al. 2005 -----------------------------------------------------------

data_crump <-
  read_xlsx(
    "data/raw/mortality_counts/Crump et al. 2005/WEEKLYSURVEILLANCE.xlsx",
    sheet = "WEEKLYSURVEILLANCE"
  )

## Luby et al. 2006 ------------------------------------------------------------

data_luby2006 <-
  read.csv(
    "data/raw/mortality_counts/Luby et al. 2006/Child plus HH plus cluster plus group Floc Health for Ricardo.csv"
  )

deaths_luby2006 <-
  read_xls(
    "data/raw/mortality_counts/Luby et al. 2006/FlocHealthDeaths.xls",
    sheet = "Sheet1"
  )

## Reller et al. 2003 ----------------------------------------------------------

data_reller <-
  read.csv(
    "data/raw/mortality_counts/Reller et al. 2003/Diarrhea Incidence Reller paper Reduced for Ricardo.csv"
  )

## Peletz et al. 2012 ----------------------------------------------------------

data_peletz <-
  read_dta("data/raw/mortality_counts/Peletz et al. 2012/Zambia data 7Feb19 short.dta")

## Null et al. 2018 - no age variables -----------------------------------------

data_null <-
  read_dta("data/raw/mortality_counts/Null et al. 2018/washb-kenya-diar-public.dta")

data_null_death <-
  read_dta("data/raw/mortality_counts/Null et al. 2018/washb-kenya-mortality-public.dta")

## Haushofer et al. 2020 -------------------------------------------------------
data_haush <-
  read_dta("data/raw/mortality_counts/Haushofer et al. 2021/kremer_martens_data.dta")


# (2) Data processing ==========================================================

## Boisson et al. 2013 ---------------------------------------------------------

deaths_boisson <- deaths_boisson %>%
  dplyr::filter(`<5` == "yes") %>%
  mutate(death = 1)

mortality_boisson <- data_boisson %>%
  group_by(indid) %>%
  # Age variables are age reported at the baseline/ first follow-up
  # for those missed in the baseline census
  summarise(
    ageyr = min(ageyr),
    # age of those included in baseline
    agereported = min(agereported),
    # age of those missed at baseline but included in follow-up + baseline
    newborn = mean(newborn, na.rm = T),
    agemth_end = max(agemth),
    agemth_survey0 = min(agemth),
    group = mean(group),
    follow_up_time = max(date, na.rm = TRUE),
    survey0_time = min(date, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  left_join(deaths_boisson) %>%
  mutate(
    death = ifelse(is.na(death), 0, 1),
    treatment_time = ymd("2010-11-01"),
    follow_up_time = ymd(follow_up_time),
    event_time_lb = case_when(
      indid == 122030501 ~ ymd("2011-02-10"),
      indid == 122170507 ~ ymd("2011-10-14"),
      indid == 245130801 ~ ymd("2011-01-19")
    ),
    event_time_ub = case_when(
      indid == 122030501 ~ ymd("2011-03-18"),
      indid == 122170507 ~ ymd("2011-11-29"),
      indid == 245130801 ~ ymd("2011-02-17")
    ),
    precision_fut = "day",
    precision_trt = "month",
    precision_evt = "month",
    age_month = agemth_survey0 - as.numeric(survey0_time - treatment_time) /
      30,
    age_year = ifelse(is.na(age_month), agereported, age_month / 12),
    age_month = ifelse(is.na(age_month), age_year * 12, age_month),
    crossed_five = ifelse(
      agereported < 5 &
        agereported + interval(treatment_time, follow_up_time) %/% years(1) >=
        5,
      1,
      0
    )
  ) %>%
  rename(wtreatment = group) %>%
  add_column(cluster_id = NA) %>%
  gen_vars("Boisson et al., 2013", "month")

## Dupas et al. 2021 ------------------------------------------------------------

# In data_dupas, if child_key is blank (== "") it seems that key has the same
# structure as child_key: "uuid:blablaSurveyCTOStuff/modeule-hhroster-hhchildren[$childid]"
# so we just harmonise keys based off that and then merge in the crosswalk df saved
# on Dupas' website: https://web.stanford.edu/~pdupas/MSW%20Public.zip
data_dupas = data_dupas %>%
  mutate(tmp_key = if_else(child_key == "", key, child_key)) %>%
  left_join(
    dupas_child_key_cw_df %>%
      select(key, child_id) %>%
      mutate(key_match = "match"),
    by = c("tmp_key" = "key")
  )

# 6 rows don't match that seem to be empty
# Pascaline!!!
data_dupas %>%
  dplyr::filter(is.na(key_match))
check_duplicate_ids(mortality_boisson, indid)

mortality_dupas = data_dupas %>%
  dplyr::filter(!is.na(key_match)) %>%
  select(
    id,
    child_position,
    childmob,
    coupon,
    wash_effect,
    fd_effect,
    ration_effect,
    bl_childage_years,
    child_gone,
    sdate,
    monthofdeath,
    child_id
  ) %>%
  mutate(death = ifelse(child_gone == 3, 1, 0),
         death = ifelse(is.na(death), 0, death)) %>%
  dplyr::filter(!((
    fd_effect == 1 | ration_effect == 1 |
      wash_effect == 1
  ) & coupon != 1)) %>%
  # group_by(id, coupon, fd_effect, ration_effect) %>% # Took out grouping by wash_effect
  group_by(child_id) %>%
  summarise(
    death = max(death),
    monthofdeath = ifelse(death == 1, max(monthofdeath, na.rm =
                                            T), NA),
    # I think bl_childage_years takes the min of child in each households across surveys
    # bl_child_years is the same for every household
    bl_childage_years = min(bl_childage_years),
    # Taking the first survey date - baseline survey (?)
    sdate = min(sdate),
    coupon = unique(coupon)
  ) %>%
  rename(wtreatment = coupon, age_year = bl_childage_years) %>%
  mutate(
    monthofdeath = as.Date(monthofdeath, origin = "1970-01-01"),
    event_time_lb = monthofdeath,
    event_time_ub = event_time_lb + ifelse(month(monthofdeath) == 4 |
                                             month(monthofdeath) == 6, 29, 30),
    event_time_ub = as.Date(event_time_ub, origin = "1970-01-01"),
    age_month = age_year * 12,
    # Line below accounts for first survey occurring post-treatment
    treatment_time = ymd(sdate + ifelse(
      !is.na(age_month) & age_month < 0, age_month * 30, 0
    )),
    follow_up_time = ymd("2019-06-30"),
    precision_fut = "month",
    precision_trt = "day",
    precision_evt = "month",
    crossed_five = ifelse(
      age_year < 5 &
        age_year + interval(treatment_time, follow_up_time) %/% years(1) >=
        5,
      1,
      0
    )
  ) %>%
  add_column(cluster_id = NA) %>%
  gen_vars("Dupas et al., 2021", "years")

## Kirby et al. 2019 ---------------------------------------------------------
# Event date unknown
# age at death not clear from data
# Since data was collected over several rounds
# For now, keeping the max age across all rounds

deaths_kirby <- deaths_kirby %>%
  rename(a0 = assignment,
         a5 = cluster_id) %>%
  group_by(child_id) %>%
  mutate(max = max(surveyround)) %>%
  # Keeping the last survey round - assuming here that the
  # event was recorded in the last survey round (even if it was
  # recorded before that)
  dplyr::filter(!surveyround < max) %>%
  ungroup() %>%
  select(a0, a5, child_id, surveydate) %>%
  mutate(death = 1)

mortality_kirby <- data_kirby %>%
  group_by(childid) %>%
  mutate(
    age_survey0 = min(dobc),
    survey0 = min(round),
    max = max(round)
  ) %>%
  # Keeping the last survey round
  dplyr::filter(!round < max) %>%
  ungroup() %>%
  select(childid, a0, a5, dobc, ch13c, survey0, age_survey0) %>%
  rename(child_id = childid) %>%
  left_join(deaths_kirby) %>%
  mutate(
    death = ifelse(!is.na(death), 1, 0),
    wtreatment = ifelse(a0 == "control", 0, 1),
    # Time of last follow-up
    follow_up_time = ymd("2016-03-31"),
    precision_fut = "month",
    precision_trt = "quarter",
    precision_evt = "year",
    # Treatment time - end of baseline enrollment
    # Specific date of intervention not provided in the data
    treatment_time = ymd("2014-11-01"),
    # Children aged out
    survey0_time = case_when (
      survey0 == 0 ~ treatment_time,
      survey0 == 1 ~ ymd("2015-04-01"),
      survey0 == 2 ~ ymd("2015-08-01"),
      survey0 == 3 ~ ymd("2016-01-01")
    ),
    age_month = age_survey0 - as.numeric(survey0_time - treatment_time) /
      30,
    crossed_five = ifelse(
      age_month < 60 &
        age_month + interval(treatment_time, follow_up_time) %/% months(1) >=
        60,
      1,
      0
    ),
    age_end = age_month + interval(treatment_time, follow_up_time) %/% months(1),
    age_year = age_month / 12,
    event_time_lb = treatment_time  # This is a very rough estimate...
  ) %>%
  rename(event_time_ub = surveydate, cluster_id = a5) %>%
  gen_vars("Kirby et al., 2019", "month")

mortality_kirby$event_time_lb[which(mortality_kirby$death == 0)] = NA

mortality_kirby$cluster_id <-
  as.character(mortality_kirby$cluster_id)
mortality_kirby$cluster_id <-
  sapply(mortality_kirby$cluster_id, function(x)
    paste('Kirby et al., 2019', x))
mortality_kirby$cluster_id <-
  sapply(mortality_kirby$cluster_id, get_cluster)

## Kremer et al. 2011 ---------------------------------------------------------

mortality_kremer <- data_kremer %>%
  # Not including treatment group 2 (treatment received in second year?)
  dplyr::filter(t05 != 1) %>%
  select(spring_id,
         child_death_dummy,
         age_base,
         evertreat,
         date_interview) %>%
  mutate(
    age_month = age_base * 12,
    # treatment time - springs were protected from Jan-April 2005
    # follow-up time - first round from April-August 2005
    treatment_time = ymd("2005-03-01"),
    follow_up_time = ymd("2007-03-31"),
    event_time_lb = treatment_time,
    # Unclear how to get this!
    event_time_ub = ymd("2005-08-31"),
    precision_fut = "quarter",
    precision_trt = "quarter",
    precision_evt = "quarter",
    crossed_five = ifelse(
      age_base < 5 &
        age_base + interval(treatment_time, follow_up_time) %/% years(1) >=
        5,
      1,
      0
    ),
    spring_id = as.character(spring_id)
  ) %>%  # How to get age at end of study?
  rename(
    wtreatment = evertreat,
    death = child_death_dummy,
    age_year = age_base,
    cluster_id = spring_id
  ) %>%
  gen_vars("Kremer et. al., 2011", "years")

mortality_kremer$event_time_lb[which(mortality_kremer$death == 0)] = NA
mortality_kremer$event_time_ub[which(mortality_kremer$death == 0)] = NA

mortality_kremer$cluster_id <-
  sapply(mortality_kremer$cluster_id, function(x)
    paste('Kremer et al., 2011', x))
mortality_kremer$cluster_id <-
  sapply(mortality_kremer$cluster_id, get_cluster)

## Chiller et al. 2006 --------------------------------------------------------

# Note: For 103 observations, where the dob was missing
# For those, the EDAD variable is still there and the
# EDADAC variable takes the value 0. (05/24/2022) -
# SN is replacing EDADAC with EDAD for these cases

data_chiller_id <- data_chiller_id %>%
  select(MertuID, Cluster) %>%
  rename(Mertuid = MertuID)

mortality_chiller <- data_chiller %>%
  left_join(data_chiller_id) %>%
  mutate(village = substr(Mertuid, 1, 2)) %>%
  select(PersonaID, Cluster, GRUPO, FECNAC, FECENT, village) %>%
  group_by(PersonaID, village, Cluster, GRUPO) %>%
  summarise(dob = max(FECNAC), follow_up_time = max(FECENT)) %>%
  mutate(
    death = ifelse(PersonaID == "143245", 1, 0),
    treatment = case_when(GRUPO == "CC" ~ 0,
                          GRUPO == "CK" ~ 1),
    # Treatment time
    treatment_time = "2002-11-04",
    treatment_time = ymd(treatment_time),
    # Follow-up time - 13 weeks from intervention
    follow_up_time = ymd("2003-01-31"),
    precision_fut = "day",
    precision_trt = "week",
    precision_evt = "year",
    # Event time (best guess is just range of follow-up times)
    event_time_lb = case_when(PersonaID == "143245" ~ ymd("2002-11-04")),
    event_time_ub = case_when(PersonaID == "143245" ~ ymd("2003-01-31")),
    # Age
    age_month = as.numeric(treatment_time - ymd(dob)) / 30,
    age_year = age_month / 12,
    # crossed the age of 5
    crossed_five = ifelse(
      age_month < 60 &
        age_month + interval(treatment_time, follow_up_time)
      %/% months(1) >= 60,
      1,
      0
    ),
  ) %>%
  dplyr::filter(!is.na(treatment)) %>%
  rename(wtreatment = treatment, cluster_id = Cluster) %>%
  gen_vars("Chiller et al., 2006", "months")

mortality_chiller$cluster_id <-
  as.character(mortality_chiller$cluster_id)
mortality_chiller$cluster_id <-
  sapply(mortality_chiller$cluster_id, function(x)
    paste('Chiller et al., 2006', x))
mortality_chiller$cluster_id <-
  sapply(mortality_chiller$cluster_id, get_cluster)

## Crump et al. 2005 ----------------------------------------------------------- 

remove <- data_crump %>%
  dplyr::filter(is.na(errorflag1)) %>%
  dplyr::filter(tw2week == "01" & tw2away == 4) %>%
  select(tw2seq) %>%
  distinct() %>%
  mutate(remove = 1)

temp <- data_crump %>%
  # Cleaning from Brandon's do-file
  dplyr::filter(is.na(errorflag1)) %>%
  dplyr::filter(tw2age < 5) %>%
  left_join(remove) %>%
  dplyr::filter(is.na(remove)) %>%
  select(-remove) %>%
  mutate(
    compound = substr(tw2seq, 0, 6),
    die = ifelse(tw2away == 4, 1, 0),
    tw2date = str_replace(tw2date, "2008-", "2003-"),
    # Typo in raw data (?)
    event_time = ifelse(die == 1, as.Date(tw2date), NA),
  ) %>%
  group_by(tw2seq, compound, tw2studyarm) %>%
  summarize(
    death = max(die),
    minage = min(tw2age),
    event_time_ub = ifelse(death == 1, min(event_time, na.rm = TRUE), NA),
    event_time_lb = as.Date(event_time_ub - 7, origin = "1970-01-01"),
    event_time_ub = as.Date(event_time_ub, origin = "1970-01-01"),
    follow_up_time = max(as.Date(tw2date, origin = "1970-01-01")),
    mintime = min(as.Date(tw2date, origin = "1970-01-01"))
  ) %>%
  ungroup() %>%
  dplyr::filter(
    tw2seq != "015086G005",
    tw2seq != "157021A011",
    tw2seq != "009022B004",
    tw2seq != "015080D006",
    tw2seq != "123140E004",
    tw2seq != "149001B004",
    tw2seq != "102052D003",
    tw2seq != "010129A014",
    tw2seq != "111036C008"
  ) %>%
  select(tw2seq,
         death,
         minage,
         mintime,
         event_time_ub,
         event_time_lb,
         follow_up_time)

mortality_crump <- data_crump %>%
  dplyr::filter(is.na(errorflag1)) %>%
  dplyr::filter(tw2age < 5) %>%
  left_join(remove) %>%
  dplyr::filter(is.na(remove)) %>%
  dplyr::filter(
    tw2seq != "015086G005",
    tw2seq != "157021A011",
    tw2seq != "009022B004",
    tw2seq != "015080D006",
    tw2seq != "123140E004",
    tw2seq != "149001B004",
    tw2seq != "102052D003",
    tw2seq != "010129A014",
    tw2seq != "111036C008"
  ) %>%
  select(tw2seq, tw2v, tw2c, tw2h, i, tw2studyarm) %>%
  distinct() %>%
  left_join(temp) %>%
  mutate(die = ifelse(is.na(death), 0, death)) %>%
  mutate(compound = paste0(tw2v, tw2c)) %>%
  select(
    tw2v,
    tw2c,
    die,
    compound,
    tw2studyarm,
    minage,
    mintime,
    event_time_ub,
    event_time_lb,
    follow_up_time
  ) %>%
  #Combining into one treatment group
  mutate(tw2studyarm = case_when(tw2studyarm == 3 ~ 1,
                                 tw2studyarm == 2 ~ 1,
                                 tw2studyarm == 1 ~ 0)) %>%
  rename(wtreatment = tw2studyarm,
         death = die) %>%
  mutate(
    treatment_time = ymd("2003-04-08"),
    precision_fut = "week",
    precision_trt = "month",
    precision_evt = "week",
    age_month = minage * 12 - as.numeric(mintime - treatment_time) / 30,
    age_year = age_month / 12,
    # crossed the age of 5
    crossed_five = ifelse(
      age_year < 5 &
        age_year + interval(treatment_time, follow_up_time) %/% years(1) > 5,
      1,
      0
    )
  ) %>%
  mutate(cluster_id = compound) %>%
  gen_vars("Crump et al., 2005", "year")

mortality_crump$event_time_lb[which(mortality_crump$death == 0)] = NA
mortality_crump$event_time_ub[which(mortality_crump$death == 0)] = NA

# Fill in NA follow up times as maximum follow up time among excluded observations
# in the same compound
mortality_crump$follow_up_time[which(mortality_crump$compound == "223044")] =
  ymd("2003-06-05")
mortality_crump$follow_up_time[which(mortality_crump$compound == "111041")] =
  ymd("2003-09-20")
mortality_crump$follow_up_time[which(mortality_crump$compound == "003036")] =
  ymd("2003-09-19")

mortality_crump <- mortality_crump %>%
  dplyr::filter(age_year < 5 | is.na(age_year))

table(mortality_crump$wtreatment, mortality_crump$death)

mortality_crump$cluster_id <-
  as.character(mortality_crump$cluster_id)
mortality_crump$cluster_id <-
  sapply(mortality_crump$cluster_id, function(x)
    paste('Crump et al., 2005', x))
mortality_crump$cluster_id <-
  sapply(mortality_crump$cluster_id, get_cluster)

## Luby et al. 2006 -----------------------------------------------------------

deaths_luby2006 <- deaths_luby2006 %>%
  mutate(ChildID = as.numeric(ChildID)) %>%
  rename(ChildId = ChildID,
         DOB_death = DOB)

mortality_luby2006 <- data_luby2006 %>%
  mutate(
    DOB = str_replace_all(DOB, "/6", "/196"),
    DOB = str_replace_all(DOB, "/5", "/195"),
    DOB = str_replace_all(DOB, "/4", "/194"),
    DOB = str_replace_all(DOB, "/3", "/193"),
    DOB = str_replace_all(DOB, "/196/", "/6/"),
    DOB = str_replace_all(DOB, "/195/", "/5/"),
    DOB = str_replace_all(DOB, "/194/", "/4/"),
    DOB = str_replace_all(DOB, "/193/", "/3/"),
    DOB = str_replace_all(DOB, "/1930/", "/30/"),
    DOB = str_replace_all(DOB, "/1931/", "/31/"),
    DOB = mdy(DOB)
  ) %>%
  left_join(deaths_luby2006) %>%
  mutate(
    treatment = ifelse(Group != "X" & Group != "S", 1, 0),
    has_soap = ifelse(Group == "FS" | Group == "S", 1, 0)
  ) %>%
  dplyr::filter(has_soap == 0) %>%
  rename(
    wtreatment = treatment,
    age_year = AGE,
    event_time = `Date of Death`,
    cluster_id = Child.plus.HH.plus.cluster_ClusterId
  ) %>%
  mutate(
    death = ifelse(!is.na(event_time), 1, 0),
    treatment_time = "2003-04-01",
    treatment_time = ymd(treatment_time),
    event_time_lb = ymd(event_time),
    event_time_ub = event_time_lb,
    age_month = age_year * 12,
    follow_up_time = ymd("2003-12-22"),
    precision_fut = "week",
    precision_trt = "month",
    precision_evt = "day",
    follow_up_age = follow_up_time - DOB,
    follow_up_age = as.numeric(follow_up_age) / 365,
    crossed_five = ifelse(follow_up_age > 5, 1, 0)
  ) %>%
  gen_vars("Luby et al., 2006", "years")

mortality_luby2006$cluster_id <-
  as.character(mortality_luby2006$cluster_id)
mortality_luby2006$cluster_id <-
  sapply(mortality_luby2006$cluster_id, function(x)
    paste('Luby et al., 2006', x))
mortality_luby2006$cluster_id <-
  sapply(mortality_luby2006$cluster_id, get_cluster)

## Reller et al. 2003 ----------------------------------------------------------

data_reller <- data_reller %>%
  rename(strata = localidad,
         clust_level = mertuid,
         childid = personaid) %>%
  mutate(wtreatment = ifelse(grupo != "CC", 1, 0)) %>%
  arrange(childid) %>%
  group_by(childid) %>%
  mutate(max_under5 = max(Under5)) %>%
  ungroup() %>%
  dplyr::filter(max_under5 == 1 | is.na(max_under5)) %>%
  arrange(childid) %>%
  group_by(childid) %>%
  summarise(
    wtreatment = first(wtreatment),
    strata = first(strata),
    clust_level = first(clust_level),
    grupo = first(grupo),
    edad = mean(edad),
    edadac = max(edadac),
    agegroup = max(agegroup),
    follow_up_time = max(mdy(fecent)),
    fecnac = max(fecnac)
  ) %>%
  arrange(clust_level) %>%
  group_by(clust_level) %>%
  mutate(child_order = row_number()) %>%
  ungroup()

data_reller <- data_reller %>%
  mutate(
    # Children <5 with diarrhea data
    death = case_when(
      clust_level == "07054B" & child_order == 1 ~ 1,
      clust_level == "11282" & child_order == 1 ~ 1,
      clust_level == "12348" & child_order == 1 ~ 1,
      clust_level == "04024" & child_order == 1 ~ 1,
      clust_level == "12338" & child_order == 1 ~ 1,
      clust_level == "09145C" & child_order == 1 ~ 1,
      clust_level == "09234" & child_order == 1 ~ 1,
      clust_level == "09175" & child_order == 1 ~ 1,
      clust_level == "22704" & child_order == 1 ~ 1,
      clust_level == "12134" & child_order == 1 ~ 1
    ),
    event_time_ub = case_when(
      clust_level == "07054B" & child_order == 1 ~ "11/10/2001",
      clust_level == "11282" & child_order == 1 ~ "11/16/2001",
      clust_level == "12348" & child_order == 1 ~ "2/15/2002",
      clust_level == "04024" & child_order == 1 ~ "5/24/2002",
      clust_level == "12338" & child_order == 1 ~ "",
      clust_level == "09145C" & child_order == 1 ~ "7/13/2002",
      clust_level == "09234" & child_order == 1 ~ "7/17/2002",
      clust_level == "09175" & child_order == 1 ~ "8/1/2002",
      clust_level == "22704" & child_order == 1 ~ "8/17/2002",
      clust_level == "12134" & child_order == 1 ~ ""
    ),
    event_time_lb = event_time_ub,
    death = ifelse(is.na(death), 0, death),
    follow_up_time = ifelse(
      is.na(follow_up_time) & death == 1,
      max(follow_up_time),
      follow_up_time
    ),
    follow_up_time = as.Date(follow_up_time, origin = "1970-01-01"),
    fecnac = str_replace_all(fecnac, "/6", "/196"),
    fecnac = str_replace_all(fecnac, "/5", "/195"),
    fecnac = str_replace_all(fecnac, "/4", "/194"),
    fecnac = str_replace_all(fecnac, "/3", "/193"),
    fecnac = str_replace_all(fecnac, "/15", "/1915"),
    fecnac = str_replace_all(fecnac, "/23", "/1923"),
    fecnac = str_replace_all(fecnac, "/25", "/1925"),
    fecnac = str_replace_all(fecnac, "/26", "/1926"),
    fecnac = str_replace_all(fecnac, "/28", "/1928"),
    fecnac = str_replace_all(fecnac, "/29", "/1929"),
    fecnac = str_replace_all(fecnac, "/196/", "/6/"),
    fecnac = str_replace_all(fecnac, "/195/", "/5/"),
    fecnac = str_replace_all(fecnac, "/194/", "/4/"),
    fecnac = str_replace_all(fecnac, "/193/", "/3/"),
    fecnac = str_replace_all(fecnac, "/1930/", "/30/"),
    fecnac = str_replace_all(fecnac, "/1931/", "/31/"),
    fecnac = str_replace_all(fecnac, "/1915/", "/15/"),
    fecnac = str_replace_all(fecnac, "/1923/", "/23/"),
    fecnac = str_replace_all(fecnac, "/1925/", "/25/"),
    fecnac = str_replace_all(fecnac, "/1926/", "/26/"),
    fecnac = str_replace_all(fecnac, "/1928/", "/28/"),
    fecnac = str_replace_all(fecnac, "/1929/", "/29/"),
    fecnac = ifelse(fecnac == "1/0/00", NA, fecnac),
    # Remove nonsensical DOBs
    fecnac = mdy(fecnac)
  )


# Children <5 with no diarrhea data - no age data
no_dair <- c(NA, 0, 12, "12487", "CC", 1, NA) %>%
  rbind(c(NA, 0, 12, "12487", "CC", 1, NA)) %>%
  rbind(c(NA, 0, 14, "14324", "CC", 1, NA)) %>%
  rbind(c(NA, 1, 9, "09623", "CS", 1, NA)) %>%
  rbind(c(NA, 1, 9, "09245", "CS", 1, NA)) %>%
  as_tibble()

colnames(no_dair) <- c("childid",
                       "wtreatment",
                       "strata",
                       "clust_level",
                       "grupo",
                       "death",
                       "child_order")
no_dair <- no_dair %>%
  mutate(
    wtreatment = as.numeric(wtreatment),
    death = as.numeric(death),
    strata = as.numeric(strata),
    child_order = as.numeric(child_order)
  )

mortality_reller <- data_reller %>%
  bind_rows(no_dair) %>%
  # check if age variable is edad or edadac
  mutate(
    treatment_time = ymd("2001-08-01"),
    event_time_ub = mdy(event_time_ub),
    event_time_lb = mdy(event_time_lb),
    age_month = as.numeric(treatment_time - fecnac) / 30,
    age_year = age_month / 12,
    precision_fut = "week",
    precision_trt = "month",
    precision_evt = "day",
    crossed_five = 0,
    cluster_id = NA
  ) %>%
  gen_vars("Reller et al., 2003", "month")

mortality_reller <- mortality_reller %>%
  dplyr::filter(age_year < 5 | is.na(age_year))
mortality_reller$event_time_lb[which(mortality_reller$death == 1 &
                                       is.na(mortality_reller$event_time_lb))] =
  mortality_reller$treatment_time[which(mortality_reller$death == 1 &
                                          is.na(mortality_reller$event_time_lb))]
mortality_reller$event_time_ub[which(mortality_reller$death == 1 &
                                       is.na(mortality_reller$event_time_ub))] =
  mortality_reller$follow_up_time[which(mortality_reller$death == 1 &
                                          is.na(mortality_reller$event_time_ub))]
mortality_reller$event_time_ub[which(mortality_reller$death == 1 &
                                       is.na(mortality_reller$event_time_ub))] =
  max(mortality_reller$follow_up_time, na.rm = TRUE)
table(mortality_reller$wtreatment, mortality_reller$death)

## Peletz et al. 2012 ----------------------------------------------------------

data_peletz$visit[which(is.na(data_peletz$dateinte))] = 0
names(data_peletz)[50] <- "agedays_b"
data_peletz$agedays_b[which(is.na(data_peletz$agedays_b))] = 0
data_peletz$firstdateinte = data_peletz$dateinte
data_peletz$firstdateinte[which(data_peletz$visit != 1)] = "2061-01-01"
data_peletz$dateinte[which(is.na(data_peletz$dateinte))] = "1961-01-01"
data_peletz$month = month(data_peletz$dateinte)

mortality_peletz <- data_peletz %>%
  # 121 "child 1" (+twin in one household)
  dplyr::filter((child == 1 &
            id != 2019) | ((child == 1 | child == 2) & id == 2019)) %>%
  group_by(person, id) %>%
  summarise(
    babydied = first(babydied),
    datedied = max(datedied, na.rm = T),
    arm = first(arm),
    diarr = first(diarr),
    communit = first(communit),
    age_month = max(agedays_b / 30),
    age_year = age_month / 12,
    follow_up_time = max(dateinte),
    visit = max(visit),
    month = month(min(firstdateinte))
  ) %>%
  rename(
    death = babydied,
    wtreatment = arm,
    strata = communit,
    event_time_ub = datedied
  ) %>%
  mutate(
    treatment_time = ifelse(month == 12 |
                              (month == 1 & visit > 0), "2010-12-01", NA),
    treatment_time = ifelse(month == 11, "2010-11-01", treatment_time),
    treatment_time = ifelse(month == 10, "2010-10-01", treatment_time),
    treatment_time = ifelse(month == 9, "2010-09-01", treatment_time),
    treatment_time = ifelse(month == 8, "2010-08-01", treatment_time),
    treatment_time = ifelse(month <= 7 &
                              month > 1, "2010-06-01", treatment_time),
    treatment_time = ymd(treatment_time),
    precision_trt = ifelse(treatment_time == "2010-06-01", "quarter", "month"),
    precision_evt = "day",
    crossed_five = 0,
    event_time_ub = ifelse(event_time_ub == "." |
                             event_time_ub == " ", NA, event_time_ub),
    event_time_ub = dmy(event_time_ub),
    cluster_id = NA
  ) %>%
  gen_vars("Peletz et al., 2012", "month")

mortality_peletz$follow_up_time[which(mortality_peletz$follow_up_time == "1961-01-01")] = NA
mortality_peletz$precision_fut = ifelse(is.na(mortality_peletz$follow_up_time), NA, "day")
mortality_peletz$event_time_ub[which(mortality_peletz$death != 1)] = NA
mortality_peletz$event_time_ub[which(is.na(mortality_peletz$death))] = NA
mortality_peletz$event_time_lb = mortality_peletz$event_time_ub
mortality_peletz$treatment_time[which(mortality_peletz$death == 1 &
                                        is.na(mortality_peletz$treatment_time))] =
  min(mortality_peletz$treatment_time, na.rm = TRUE)
data_peletz$dateinte[which(data_peletz$dateinte == "1961-01-01")] = NA

## Null et al. 2018 -----------------------------------------------------------

data_null <-
  transform(data_null, baseline = paste0(year_baseline, week_baseline))
data_null <-
  transform(data_null, survey = paste0(year_survey, week_survey))

# ------ Checking raw data ------ #

table(data_null$baseline) # Ranges from 2012 week 48 to 2014 week 9 - consistent with the paper
table(data_null$survey) # Ranges from 2012 week 48 to 2016 week 9 - Follow-ups are from Jan 2014-July 2016 // not consistent

# Looking into the time, baseline, and survey variables

survey_vars <- data_null %>%
  select(time, baseline, survey, year_baseline, year_survey) %>%
  mutate(diff = year_survey - year_baseline)

# Time==0 -> baseline, Time==1 -> year 1 follow-up, Time==2 -> year 2 follow-up
survey_vars %>%
  group_by(time) %>%
  summarise(diff = mean(diff))

# summarizing age by treatment and time (survey time)
sum_age <- data_null %>%
  group_by(tr, time) %>%
  summarise(age_med = median(agem, na.rm = T))

# ---- Back to processing data ---- #

data_null <- data_null %>%
  select(clusterid,
         compoundid,
         hhid,
         childid,
         aged,
         agem,
         agey,
         tr,
         baseline,
         survey,
         time) %>%
  mutate(baseline = as.numeric(baseline),
         survey = as.numeric(survey)) %>%
  group_by(clusterid, compoundid, hhid, childid) %>%
  summarize(
    baseline = max(baseline),
    survey1 = max(survey),
    # Month/week of last follow-up
    survey0 = min(survey),
    # Month/week of first survey
    clusterid = max(clusterid),
    compoundid = max(compoundid),
    hhid = max(hhid),
    childid = max(childid),
    time = min(time),
    # Time of first survey (0 = baseline)
    agem = min(agem),
    # Age measured at first survey
    agemax = max(agem),
    time_last = max(time),
    tr = max(tr)
  ) %>%
  ungroup()

data_null$survey1[which(data_null$survey1 == data_null$baseline)] = NA

mortality_null <- data_null_death %>%
  mutate(
    wtreatment = ifelse(tr == 2, 1, 0),
    ac = ifelse(tr == 1, 1, 0),
    pc = ifelse(tr == 8, 1, 0)
  ) %>%
  rename(death = childdeath, cluster_id = clusterid) %>%
  dplyr::filter(wtreatment == 1 | ac == 1 | pc == 1) %>%
  left_join(data_null) %>%  # Note: some children are in mortality but not main data!
  mutate(
    year_baseline = substring(as.character(baseline), 1, 4),
    week_baseline = substring(as.character(baseline), 5),
    year_survey = substring(as.character(survey1), 1, 4),
    week_survey = substring(as.character(survey1), 5),
    year_survey0 = substring(as.character(survey0), 1, 4),
    week_survey0 = substring(as.character(survey0), 5),
    treatment_time = as.Date(paste(year_baseline, week_baseline, 1, sep =
                                     "-"), "%Y-%U-%u"),
    follow_up_time = as.Date(paste(year_survey, week_survey, 1, sep =
                                     "-"), "%Y-%U-%u"),
    survey0_time = as.Date(paste(year_survey0, week_survey0, 1, sep =
                                   "-"), "%Y-%U-%u"),
    event_time_ub = follow_up_time,
    event_time_lb = treatment_time,
    precision_trt = "month",
    precision_fut = "week",
    precision_evt = "year",
    crossed_five = 0,
    age_month = agem - as.numeric(survey0_time - treatment_time) /
      30,
    age_year = age_month / 12
  ) %>%
  gen_vars("Null et al., 2018", "month")

mortality_null$event_time_ub[which(is.na(mortality_null$event_time_ub) &
                                     !is.na(mortality_null$event_time_lb))] = max(mortality_null$follow_up_time, na.rm = TRUE)
mortality_null$event_time_ub[which(mortality_null$death == 0)] = NA
mortality_null$event_time_lb[which(mortality_null$death == 0)] = NA
mortality_null$event_time_lb[which(mortality_null$death == 1 &
                                     is.na(mortality_null$event_time_lb))] =
  min(mortality_null$treatment_time, na.rm = TRUE)
mortality_null$event_time_ub[which(mortality_null$death == 1 &
                                     is.na(mortality_null$event_time_ub))] =
  max(mortality_null$follow_up_time, na.rm = TRUE)
mortality_null$treatment_time[which(mortality_null$death == 1 &
                                      is.na(mortality_null$treatment_time))] =
  mortality_null$event_time_lb[which(mortality_null$death == 1 &
                                       is.na(mortality_null$treatment_time))]

mortality_null <- mortality_null %>%
  dplyr::filter(age_year < 5 | is.na(age_year))

mean(
  interval(
    mortality_null$treatment_time,
    mortality_null$follow_up_time
  ) %/%
    months(1),
  na.rm = T
)

mortality_null$cluster_id <- as.character(mortality_null$cluster_id)
mortality_null$cluster_id <-
  sapply(mortality_null$cluster_id, function(x)
    paste('Null et al., 2018', x))
mortality_null$cluster_id <-
  sapply(mortality_null$cluster_id, get_cluster)

## Haushofer et al. 2020 --------------------------------------------------------

mortality_haush <- data_haush %>%
  dplyr::filter(wave == 1 & child_yob >= 2013 |
           wave != 1 & child_yob >= 2014) %>%
  rename(wtreatment = waterp,
         death = dead_u5) %>%
  mutate(
    event_time_lb = as.Date(paste(child_yod, child_mod, 1, sep = "-"), "%Y-%m-%d"),
    end_day = ifelse(child_mod %in% c(4, 6, 9, 11), 30, 31),
    end_day = ifelse(child_mod == 2, ifelse(child_yod == 2016, 29, 28), end_day),
    event_time_ub = as.Date(paste(child_yod, child_mod, end_day, sep =
                                    "-"), "%Y-%m-%d"),
    # Becky Scurlock: "The paper just said that the endline survey occurred "between
    # February and March 2018" so I picked a date in the middle of this range. "
    follow_up_time = ymd("2018-03-01"),
    treatment_time = case_when (
      wave == 1 ~ "2012-12-01",
      wave == 2 ~ "2013-07-01",
      wave == 3 ~ "2013-09-01",
      wave == 4 ~ "2013-11-01",
      wave == 5 ~ "2014-01-01",
      wave == 6 ~ "2014-03-01",
      wave == 7 ~ "2014-05-01"
    ),
    treatment_time = ymd(treatment_time),
    birth_month = as.Date(paste(child_yob, child_mobD, 1, sep = "-"), "%Y-%m-%d"),
    age_month = (year(treatment_time) - child_yob) * 12,
    age_month = ifelse(child_yob == 2018, age_month + 9, age_month),
    # We add 9 to children born in 2018 since they were born at most 3 = 12 - 9
    # months before follow-up
    age_month = ifelse(
      death == 1,
      interval(birth_month, treatment_time) %/%
        months(1),
      age_month
    ),
    age_year = age_month / 12,
    precision_trt = "month",
    precision_fut = "month",
    precision_evt = "month",
    crossed_five = 0
  ) %>%
  gen_vars("Haushofer et al., 2020", "month")


interval(mortality_haush$follow_up_time,
         mortality_haush$treatment_time)  %/% years(1)

mortality_haush$cluster_id <-
  as.character(mortality_haush$cluster_id)
mortality_haush$cluster_id <-
  sapply(mortality_haush$cluster_id, function(x)
    paste('Haushofer et al., 2020', x))
mortality_haush$cluster_id <-
  sapply(mortality_haush$cluster_id, get_cluster)

## List of all available individual data ----------------------------------------

mortality_1 <- list(
  "Chiller et al., 2006" = mortality_chiller,
  "Crump et al., 2005" = mortality_crump,
  "Haushofer et al., 2020" = mortality_haush,
  "Kirby et al., 2019" = mortality_kirby,
  "Kremer et al., 2011" = mortality_kremer,
  "Null et al., 2018" = mortality_null,
  "Peletz et al., 2012" = mortality_peletz,
  "Reller et al., 2003" = mortality_reller
)

mortality_2 <- list(
  "Boisson et al., 2013" = mortality_boisson,
  "Dupas et al., 2021" = mortality_dupas,
  "Luby et al., 2006" = mortality_luby2006
)

# (3) Filtering variables with children less than 5 years ======================

mortality_1 <-
  lapply(mortality_1, function(x)
    dplyr::filter(x, age_year < 5 | is.na(age_year)))

# For some of the data the age variable is not available - studies where the sample only consists of
# individuals under the age of 5
mortality_2 <-
  lapply(mortality_2, function(x)
    dplyr::filter(x, age_year < 5))
mortality_all <- c(mortality_1, mortality_2)

# (4) Keeping relevant variables for final dataset =============================
vars = c(
  "death",
  "wtreatment",
  "age_month",
  "age_year",
  "age_measure_original",
  "study",
  "study",
  "treatment_time",
  "event_time_lb",
  "event_time_ub",
  "follow_up_time",
  "precision_trt",
  "precision_evt",
  "precision_fut",
  "crossed_five",
  "cluster_id"
)
mortality_all <-
  lapply(mortality_all, function(x)
    select(ungroup(x) , vars))
for (i in 1:length(mortality_all)) {
  # Ensure treatment indicator is not a factor
  mortality_all[[i]]$wtreatment = as.numeric(mortality_all[[i]]$wtreatment)
}

# (5) Checks ===================================================================

# Individual level data
mortality_counts <- lapply(mortality_all, function(x) {
  counts <- x %>%
    group_by(wtreatment) %>%
    summarise(cases = sum(death),
              count = n()) %>%
    mutate(noncases = count - cases)
})

# (6) Final data-set of event counts for all papers ============================

mortality_all_df <- bind_rows(mortality_all)

# Check if all clusters are actually clusters
# (all trt or all control)
all_clusters_okay <- 
  mortality_all_df %>%
  group_by(study, wtreatment, cluster_id) %>%
  summarise(n = n(), t = sum(wtreatment == 1)) %>%
  dplyr::filter(!is.na(cluster_id)) %>%
  mutate(x = (n == t | t == 0)) %>%
  pull(x) %>% all()

if (!all_clusters_okay)
  stop("Cluster IDs are wrong")

# Anonymise and save data =====================================
mortality_all_df %>% 
  select(
    -c(
      age_month,
      age_year
    )
  ) %>%
  write_rds(
    here("data/final/individual_data_anonymised.rds")
  )
