#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#     Characteristics of included and excluded diarrhea studies
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Import data
#------------------------------------------------------------------------------#

library(readxl)
df_studies <- read_xlsx(
  here("data/raw/diarrhea_studies.xlsx"),
  sheet ="Sheet 1 - diarrhea_studies", 
  skip=3
)

df_studies <-df_studies%>%
  dplyr::select(reference, country, setting, intervention, "baseline water/water in control group",
         "effect estimate (on diarrhea)", "Compliance rate" , "Included" )

# Cleaning up 
df_studies <-df_studies%>%
  mutate(
    `Compliance rate`=ifelse(`Compliance rate` %in% c("not reported", "Not reported", "NA"),
                             NA, `Compliance rate`),
    `Compliance rate`= as.numeric(`Compliance rate`),
    Included=ifelse(is.na(Included), "Excluded", "Included"),
    Included_dummy=ifelse(Included=="Excluded", 0, 1),
    setting=ifelse(setting=="rural, urban, rural", "mixed", setting)
  )
# 
# df_studies[grepl("Solar disinfection of water reduces diarrhoeal", df_studies$reference),]$Included <- "Included"
# df_studies[grepl("Household water chlorination", df_studies$reference),]$Included <- "Included"
# 
# df_studies[grepl("Solar disinfection of water reduces diarrhoeal", df_studies$reference),]$Included_dummy <- 1
# df_studies[grepl("Household water chlorination", df_studies$reference),]$Included_dummy <- 1
# 


# Grouping interventions
df_studies <-df_studies%>%
  mutate(
    intervention_group=case_when(
      str_detect(intervention, "chlorination") ~ "Chlorination",
      str_detect(intervention, "chlination") ~ "Chlorination",
      str_detect(intervention, "pasteurization") ~ "Pasteurization",
      str_detect(intervention, "safe storage") ~ "safe storage",
      str_detect(intervention, "filter") ~ "Filtration" ,
      str_detect(intervention, "filtration") ~ "Filtration" ,
      str_detect(intervention, "solar") ~ "Solar disinfection" ,
      str_detect(intervention, "piped") ~ "Piped water"  ,
      str_detect(intervention, "spring protection") ~ "Community improved water supply" 
    )
  )


# Table of p-values for t-tests 
df_studies_1<-df_studies%>%
  mutate(intervention_group=ifelse(is.na(intervention_group), "Spring protection", intervention_group))%>%
  dplyr::filter(!is.na(intervention))%>%
  mutate(
    rural=ifelse(setting=="rural", 1, 0),
    unimproved=ifelse(`baseline water/water in control group`=="unimproved", 1, 0),
    chlorination=ifelse(intervention_group %in% c("Chlorination"), 1, 0),
    filtration=ifelse(intervention_group %in% c("Filtration"), 1, 0),
    community=ifelse(intervention_group %in% c("Community improved water supply"), 1, 0)
  )

df_compliance<-df_studies_1%>%
  dplyr::filter(!is.na(`Compliance rate`))

reg<-list()

reg[[1]]<-lm(`effect estimate (on diarrhea)`  ~ Included_dummy, data= df_studies_1)
reg[[2]]<-lm(`Compliance rate`  ~ Included_dummy, data= df_compliance)
reg[[3]]<-lm(rural  ~ Included_dummy, data= df_studies_1)
reg[[4]]<-lm(unimproved  ~ Included_dummy, data= df_studies_1)
names(reg) <- c("effect on diarrhea", "compliance", "rural", "unimproved")






