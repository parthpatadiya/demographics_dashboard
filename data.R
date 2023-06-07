library(haven)
dm2_data <- read_sas("data/dm2.sas7bdat",col_select = c("project","Subject","SiteNumber","Site","StudyEnvSiteNumber","AGE","SEX","ETHNIC","WHITE","BLACK","ASIAN","INDALK","ISLAND","RACEUNK","RACENREP"))

library(dplyr)

dm2_data=dm2_data %>% 
  mutate(AGE_GROUP = case_when(AGE > 18 & AGE <= 65 ~ "18-65",
                             AGE >= 66 & AGE <= 75 ~ "66-75", 
                             AGE >= 76 ~ ">=76",
                             is.na(AGE) ~ "Not Reported or Missing"))%>%
  mutate(RACIAL_GROUP = c("White","Black or African American","Asian","American Indian or Alaska Native", "Native Hawaiian/Other Pacific Islander","Race Unknown","Race Not Reported")[max.col(dm2_data[c("WHITE","BLACK","ASIAN","INDALK","ISLAND","RACEUNK","RACENREP")],ties.method = "last")])
dm2_data=  replace(dm2_data,dm2_data=='', "Not Reported or Missing")




