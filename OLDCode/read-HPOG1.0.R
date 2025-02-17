library(readr)
library(dplyr)
library(openxlsx)
library(rprojroot)

# Find the root of the current project
root <- find_root(is_rstudio_project)

source("utils.R")

# ------- HPOG 1.0 Data Constants ------- 
HPOG_DATA_PATH <- 'data/HPOG 1.0/'
HPOG_DS4 <- "37290-0004-Data-REST.tsv"
HPOG_DS5 <- "37290-0005-Data-REST.tsv"
HPOG_DS6 <- "37290-0006-Data-REST.tsv"
HPOG_DS15 <- "37290-0015-Data-REST.tsv"
HPOG_DS17 <- "37290-0017-Data-REST.tsv"
HPOG_DS21 <- "37290-0021-Data-REST.tsv"

# ------- Read Raw data ------- 
hpog_ds4_raw <- read_tsv(paste(HPOG_DATA_PATH, HPOG_DS4, sep = ""), col_types = cols()) 
hpog_ds5_raw <- read_tsv(paste(HPOG_DATA_PATH, HPOG_DS5, sep = ""), col_types = cols()) 
hpog_ds6_raw <- read_tsv(paste(HPOG_DATA_PATH, HPOG_DS6, sep = ""), col_types = cols()) 
hpog_ds15_raw <- read_tsv(paste(HPOG_DATA_PATH, HPOG_DS15, sep = ""), col_types = cols()) 
hpog_ds17_raw <- read_tsv(paste(HPOG_DATA_PATH, HPOG_DS17, sep = ""), col_types = cols()) 
hpog_ds21_raw <- read_tsv(paste(HPOG_DATA_PATH, HPOG_DS21, sep = ""), col_types = cols()) 

# ------- Extract Required Columns ------- 
hpog_ds4 <- hpog_ds4_raw %>%
  select(RID, Q36, Q36A, Q36B) # added RID

hpog_ds5 <- hpog_ds5_raw %>%
  select(RID, PACE_ID, RANDOM_ASSIGNMENT, DATE_RANDOM, USERSITE, CREDENTIAL, CURRENTWORK,
         COV_SVCS, COV_BEHAVIORAL_INCENTIVES, COV_AVG_FTE_CASELOAD, COV_CHILDCARE_TRANSPORT, 
         COV_SVCS_SOCIAL, COV_TUITION_FIN_SVCS, COV_EMPLOYMENT_SUPPORTS, COV_LOCATION_OF_SERVICES, 
         COV_EMERGENCY_ASSISTANCE, COV_PEER_SUPPORT, COV_CP_PRINCIPLES, 
         LC_EDATTAIN, LC_EMPPCT, LC_MEDWAGE, LC_PUBASS, LC_SCHEN, LC_TOTPOP, LC_UERATE, SURVEYRESPONDENT) %>% 
  rename(surveyrespondent_15 = SURVEYRESPONDENT)


hpog_ds6 <- hpog_ds6_raw %>% 
  select(RID, DOB_MONTH, DOB_YEAR, ETHNICITY, RACE_W, RACE_A, RACE_B, RACE_HPI, RACE_NA, 
         DEGREE_RECEIVED_GED, DEGREE_RECEIVED_HS, DEGREE_RECEIVED_CERTIFICATE, 
         HIGHEST_EDUCATION, DEPENDENT_CHILDREN, GENDER, MARITAL, CITIZENSHIP, 
         LIMITED_ENGLISH, CURRENT_EMPLOYMENT_ENROLL, PAID_WORK, IB_TANF, IB_SNAP, IB_WIC, 
         EXPECTED_SCHOOL_ENROLL, EXPECTED_WORK, EXPECTED_HOURS, 
         GOALS_ORGANIZATION_TYPE, GOALS_5YEAR_PLAN, GOALS_FUTURE_OCCUPATION, 
         OFTEN_ALCOHOL_DRUG, OFTEN_CHILD_CARE, OFTEN_ILLNESS, OFTEN_TRANSPORTATION)

hpog_ds15 <- hpog_ds15_raw %>%
  select(ABTSRBIID, RID, USERSITE, LICENSE36, EMPLOYEDATFOLLOWUP, SURVEYRESPONDENT) %>% 
  rename(surveyrespondent_36 = SURVEYRESPONDENT)

hpog_ds17 <- hpog_ds17_raw %>%
  select(ABTSRBIID, F5, F6, STARTDATE, CLOSED, F5A, F5A_2)

hpog_ds21 <- hpog_ds21_raw %>%
  select(ABTSRBIID, USERSITE, EDPROGRESS_ANYCREDENTIAL, 
         EMPLOYMENT_EMPLOYED, EMPLOYMENT_WEEKHOURS, EMPLOYMENT_HOURLYWAGE, EMPLOYMENT_WEEKEARNINGS, 
         WEIGHT, SURVEYRESPONDENT72) %>% 
  rename(surveyrespondent_72 = SURVEYRESPONDENT72)


# ------- Write extracted info. to CSV ------- 
folder_path <- file.path(root, "read-HPOG1.0-output")

# Create the folder if it doesn't exist
if(!dir.exists(folder_path)) { 
  dir.create(folder_path)
}

write.csv(hpog_ds4, file.path(folder_path,"hpog_ds4.csv"), row.names = FALSE)
write.csv(hpog_ds5, file.path(folder_path,"hpog_ds5.csv"), row.names = FALSE)
write.csv(hpog_ds6, file.path(folder_path,"hpog_ds6.csv"), row.names = FALSE)
write.csv(hpog_ds15, file.path(folder_path,"hpog_ds15.csv"), row.names = FALSE)
write.csv(hpog_ds17, file.path(folder_path,"hpog_ds17.csv"), row.names = FALSE)
write.csv(hpog_ds21, file.path(folder_path,"hpog_ds21.csv"), row.names = FALSE)

# Remove all data frames
rm(list = ls())
gc()