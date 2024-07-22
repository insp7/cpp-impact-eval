library(readr)
library(dplyr)
library(openxlsx)
library(rprojroot)

# Find the root of the current project
root <- find_root(is_rstudio_project)

# ------- PACE Data Constants ------- 
PACE_DATA_PATH <- 'data/PACE/'
PACE_DS1 <- "37289-0001-Data-REST.tsv"
PACE_DS2 <- "37289-0002-Data-REST.tsv"
PACE_DS7 <- "37289-0007-Data-REST.tsv"
PACE_DS8 <- "37289-0008-Data-REST.tsv"
PACE_DS10 <- "37289-0010-Data-REST.tsv"
PACE_DS15 <- "37289-0015-Data-REST.tsv"

# ------- Read Raw data ------- 
pace_ds1_raw <- read_tsv(paste(PACE_DATA_PATH, PACE_DS1, sep = ""), col_types = cols())
pace_ds2_raw <- read_tsv(paste(PACE_DATA_PATH, PACE_DS2, sep = ""), col_types = cols())
pace_ds7_raw <- read_tsv(paste(PACE_DATA_PATH, PACE_DS7, sep = ""), col_types = cols()) # SAQ
pace_ds8_raw <- read_tsv(paste(PACE_DATA_PATH, PACE_DS8, sep = ""), col_types = cols())
pace_ds10_raw <- read_tsv(paste(PACE_DATA_PATH, PACE_DS10, sep = ""), col_types = cols()) # Augmented Job Spells Data
pace_ds15_raw <- read_tsv(paste(PACE_DATA_PATH, PACE_DS15, sep = ""), col_types = cols())

# ------- Extract Required Columns ------- 
pace_ds1 <- pace_ds1_raw %>%
  select(ISISID, R_SITE, R_TREATMENT_STATUS, R_YEAR_UP_SUB, AGE, CURRWORKHOURS, CURRHOURLYWAGE, WEEKLYWAGES, WEIGHT15, SURVEYRESPONDENT15)

pace_ds2 <- pace_ds2_raw %>% 
  select(B_UNIQUE_ID1, B09_HISPANIC, B10_RACE_WHITE, B10_RACE_BLACK, B10_RACE_ASIAN, B10_RACE_AM_INDIAN, B10_RACE_PACIFIC, 
         B23_GRADES, B17_EDUC, B07_SEX, B08_MARSTAT, B11_BORN_USA, B12A_OTHER_LANG, B15A_NUM_CHN, B18_VOCTECH_CERT, 
         B12B_SPEAK_ENGLISH, B12C_READ_ENGLISH, B24_CURR_WORK, B26C_PA, B26B_WICSNAP)

pace_ds7 <- pace_ds7_raw %>% 
  select(B_UNIQUE_ID1, S01_FUTURE_SCHOOL, S02_FUTURE_WORK, S02A_WORK_HOURS, S05_FINANCIAL_SUPPORT, 
         S13A_ABILITIES, S13B_PLAN, S13C_HELP, S13D_JOB, S13E_ORGANIZATION, S13F_OCCUPATION, S13G_EDUCATION,
         S11CC_B_OTHER_THAN_SCHOOL, S11CC_F_STOP_ATTENDING, S11CC_I_HOW_LONG, S11CC_P_DO_WELL, S11CC_T_MOTIVIATED, 
         S11CC_DD_SUCCEED, S11CC_EE_BEST_CHOICE, S11CC_GG_COMMITTED, S11CC_JJ_ACHIEVE_GOALS, S11CC_LL_RIGHT_FOR_ME,
         S11AD_D_DISCIPLINED, S11AD_J_SKIP_CLASSES, S11AD_M_ABILITIES, S11AD_N_NOTES, S11AD_R_DEADLINES, 
         S11AD_U_PERFORMANCE, S11AD_II_DO_BEST, S11AD_KK_CONSISTENTLY, S11AD_NN_HARD_WORKING, S11AD_PP_ASSIGNMENTS,
         S11ES_A_CALM, S11ES_E_LOSE_CONTROL, S11ES_G_IRRITATED, S11ES_L_UPSET, S11ES_Q_EXPRESS_ANGER, S11ES_S_THINK_CLEARLY, 
         S11ES_V_TEMPER, S11ES_W_ANNOYED, S11ES_Y_PATIENT, S11ES_AA_ARGUMENTS, S11ES_FF_FRUSTRATION, S11ES_HH_OUT_OF_CONTROL,
         S12A_DEPEND, S12B_CLOSE, S12C_TURN_TO, S12D_ACTIVITIES, S12E_RESPECT, 
         S12F_ASSISTANCE, S12G_EMOTIONAL_SECURITY, S12H_COMPENTENCE, S12I_INTERESTS, S12J_TRUSTWORTHY, 
         S15A_CHILD_CARE, S15B_TRANSPORTATION, S15C_ALCOHOL, S15D_ILLNESS, S15E_ARGUMENTS, S15F_VIOLENCE,
         S14A_CONTROL, S14B_CONFIDENCE, S14C_GOING_YOUR_WAY, S14D_DIFFICULTIES)

pace_ds8 <- pace_ds8_raw %>%
  select(ABTSRBIID, SITE_NUM, EMPLOYEDATFOLLOWUP, CURRENTHOURS, CURRENTWAGERATE, ACE_SIMILARWT_3DIM, C_SKIPOUT)

pace_ds10 <- pace_ds10_raw %>% 
  select(ABTSRBIID, CLOSED, F5, F6, F1A, F1A_2, STARTDATE)

pace_ds15 <- pace_ds15_raw %>%
  select(ABTID, SITE_NUM, EMPLOYEDATFOLLOWUP_LO, CURRENTHOURS_LO, CURRENTWAGERATE_LO, WEEKLYEARNINGS_LO, WEIGHT72NDNHPS)
pace_ds15$surveyrespondent_72_pace <- 1

# ------- Write extracted info. to CSV ------- 
folder_path <- file.path(root, "read-PACE-output")

# Create the folder if it doesn't exist
if(!dir.exists(folder_path)) { 
  dir.create(folder_path)
}

write.csv(pace_ds1, file.path(folder_path,"pace_ds1.csv"), row.names = FALSE)
write.csv(pace_ds2, file.path(folder_path,"pace_ds2.csv"), row.names = FALSE) 
write.csv(pace_ds7, file.path(folder_path,"pace_ds7.csv"), row.names = FALSE)
write.csv(pace_ds8, file.path(folder_path,"pace_ds8.csv"), row.names = FALSE)
write.csv(pace_ds10, file.path(folder_path,"pace_ds10.csv"), row.names = FALSE)
write.csv(pace_ds15, file.path(folder_path,"pace_ds15.csv"), row.names = FALSE)

# Remove all data frames
rm(list = ls())
gc()