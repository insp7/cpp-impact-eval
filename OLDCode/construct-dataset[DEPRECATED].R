# HPOG: DS4, DS5, DS6, DS12, DS15, DS17, DS21
# PACE: DS1, DS2, DS8, DS15

# HPOG Files Description:
# HPOG DS4 - 15 Month Follow-Up Survey - RAW DATA - rid - if it is in ds5, preserve, else dont
# HPOG DS5 - Analysis File Outcomes and Covariates - more processed version of DS4 - rid - Preserve all rows
# HPOG DS6 - Performance Reporting System - Participant
# HPOG DS7 - Performance Reporting System: Impact Supplemental
# HPOG DS12 - PACE Basic Information Form Data File
# HPOG DS15 - 3 Year Updated Analysis Data File
# HPOG DS17 - 3 year Augmented Job Spells Data File
# HPOG DS21 - 6 Year Updated Analysis Data File

# PACE Files Description:
# PACE DS1 - Analysis Data File
# PACE DS2 - Basic Information Form Data File
# PACE DS7 - Self Administered Questionnaire ... NOTE: not used yet
# PACE DS8 - 3 Year Updated Analysis Data File
# PACE DS15 - 6 Year Updated Analysis Data File

library(readr)
library(dplyr)
library(openxlsx)

# ----------------- READ THE NECESSARY DS FILES -----------------

# Read HPOG 1.0 Data
HPOG_DATA_PATH <- 'data/HPOG 1.0/'
HPOG_DS4 <- "37290-0004-Data-REST.tsv"
HPOG_DS5 <- "37290-0005-Data-REST.tsv"
HPOG_DS6 <- "37290-0006-Data-REST.tsv"
HPOG_DS15 <- "37290-0015-Data-REST.tsv"
HPOG_DS17 <- "37290-0017-Data-REST.tsv"
HPOG_DS21 <- "37290-0021-Data-REST.tsv"

# by default paste function separates 2 strings by space
hpog_ds4_raw <- read_tsv(paste(HPOG_DATA_PATH, HPOG_DS4, sep = ""), col_types = cols()) 
hpog_ds5_raw <- read_tsv(paste(HPOG_DATA_PATH, HPOG_DS5, sep = ""), col_types = cols()) 
hpog_ds6_raw <- read_tsv(paste(HPOG_DATA_PATH, HPOG_DS6, sep = ""), col_types = cols()) 
hpog_ds15_raw <- read_tsv(paste(HPOG_DATA_PATH, HPOG_DS15, sep = ""), col_types = cols()) 
hpog_ds17_raw <- read_tsv(paste(HPOG_DATA_PATH, HPOG_DS17, sep = ""), col_types = cols()) 
hpog_ds21_raw <- read_tsv(paste(HPOG_DATA_PATH, HPOG_DS21, sep = ""), col_types = cols()) 


# Read PACE Data
PACE_DATA_PATH <- 'data/PACE/'
PACE_DS1 <- "37289-0001-Data-REST.tsv"
PACE_DS2 <- "37289-0002-Data-REST.tsv"
PACE_DS7 <- "37289-0007-Data-REST.tsv"
PACE_DS8 <- "37289-0008-Data-REST.tsv"
PACE_DS10 <- "37289-0010-Data-REST.tsv"
PACE_DS15 <- "37289-0015-Data-REST.tsv"

pace_ds1_raw <- read_tsv(paste(PACE_DATA_PATH, PACE_DS1, sep = ""), col_types = cols())
pace_ds2_raw <- read_tsv(paste(PACE_DATA_PATH, PACE_DS2, sep = ""), col_types = cols())
pace_ds7_raw <- read_tsv(paste(PACE_DATA_PATH, PACE_DS7, sep = ""), col_types = cols()) # SAQ
pace_ds8_raw <- read_tsv(paste(PACE_DATA_PATH, PACE_DS8, sep = ""), col_types = cols())
pace_ds10_raw <- read_tsv(paste(PACE_DATA_PATH, PACE_DS10, sep = ""), col_types = cols()) # Augmented Job Spells Data
pace_ds15_raw <- read_tsv(paste(PACE_DATA_PATH, PACE_DS15, sep = ""), col_types = cols())

# ----------------- EXTRACT HPOG 1.0 REQUIRED COLUMNS -----------------
hpog_ds4 <- hpog_ds4_raw %>%
  select(RID, Q36A, Q36B) # added RID

hpog_ds5 <- hpog_ds5_raw %>%
  select(RID, PACE_ID, RANDOM_ASSIGNMENT, USERSITE, CREDENTIAL, CURRENTWORK,
         COV_SVCS, COV_BEHAVIORAL_INCENTIVES, COV_AVG_FTE_CASELOAD, COV_CHILDCARE_TRANSPORT, COV_SVCS_SOCIAL, 
         COV_TUITION_FIN_SVCS, COV_EMPLOYMENT_SUPPORTS, COV_LOCATION_OF_SERVICES, COV_EMERGENCY_ASSISTANCE, COV_PEER_SUPPORT, COV_CP_PRINCIPLES, LC_EDATTAIN, LC_EMPPCT, LC_MEDWAGE, LC_PUBASS, LC_SCHEN, LC_TOTPOP, LC_UERATE)

hpog_ds6 <- hpog_ds6_raw %>% 
  select(RID, DOB_MONTH, DOB_YEAR, RA_DATE_MONTH, RA_DATE_YEAR, ETHNICITY, RACE_W, RACE_A, RACE_B, RACE_HPI, RACE_NA, DEGREE_RECEIVED_GED, DEGREE_RECEIVED_HS, DEGREE_RECEIVED_CERTIFICATE, HIGHEST_EDUCATION, DEPENDENT_CHILDREN, GENDER, MARITAL, CITIZENSHIP, LIMITED_ENGLISH, CURRENT_EMPLOYMENT_ENROLL, PAID_WORK, IB_TANF, IB_SNAP, IB_WIC, EXPECTED_SCHOOL_ENROLL, EXPECTED_WORK, EXPECTED_HOURS, GOALS_ORGANIZATION_TYPE, GOALS_5YEAR_PLAN, GOALS_FUTURE_OCCUPATION, OFTEN_ALCOHOL_DRUG, OFTEN_CHILD_CARE, OFTEN_ILLNESS, OFTEN_TRANSPORTATION)

hpog_ds15 <- hpog_ds15_raw %>%
  select(ABTSRBIID, RID, USERSITE, LICENSE36, EMPLOYEDATFOLLOWUP)

hpog_ds17 <- hpog_ds17_raw %>%
  select(ABTSRBIID, F5, F6, STARTDATE, CLOSED, F5A, F5A_2)

hpog_ds21 <- hpog_ds21_raw %>%
  select(ABTSRBIID, USERSITE, EDPROGRESS_ANYCREDENTIAL, EMPLOYMENT_EMPLOYED, EMPLOYMENT_WEEKHOURS, 
         EMPLOYMENT_HOURLYWAGE, EMPLOYMENT_WEEKEARNINGS, WEIGHT)

# ----------------- EXTRACT PACE REQUIRED COLUMNS -----------------
pace_ds1 <- pace_ds1_raw %>%
  select(ISISID, R_SITE, R_TREATMENT_STATUS, R_YEAR_UP_SUB, AGE, CURRWORKHOURS, CURRHOURLYWAGE, WEEKLYWAGES, WEIGHT15)

pace_ds2 <- pace_ds2_raw %>%
  select(B_UNIQUE_ID1, B09_HISPANIC, B10_RACE_WHITE, B10_RACE_BLACK, B10_RACE_ASIAN, B10_RACE_AM_INDIAN, B10_RACE_PACIFIC, B23_GRADES, B17_EDUC, B07_SEX, B08_MARSTAT, B11_BORN_USA, B12A_OTHER_LANG, B15A_NUM_CHN, B18_VOCTECH_CERT, B12B_SPEAK_ENGLISH, B12C_READ_ENGLISH, B24_CURR_WORK, B26C_PA, B26B_WICSNAP)

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
  select(ABTSRBIID, SITE_NUM, EMPLOYEDATFOLLOWUP, CURRENTHOURS, CURRENTWAGERATE, ACE_SIMILARWT_3DIM)

pace_ds10 <- pace_ds10_raw %>% 
  select(ABTSRBIID, CLOSED, F5, F6, F1A, F1A_2)

pace_ds15 <- pace_ds15_raw %>%
  select(ABTID, SITE_NUM, EMPLOYEDATFOLLOWUP_LO, CURRENTHOURS_LO, CURRENTWAGERATE_LO, WEEKLYEARNINGS_LO, WEIGHT72NDNHPS)

#  ----------------- FREE UP SAPCE  ----------------- 

rm(HPOG_DATA_PATH)
rm(PACE_DATA_PATH)

rm(HPOG_DS4)
rm(HPOG_DS5)
rm(HPOG_DS6)
rm(HPOG_DS15)
rm(HPOG_DS17)
rm(HPOG_DS21)

rm(PACE_DS1)
rm(PACE_DS2)
rm(PACE_DS7)
rm(PACE_DS8)
rm(PACE_DS10)
rm(PACE_DS15)

rm(hpog_ds4_raw)
rm(hpog_ds5_raw)
rm(hpog_ds6_raw)
rm(hpog_ds15_raw)
rm(hpog_ds17_raw)
rm(hpog_ds21_raw)

rm(pace_ds1_raw)
rm(pace_ds2_raw)
rm(pace_ds7_raw)
rm(pace_ds8_raw)
rm(pace_ds10_raw)
rm(pace_ds15_raw)


# Pre-processing for PACE DS10

calculate_employment_weekhours_pace <- function() {
  # initialize a vector to store the calculated values
  employment_weekhours_pace <- numeric(nrow(pace_ds10))
  
  # iterate over unique values of abtsrbiid
  unique_abtsrbiid <- unique(pace_ds10$ABTSRBIID)
  for(id in unique_abtsrbiid) {
    # subset the data for the current abtsrbiid
    pace_ds10_subset <- pace_ds10[pace_ds10$ABTSRBIID == id, ]
    
    # if all closed = 1, set employment_weekhours to 0
    if(all(pace_ds10_subset$CLOSED == 1)) {
      employment_weekhours_pace[pace_ds10$ABTSRBIID == id] <- 0
    } else {
      # else, calculate the sum of f6 when closed = 0
      employment_weekhours_pace[pace_ds10$ABTSRBIID == id] <- sum(pace_ds10_subset$F6[pace_ds10_subset$CLOSED == 0], na.rm = T)
    }
  }  
  return (employment_weekhours_pace)
}

pace_ds10 <- pace_ds10[pace_ds10$CLOSED == 0, ] # select all rows where closed == 0
pace_ds10$F5[pace_ds10$F5 == -1 | pace_ds10$F5 == -2] <- NA
pace_ds10$F6[pace_ds10$F6 == -1 | pace_ds10$F6 == -2] <- NA
pace_ds10$employment_weekhours_pace <- calculate_employment_weekhours_pace()
pace_ds10 <- pace_ds10 %>% 
  mutate(weekly_earnings_36_variation = case_when(
    F1A_2 == 1 ~ F1A * 5,
    F1A_2 == 2 ~ F1A,
    F1A_2 == 3 ~ F1A / 2,
    F1A_2 == 4 ~ F1A / 4,
    F1A_2 == 5 | F1A_2 == 6 | F1A_2 == 97 | F1A_2 == 98 ~ NA_real_,
    F1A_2 == 7 ~ F1A / 52,
    TRUE ~ NA_real_
  ))
pace_ds10$employment_weekearnings_pace <- ifelse(!is.na(pace_ds10$F5) & !is.na(pace_ds10$F6), 
                                            pace_ds10$F5 * pace_ds10$F6, 
                                            ifelse((is.na(pace_ds10$F5) | is.na(pace_ds10$F6)) & !is.na(pace_ds10$F1A), 
                                                   pace_ds10$weekly_earnings_36_variation, NA))
pace_ds10 <- pace_ds10 %>% 
  group_by(ABTSRBIID, employment_weekhours_pace) %>% 
  summarise(employment_weekearnings_pace = sum(employment_weekearnings_pace, na.rm = T))
pace_ds10$employment_weekhours_pace[pace_ds10$employment_weekhours_pace == 0] <- NA # if employment_weekhours_pace == 0, set it to NA -- to avoid divide by zero error.
pace_ds10$employment_weekearnings_pace[pace_ds10$employment_weekearnings_pace == 0] <- NA # set employment_weekearnings_pace to NA, if employment_weekearnings_pace is 0
pace_ds10$employment_hourlywage_pace <- ifelse(!is.na(pace_ds10$employment_weekearnings_pace) & !is.na(pace_ds10$employment_weekhours_pace), 
                                               pace_ds10$employment_weekearnings_pace / pace_ds10$employment_weekhours_pace, NA)
pace_ds10 <- pace_ds10 %>% 
  select(ABTSRBIID, employment_hourlywage_pace, employment_weekhours_pace, employment_weekearnings_pace)

# End of Pre-processing for PACE DS10

# ----------------- CREATE PACE DATA FRAME -----------------

# ALL PACE DATA in one table joined on ISISID = B_UNIQUE_ID1 = ABTSRBIID = ABTID
pace_df1 <- inner_join(pace_ds1, pace_ds2, by = c("ISISID" = "B_UNIQUE_ID1"))
pace_df2 <- inner_join(pace_ds7, pace_df1, by = c("B_UNIQUE_ID1" = "ISISID"))
pace_df3 <- inner_join(pace_ds8, pace_df2, by = c("ABTSRBIID" = "B_UNIQUE_ID1"))
pace_df4 <- left_join(pace_df3, pace_ds10, by = c("ABTSRBIID"))
pace_df <- full_join(pace_df4, pace_ds15, by = c("ABTSRBIID" = "ABTID"))  # ALL PACE DATA in 1 Table
glimpse(pace_df)

# ----------------- CREATE HPOG DATA FRAME -----------------

# Pre-processing for HPOG DS17
hpog_ds17_alt <- hpog_ds17 # for alternative version
#' Function to calculate employment_weekhours as specified:
#' 
#' employment_weekhours: For each unique person(i.e. unique value of ABTSRBIID), 
#' this variable should be set to 0, if CLOSED = 1 (i.e. all job spells for that person have ended),
#' otherwise, set to the sum of F6 across all "open" job spells i.e. when CLOSED = 0
#' 
#' @return A vector of employment_weekhours calculated as per the construction logic.
calculate_employment_weekhours <- function() {
  # initialize a vector to store the calculated values
  employment_weekhours <- numeric(nrow(hpog_ds17))
  
  # iterate over unique values of abtsrbiid
  unique_abtsrbiid <- unique(hpog_ds17$ABTSRBIID)
  for(id in unique_abtsrbiid) {
    # subset the data for the current abtsrbiid
    hpog_ds17_subset <- hpog_ds17[hpog_ds17$ABTSRBIID == id, ]
    
    # if all closed = 1, set employment_weekhours to 0
    if(all(hpog_ds17_subset$CLOSED == 1)) {
      employment_weekhours[hpog_ds17$ABTSRBIID == id] <- 0
    } else {
      # else, calculate the sum of f6 when closed = 0
      employment_weekhours[hpog_ds17$ABTSRBIID == id] <- sum(hpog_ds17_subset$F6[hpog_ds17_subset$CLOSED == 0], na.rm = T)
    }
  }  
  return (employment_weekhours)
}

hpog_ds17 <- hpog_ds17[hpog_ds17$CLOSED == 0, ] # select all rows where closed == 0
hpog_ds17$F5[hpog_ds17$F5 == -1 | hpog_ds17$F5 == -2] <- NA
hpog_ds17$F6[hpog_ds17$F6 == -1 | hpog_ds17$F6 == -2] <- NA
hpog_ds17$employment_weekhours <- calculate_employment_weekhours()
hpog_ds17 <- hpog_ds17 %>% 
  mutate(weekly_earnings_36_variation = case_when(
    F5A_2 == 1 ~ F5A * 5,
    F5A_2 == 2 ~ F5A,
    F5A_2 == 3 ~ F5A / 2,
    F5A_2 == 4 ~ F5A / 4,
    F5A_2 == 5 | F5A_2 == 6 | F5A_2 == 97 | F5A_2 == 98 ~ NA_real_,
    F5A_2 == 7 ~ F5A / 52,
    TRUE ~ NA_real_
  ))
hpog_ds17$employment_weekearnings <- ifelse(!is.na(hpog_ds17$F5) & !is.na(hpog_ds17$F6), 
                                            hpog_ds17$F5 * hpog_ds17$F6, 
                                            ifelse((is.na(hpog_ds17$F5) | is.na(hpog_ds17$F6)) & !is.na(hpog_ds17$F5A), 
                                                   hpog_ds17$weekly_earnings_36_variation, NA))
hpog_ds17 <- hpog_ds17 %>% 
  group_by(ABTSRBIID, employment_weekhours) %>% 
  summarise(employment_weekearnings = sum(employment_weekearnings, na.rm = T)) %>% 
  ungroup()
hpog_ds17$employment_weekhours[hpog_ds17$employment_weekhours == 0] <- NA # if employment_weekhours == 0, set it to NA -- to avoid divide by zero error.
hpog_ds17$employment_weekearnings[hpog_ds17$employment_weekearnings == 0] <- NA # set employment_weekearnings to NA, if employment_weekearnings is 0
hpog_ds17$employment_hourlywage <- ifelse(!is.na(hpog_ds17$employment_weekearnings) & !is.na(hpog_ds17$employment_weekhours), 
                                          hpog_ds17$employment_weekearnings / hpog_ds17$employment_weekhours, NA)
hpog_ds17 <- hpog_ds17 %>% 
  select(ABTSRBIID, employment_hourlywage, employment_weekhours, employment_weekearnings)
# End of  Pre-processing for HPOG DS17

# Start of Pre-processing for HPOG DS17 ALTERNATIVE Version
hpog_ds17_alt <- hpog_ds17_alt[hpog_ds17_alt$CLOSED == 0, ] # select all rows where closed == 0
hpog_ds17_alt$F5[hpog_ds17_alt$F5 == -1 | hpog_ds17_alt$F5 == -2] <- NA
hpog_ds17_alt$F6[hpog_ds17_alt$F6 == -1 | hpog_ds17_alt$F6 == -2] <- NA

# Retain latest open jobs -- 
# when a participant has more than one job starting on the same STARTDATE, 
# randomly pick one open job
# Set Seed to eliminate randomness
set.seed(7)
hpog_ds17_alt <- hpog_ds17_alt %>% 
  group_by(ABTSRBIID) %>% 
  filter(STARTDATE == min(STARTDATE)) %>% 
  ungroup() %>% 
  group_by(ABTSRBIID) %>% 
  sample_n(1) %>% 
  ungroup()
hpog_ds17_alt$employment_weekhours <- hpog_ds17_alt$F6
hpog_ds17_alt <- hpog_ds17_alt %>% 
  mutate(weekly_earnings_36_variation = case_when(
    F5A_2 == 1 ~ F5A * 5,
    F5A_2 == 2 ~ F5A,
    F5A_2 == 3 ~ F5A / 2,
    F5A_2 == 4 ~ F5A / 4,
    F5A_2 == 5 | F5A_2 == 6 | F5A_2 == 97 | F5A_2 == 98 ~ NA_real_,
    F5A_2 == 7 ~ F5A / 52,
    TRUE ~ NA_real_
  ))
hpog_ds17_alt$employment_weekearnings <- ifelse(!is.na(hpog_ds17_alt$F5) & !is.na(hpog_ds17_alt$F6), 
                                            hpog_ds17_alt$F5 * hpog_ds17_alt$F6, 
                                            ifelse((is.na(hpog_ds17_alt$F5) | is.na(hpog_ds17_alt$F6)) & !is.na(hpog_ds17_alt$F5A), 
                                                   hpog_ds17_alt$weekly_earnings_36_variation, NA))

hpog_ds17_alt <- hpog_ds17_alt %>% 
  group_by(ABTSRBIID, employment_weekhours) %>% 
  summarise(employment_weekearnings = sum(employment_weekearnings, na.rm = T))
hpog_ds17_alt$employment_weekhours[hpog_ds17_alt$employment_weekhours == 0] <- NA # if employment_weekhours == 0, set it to NA -- to avoid divide by zero error.
hpog_ds17_alt$employment_weekearnings[hpog_ds17_alt$employment_weekearnings == 0] <- NA # set employment_weekearnings to NA, if employment_weekearnings is 0
hpog_ds17_alt$employment_hourlywage <- ifelse(!is.na(hpog_ds17_alt$employment_weekearnings) & !is.na(hpog_ds17_alt$employment_weekhours), 
                                          hpog_ds17_alt$employment_weekearnings / hpog_ds17_alt$employment_weekhours, NA)
hpog_ds17_alt <- hpog_ds17_alt %>% rename(employment_hourlywage_alt = employment_hourlywage)
hpog_ds17_alt <- hpog_ds17_alt %>% rename(employment_weekhours_alt = employment_weekhours)
hpog_ds17_alt <- hpog_ds17_alt %>% rename(employment_weekearnings_alt = employment_weekearnings)
hpog_ds17 <- left_join(hpog_ds17_alt, hpog_ds17, by = c("ABTSRBIID"))
rm(hpog_ds17_alt)
# END ALT

df1 <- left_join(hpog_ds5, hpog_ds4, by = c("RID"))
df2 <- left_join(df1, hpog_ds6, by = c("RID"))
df3 <- left_join(df2, hpog_ds15, by = c("PACE_ID" = "ABTSRBIID")) %>% 
  select(RID.x, PACE_ID, USERSITE.x, USERSITE.y, LICENSE36, EMPLOYEDATFOLLOWUP, everything())
df4 <- left_join(df3, hpog_ds15, by = c("RID.x" = "RID"))
df4 <- df4 %>% 
  select(RID.x, PACE_ID, ABTSRBIID, USERSITE.x, USERSITE.y, USERSITE, LICENSE36.x, LICENSE36.y, EMPLOYEDATFOLLOWUP.x, EMPLOYEDATFOLLOWUP.y, everything())

# At this point you can drop usersite.y, usersite
df5 <- df4 %>% 
  select(-USERSITE.y, -USERSITE)

# merge license36.x and license36.y;
license36 <- coalesce(df5$LICENSE36.x, df5$LICENSE36.y)
df6 <- df5 %>% 
  select(-LICENSE36.x, -LICENSE36.y)
df6$LICENSE36 <- license36
df6 <- df6 %>% 
  select(RID.x, PACE_ID, ABTSRBIID, USERSITE.x, LICENSE36, EMPLOYEDATFOLLOWUP.x, EMPLOYEDATFOLLOWUP.y, everything())

# similarly merge employedatfolloup.x and employedatfollowup.y
employedatfollowup <- coalesce(df6$EMPLOYEDATFOLLOWUP.x, df6$EMPLOYEDATFOLLOWUP.y)
df7 <- df6 %>% 
  select(-EMPLOYEDATFOLLOWUP.x, -EMPLOYEDATFOLLOWUP.y)
df7$EMPLOYEDATFOLLOWUP <- employedatfollowup
df7 <- df7 %>% 
  select(RID.x, PACE_ID, ABTSRBIID, USERSITE.x, LICENSE36, EMPLOYEDATFOLLOWUP, everything())

# You probably can merge pace_id and ABTSRBIID; but check carefully
abtsrbiid <- coalesce(df7$PACE_ID, df7$ABTSRBIID)
df8 <- df7 %>% 
  select(-ABTSRBIID, -PACE_ID)
df8$ABTSRBIID <- abtsrbiid
df8 <- df8 %>% 
  select(RID.x, ABTSRBIID, USERSITE.x, LICENSE36, EMPLOYEDATFOLLOWUP, everything())

# merging hpog ds17
df9 <- left_join(df8, hpog_ds17, by = c("ABTSRBIID"))

# Remove RID.y
df10 <- df9 %>% 
  select(-RID.y)

# merge hpog ds21
df11 <- left_join(df10, hpog_ds21, by = c("ABTSRBIID"))

# Final HPOG 1.0 DataFrame
hpog_df <- df11
glimpse(hpog_df)
# print(sum(is.na(hpog_df$RID.x))) # total 6 missing

# FINAL MERGE: hpog_df + pace_df
data <- full_join(hpog_df, pace_df, by = c("ABTSRBIID"))

# ----------------- CREATE STUDY ID TO DISTINGUISH HPOG 1.0 & PACE -----------------
data$STUDY_HPOG <- 0
data$STUDY_PACE <- 0

pace_and_hpog_obs <- inner_join(hpog_ds15, df2, by = c("ABTSRBIID" = "PACE_ID"))

# HPOG 1.0 & PACE BOTH
matching_ids <- data$ABTSRBIID %in% pace_and_hpog_obs$ABTSRBIID # identify the rows in 'data' frame where the ABTSRBIID matches with the ABTSRBIID in 'pace_and_hpog_obs'
data$STUDY_HPOG[matching_ids] <- 1 # Set the value of STUDY_HPOG to 1 for the rows where id matches
data$STUDY_PACE[matching_ids] <- 1 # Set the value of STUDY_PACE to 1 for the rows where id matches

# HPOG 1.0 only
hpog_only <- anti_join(df2, hpog_ds15, by = c("PACE_ID" = "ABTSRBIID"))
matching_ids <- data$RID.x %in% hpog_only$RID
data$STUDY_HPOG[matching_ids] <- 1

# PACE only
pace_only <- anti_join(pace_df, hpog_ds15, by = c("ABTSRBIID"))
matching_ids <- data$ABTSRBIID %in% pace_only$ABTSRBIID
data$STUDY_PACE[matching_ids] <- 1

# HPOG 1.0 and PACE both
# print(sum(data$STUDY_HPOG == 1 & data$STUDY_PACE == 1)) 

# ----------------- VARIABLE MANIPULATION -----------------

data_copy <- data # backup copy as restore point

# A. DEPENDENT VARIABLES
# ======================

# credential_15 [HPOG]
data$CREDENTIAL[!(data$CREDENTIAL %in% c(0, 1))] <- NA # set to NA if credential ~in (0, 1)
names(data)[names(data) == "CREDENTIAL"] <- "credential_15"

# credential_36 [HPOG]
data$LICENSE36[!(data$LICENSE36 %in% c(0, 1))] <- NA
names(data)[names(data) == "LICENSE36"] <- "credential_36"

# credential_72 [HPOG]
data$EDPROGRESS_ANYCREDENTIAL[!(data$EDPROGRESS_ANYCREDENTIAL %in% c(0, 1))] <- NA
names(data)[names(data) == "EDPROGRESS_ANYCREDENTIAL"] <- "credential_72"

# currently_working_15 [HPOG]
data$CURRENTWORK[!(data$CURRENTWORK %in% c(0, 1))] <- NA
names(data)[names(data) == "CURRENTWORK"] <- "currently_working_15"

# currently_working_36 [HPOG & PACE]
data$EMPLOYEDATFOLLOWUP.x[!(data$EMPLOYEDATFOLLOWUP.x %in% c(0, 1))] <- NA # For HPOG
names(data)[names(data) == "EMPLOYEDATFOLLOWUP.x"] <- "currently_working_36_hpog"

data$EMPLOYEDATFOLLOWUP.y[!(data$EMPLOYEDATFOLLOWUP.y %in% c(0, 1))] <- NA # For PACE
names(data)[names(data) == "EMPLOYEDATFOLLOWUP.y"] <- "currently_working_36_pace"

currently_working_36 <- coalesce(data$currently_working_36_hpog, data$currently_working_36_pace)
data$currently_working_36 <- currently_working_36
data <- data %>% 
  select(-currently_working_36_hpog, -currently_working_36_pace)

# currently_working_72 [HPOG & PACE]
data$EMPLOYMENT_EMPLOYED[!(data$EMPLOYMENT_EMPLOYED %in% c(0, 1))] <- NA # For HPOG
names(data)[names(data) == "EMPLOYMENT_EMPLOYED"] <- "currently_working_72_hpog"

data$EMPLOYEDATFOLLOWUP_LO[!(data$EMPLOYEDATFOLLOWUP_LO %in% c(0, 1))] <- NA # For PACE
names(data)[names(data) == "EMPLOYEDATFOLLOWUP_LO"] <- "currently_working_72_pace"

data$currently_working_72 <- coalesce(data$currently_working_72_hpog, data$currently_working_72_pace)
data <- data %>% 
  select(-currently_working_72_hpog, -currently_working_72_pace)

# hours_worked_per_week_15
data$Q36A[data$Q36A %in% c(-97, -98, -99)] <- NA # HPOG
data$CURRWORKHOURS[data$CURRWORKHOURS %in% c(-95)] <- NA # PACE
data$hours_worked_per_week_15 <- coalesce(data$Q36A, data$CURRWORKHOURS)
data <- data %>% 
  select(-CURRWORKHOURS, -Q36A)

# hourly_wage_rate_15
data$Q36B[data$Q36B %in% c(-99, -98, -97)] <- NA
data$CURRHOURLYWAGE[data$CURRHOURLYWAGE %in% c(-95)] <- NA
data$hourly_wage_rate_15 <- coalesce(data$Q36B, data$CURRHOURLYWAGE)
data <- data %>% 
  select(-Q36B, -CURRHOURLYWAGE)

# weekly_earnings_15 = hours_worked_per_week_15 * hourly_wage_rate_15
data$weekly_earnings_15_hpog <- data$hours_worked_per_week_15 * data$hourly_wage_rate_15

# 36 month hours/wages/earnings variables
show_36month_hours_wages_earnings_variables <- data %>% 
  select(RID.x, ABTSRBIID, STUDY_HPOG, STUDY_PACE, employment_weekhours, employment_weekhours_pace, employment_weekearnings, employment_weekearnings_pace, employment_hourlywage, employment_hourlywage_pace) %>% 
  filter(data$employment_hourlywage != data$employment_hourlywage_pace)

# hours_worked_per_week_36 
data$hours_worked_per_week_36 <- coalesce(data$employment_weekhours, data$employment_weekhours_pace)
data <- data %>% 
  select(-employment_weekhours, -employment_weekhours_pace)

# hourly_wage_rate_36
# print(sum(data$employment_hourlywage != data$employment_hourlywage_pace, na.rm = T)) # 23
data$hourly_wage_rate_36 <- coalesce(data$employment_hourlywage_pace, data$employment_hourlywage) # Retain pace values for the 23 unmatched obs.
data <- data %>% 
  select(-employment_hourlywage, -employment_hourlywage_pace)

# weekly_earnings_36 = hours_worked_per_week_36 * hourly_wage_rate_36
data$weekly_earnings_36 <- data$hours_worked_per_week_36 * data$hourly_wage_rate_36

# hours_worked_per_week_72
data$EMPLOYMENT_WEEKHOURS[data$EMPLOYMENT_WEEKHOURS == -95] <- NA
data$CURRENTHOURS_LO[data$CURRENTHOURS_LO == -95] <- NA
data$hours_worked_per_week_72 <- coalesce(data$EMPLOYMENT_WEEKHOURS, data$CURRENTHOURS_LO)
data <- data %>% 
  select(-EMPLOYMENT_WEEKHOURS, -CURRENTHOURS_LO)

# hourly_wage_rate_72
data <- data %>% 
  mutate(employment_hourlywage_72_month = ifelse(CURRENTWAGERATE_LO != EMPLOYMENT_HOURLYWAGE, 
                                                 CURRENTWAGERATE_LO, EMPLOYMENT_HOURLYWAGE))
data$EMPLOYMENT_HOURLYWAGE[data$EMPLOYMENT_HOURLYWAGE %in% c(-95, -96)] <- NA
data$CURRENTWAGERATE_LO[data$CURRENTWAGERATE_LO %in% c(-96, -95)] <- NA
data$hourly_wage_rate_72 <- coalesce(data$CURRENTWAGERATE_LO, data$employment_hourlywage_72_month)
data <- data %>% 
  select(-CURRENTWAGERATE_LO, -EMPLOYMENT_HOURLYWAGE, -employment_hourlywage_72_month)

# weekly_earnings_72
data$weekly_earnings_72 <- data$hours_worked_per_week_72 * data$hourly_wage_rate_72 

# hours_worked_per_week_alt_36
# print(sum(data$employment_weekhours_alt != data$CURRENTHOURS, na.rm = T)) # 47
data$hours_worked_per_week_alt_36 <- coalesce(data$CURRENTHOURS, data$employment_weekhours_alt)
data <- data %>% 
  select(-CURRENTHOURS, -employment_weekhours_alt)

# hourly_wage_rate_alt_36
# print(sum(data$employment_hourlywage_alt != data$CURRENTWAGERATE, na.rm = T)) # 161
data$hourly_wage_rate_alt_36 <- coalesce(data$CURRENTWAGERATE, data$employment_hourlywage_alt)
data <- data %>% 
  select(-CURRENTWAGERATE, -employment_hourlywage_alt)

# weekly_earnings_alt_36
data$weekly_earnings_alt_36 <- data$hours_worked_per_week_alt_36 * data$hourly_wage_rate_alt_36

# B. INDPENDENT VARIABLES
# =======================

# treatment_group_0
data$RANDOM_ASSIGNMENT[data$RANDOM_ASSIGNMENT %in% c(1, 2)] <- 1
data$treatment_group_0 <- coalesce(data$RANDOM_ASSIGNMENT, data$R_TREATMENT_STATUS)
data <- data %>% 
  select(-RANDOM_ASSIGNMENT, -R_TREATMENT_STATUS)

# Age calculation logic
calculate_age <- function(ra_date_month, ra_date_year, dob_month, dob_year) {
  age <- ra_date_year - dob_year
  age <- age - (ra_date_month < dob_month)
  return (age)
}

# age_in_years_0
data$RA_DATE_MONTH[data$RA_DATE_MONTH == -95] <- NA
data$RA_DATE_YEAR[data$RA_DATE_YEAR == -95] <- NA
data$DOB_YEAR[data$DOB_YEAR == -95] <- NA
data$age_in_years_0 <- calculate_age(data$RA_DATE_MONTH, data$RA_DATE_YEAR, data$DOB_MONTH, data$DOB_YEAR)
# print(mean(data$age_in_years_0, na.rm = T)) # 31.56385
data <- data %>% 
  select(-RA_DATE_MONTH, -RA_DATE_YEAR, -DOB_MONTH, -DOB_YEAR)

# age_LT21_0
# age_21To24_0
# age_25To34_0
# age_GE35_0
data$age_LT21_0 <- ifelse(data$age_in_years_0 >= 0 & data$age_in_years_0 < 21, 1, 0)
data$age_21To24_0 <- ifelse(data$age_in_years_0 >= 21 & data$age_in_years_0 < 25, 1, 0)
data$age_25To34_0 <- ifelse(data$age_in_years_0 >= 25 & data$age_in_years_0 < 35, 1, 0)
data$age_GE35_0 <- ifelse(data$age_in_years_0 >= 35, 1, 0)

data$age_LT21_0_pace <- ifelse(data$AGE == 1, 1, 0)
data$age_21To24_0_pace <- ifelse(data$AGE == 2, 1, 0)
data$age_25To34_0_pace <- ifelse(data$AGE == 3, 1, 0)
data$age_GE35_0_pace <- ifelse(data$AGE == 4, 1, 0)

data$age_LT21_0 <- coalesce(data$age_LT21_0, data$age_LT21_0_pace)
data$age_21To24_0 <- coalesce(data$age_21To24_0, data$age_21To24_0_pace)
data$age_25To34_0 <- coalesce(data$age_25To34_0, data$age_25To34_0_pace)
data$age_GE35_0 <- coalesce(data$age_GE35_0, data$age_GE35_0_pace)

data <- data %>% 
  select(-age_LT21_0_pace, -age_21To24_0_pace, -age_25To34_0_pace, -age_GE35_0_pace)

# ethnicity_hispanic_0 - ASK about the frequency of ethnicity, on the xls file, it shows 23.6% -- infoloss
data$ETHNICITY[!(data$ETHNICITY %in% c(1, 2))] <- NA
data$ETHNICITY[data$ETHNICITY == 2] <- 0
data$B09_HISPANIC[data$B09_HISPANIC == -95] <- NA
data$B09_HISPANIC[data$B09_HISPANIC == 1] <- 0
data$B09_HISPANIC[data$B09_HISPANIC %in% c(2, 3, 4, 5)] <- 1
# Handle the exceptional cases:
# Set to NA when data$B09_HISPANIC != data$ETHNICITY
data$B09_HISPANIC[data$ABTSRBIID == 'WA40181'] <- NA
data$ETHNICITY[data$ABTSRBIID == 'WA40181'] <- NA
data$B09_HISPANIC[data$ABTSRBIID == 'WA40545'] <- NA
data$ETHNICITY[data$ABTSRBIID == 'WA40545'] <- NA
data$B09_HISPANIC[data$ABTSRBIID == 'IL10470'] <- NA
data$ETHNICITY[data$ABTSRBIID == 'IL10470'] <- NA
data$ethnicity_hispanic_0 <- coalesce(data$ETHNICITY, data$B09_HISPANIC)
data <- data %>% 
  select(-ETHNICITY, -B09_HISPANIC)

# race_white_0 -- infoloss; when race_w != b10_race_white, set both to NA
data$RACE_W[data$RACE_W == -95] <- NA
data$RACE_W[data$RACE_W == 2] <- 0

filter1 <- data %>% 
  select(RID.x, ABTSRBIID, STUDY_HPOG, STUDY_PACE, RACE_W, B10_RACE_WHITE) %>% 
  filter(data$RACE_W == 1 & data$B10_RACE_WHITE == 0)
data$RACE_W[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$B10_RACE_WHITE[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
filter2 <- data %>% 
  select(RID.x, ABTSRBIID, STUDY_HPOG, STUDY_PACE, RACE_W, B10_RACE_WHITE) %>% 
  filter(data$RACE_W == 0 & data$B10_RACE_WHITE == 1)
data$RACE_W[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$B10_RACE_WHITE[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$race_white_0 <- coalesce(data$RACE_W, data$B10_RACE_WHITE)
data <- data %>% 
  select(-RACE_W, -B10_RACE_WHITE)

# race_black_0 -- infoloss; when race_b != b10_race_black; set both to NA
data$RACE_B[data$RACE_B == -95] <- NA
data$RACE_B[data$RACE_B == 2] <- 0
filter1 <- data %>% 
  select(RID.x, ABTSRBIID, STUDY_HPOG, STUDY_PACE, RACE_B, B10_RACE_BLACK) %>% 
  filter(data$RACE_B == 1 & data$B10_RACE_BLACK == 0)
data$RACE_B[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$B10_RACE_BLACK[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
filter2 <- data %>% 
  select(RID.x, ABTSRBIID, STUDY_HPOG, STUDY_PACE, RACE_B, B10_RACE_BLACK) %>% 
  filter(data$RACE_B == 0 & data$B10_RACE_BLACK == 1)
data$RACE_B[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$B10_RACE_BLACK[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$race_black_0 <- coalesce(data$RACE_B, data$B10_RACE_BLACK)
data <- data %>% 
  select(-RACE_B,-B10_RACE_BLACK)

# race_asian_0 -- infoloss, when race_a != b10_race_asian; set both to NA
data$RACE_A[data$RACE_A == -95] <- NA
data$RACE_A[data$RACE_A == 2] <- 0
filter1 <- data %>% 
  select(ABTSRBIID, RACE_A, B10_RACE_ASIAN) %>% 
  filter(data$RACE_A == 1 & data$B10_RACE_ASIAN == 0)
data$RACE_A[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$B10_RACE_ASIAN[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
filter2 <- data %>% 
  select(ABTSRBIID, RACE_A, B10_RACE_ASIAN) %>% 
  filter(data$RACE_A == 0 & data$B10_RACE_ASIAN == 1)
data$RACE_A[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$B10_RACE_ASIAN[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$race_asian_0 <- coalesce(data$RACE_A, data$B10_RACE_ASIAN)
data <- data %>% 
  select(-RACE_A,-B10_RACE_ASIAN)

# race_american_indian_0 -- infoloss, when RACE_NA != B10_RACE_AM_INDIAN; set both to NA
data$RACE_NA[data$RACE_NA == -95] <- NA
data$RACE_NA[data$RACE_NA == 2] <- 0
filter1 <- data %>% 
  select(ABTSRBIID, RACE_NA, B10_RACE_AM_INDIAN) %>% 
  filter(data$RACE_NA == 1 & data$B10_RACE_AM_INDIAN == 0)
data$RACE_NA[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$B10_RACE_AM_INDIAN[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
filter2 <- data %>% 
  select(ABTSRBIID, RACE_NA, B10_RACE_AM_INDIAN) %>% 
  filter(data$RACE_NA == 0 & data$B10_RACE_AM_INDIAN == 1)
data$RACE_NA[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$B10_RACE_AM_INDIAN[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$race_american_indian_0 <- coalesce(data$RACE_NA, data$B10_RACE_AM_INDIAN)
data <- data %>% 
  select(-RACE_NA,-B10_RACE_AM_INDIAN)

# race_pacific_islander_0 -- infoloss, when RACE_HPI != B10_RACE_PACIFIC; set both to NA
data$RACE_HPI[data$RACE_HPI == -95] <- NA
data$RACE_HPI[data$RACE_HPI == 2] <- 0
filter2 <- data %>% 
  select(ABTSRBIID, RACE_HPI, B10_RACE_PACIFIC) %>% 
  filter(data$RACE_HPI == 0 & data$B10_RACE_PACIFIC == 1)
data$RACE_HPI[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$B10_RACE_PACIFIC[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$race_pacific_islander_0 <- coalesce(data$RACE_HPI, data$B10_RACE_PACIFIC)
data <- data %>% 
  select(-RACE_HPI,-B10_RACE_PACIFIC)

# Grade variables -- what to do when b23_grades == 1 i.e.Did not attend high school in the U.S.
data$B23_GRADES[data$B23_GRADES == -95] <- NA
data$grades_mostly_A_0 <- ifelse(data$B23_GRADES == 2, 1, 0) # grades_mostly_A_0
data$grades_mostly_B_0 <- ifelse(data$B23_GRADES == 3, 1, 0) # grades_mostly_B_0
data$grades_mostly_C_0 <- ifelse(data$B23_GRADES == 4, 1, 0) # grades_mostly_C_0
data$grades_mostly_D_0 <- ifelse(data$B23_GRADES == 5, 1, 0) # grades_mostly_D_0
data$grades_mostly_E_0 <- ifelse(data$B23_GRADES == 6, 1, 0) # grades_mostly_E_0
data <- data %>% 
  select(-B23_GRADES)

# educ_no_hs_credential_0 -- ASK ROB about this construction logic
# For HPOG:
# = NA if both DEGREE_RECEIVED_GED & DEGREE_RECEIVED_HS = -95; = 0 if DEGREE_RECEIVED_GED or DEGREE_RECEIVED_HS = 1; ELSE = 0
# For PACE: 
# = 1 if b17_educ in (1, 2); ELSE = NA if b17_educ = -95; ELSE = 0
# HPOG
data$DEGREE_RECEIVED_GED[data$DEGREE_RECEIVED_GED == -95] <- NA
data$DEGREE_RECEIVED_HS[data$DEGREE_RECEIVED_HS == -95] <- NA
data$educ_no_hs_credential_hpog <- ifelse(is.na(data$DEGREE_RECEIVED_GED) & is.na(data$DEGREE_RECEIVED_HS), NA, ifelse(data$DEGREE_RECEIVED_GED == 1 | data$DEGREE_RECEIVED_HS == 1, 0, 1))
# PACE
data$B17_EDUC[data$B17_EDUC == -95] <- NA
data$educ_no_hs_credential_pace <- ifelse(data$B17_EDUC %in% c(1, 2), 1, ifelse(is.na(data$B17_EDUC), NA, 0))
filter1 <- data %>% 
  select(ABTSRBIID, educ_no_hs_credential_hpog, educ_no_hs_credential_pace) %>% 
  filter(data$educ_no_hs_credential_hpog == 1 & data$educ_no_hs_credential_pace == 0)
data$educ_no_hs_credential_hpog[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$educ_no_hs_credential_pace[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
filter2 <- data %>% 
  select(ABTSRBIID, educ_no_hs_credential_hpog, educ_no_hs_credential_pace) %>% 
  filter(data$educ_no_hs_credential_hpog == 0 & data$educ_no_hs_credential_pace == 1)
data$educ_no_hs_credential_hpog[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$educ_no_hs_credential_pace[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
# VERIFY
show <- data %>% 
  select(RID.x, ABTSRBIID, DEGREE_RECEIVED_GED, DEGREE_RECEIVED_HS, educ_no_hs_credential_hpog, B17_EDUC, educ_no_hs_credential_pace)
# print(sum(data$educ_no_hs_credential_hpog == 0 & data$educ_no_hs_credential_pace == 1, na.rm = TRUE))
# COALESCE
data$educ_no_hs_credential_0 <- coalesce(data$educ_no_hs_credential_hpog, data$educ_no_hs_credential_pace)
data <- data %>% 
  select(-educ_no_hs_credential_hpog, -educ_no_hs_credential_pace)

# educ_ged_or_alternative_crendetial_0 -- show this to rob
# print(sum(data$educ_ged_or_alternative_hpog == 1 & data$educ_ged_or_alternative_pace == 0, na.rm = TRUE)) # outputs 134
# print(sum(data$educ_ged_or_alternative_hpog == 0 & data$educ_ged_or_alternative_pace == 1, na.rm = TRUE)) # outputs 16
data$educ_ged_or_alternative_hpog <- ifelse(!(data$DEGREE_RECEIVED_GED %in% c(1, 2)), NA, ifelse(data$DEGREE_RECEIVED_GED == 1, 1, 0))
data$educ_ged_or_alternative_pace <- ifelse(data$B17_EDUC == 3, 1, ifelse(is.na(data$B17_EDUC), NA, 0))
filter1 <- data %>% 
  select(ABTSRBIID, educ_ged_or_alternative_hpog, educ_ged_or_alternative_pace) %>% 
  filter(data$educ_ged_or_alternative_hpog == 1 & data$educ_ged_or_alternative_pace == 0)
data$educ_ged_or_alternative_hpog[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$educ_ged_or_alternative_pace[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
filter2 <- data %>% 
  select(ABTSRBIID, educ_ged_or_alternative_hpog, educ_ged_or_alternative_pace) %>% 
  filter(data$educ_ged_or_alternative_hpog == 0 & data$educ_ged_or_alternative_pace == 1)
data$educ_ged_or_alternative_hpog[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$educ_ged_or_alternative_pace[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$educ_ged_or_alternative_credential_0 <- coalesce(data$educ_ged_or_alternative_hpog, data$educ_ged_or_alternative_pace)
show_educ_ged_or_alternative_crendetial_0 <- data %>% 
  select(RID.x, ABTSRBIID, DEGREE_RECEIVED_GED, B17_EDUC, educ_ged_or_alternative_hpog, educ_ged_or_alternative_pace, educ_ged_or_alternative_credential_0)
data <- data %>% 
  select(-educ_ged_or_alternative_hpog, educ_ged_or_alternative_pace)

# educ_regular_high_school_diploma_0 -- show this to rob
# print(sum(data$educ_regular_high_school_diploma_hpog == 1 & data$educ_regular_high_school_diploma_pace == 0, na.rm = TRUE)) # outputs 678
# print(sum(data$educ_regular_high_school_diploma_hpog == 0 & data$educ_regular_high_school_diploma_pace == 1, na.rm = TRUE)) # outputs 7
data$educ_regular_high_school_diploma_hpog <- ifelse(!(data$DEGREE_RECEIVED_HS %in% c(1,2)), NA, ifelse(data$DEGREE_RECEIVED_HS == 1, 1, 0))
data$educ_regular_high_school_diploma_pace <- ifelse(data$B17_EDUC == 4, 1, ifelse(is.na(data$B17_EDUC), NA, 0))
filter1 <- data %>% 
  select(ABTSRBIID, educ_regular_high_school_diploma_hpog, educ_regular_high_school_diploma_pace) %>% 
  filter(data$educ_regular_high_school_diploma_hpog == 1 & data$educ_regular_high_school_diploma_pace == 0)
data$educ_regular_high_school_diploma_hpog[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$educ_regular_high_school_diploma_pace[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
filter2 <- data %>% 
  select(ABTSRBIID, educ_regular_high_school_diploma_hpog, educ_regular_high_school_diploma_pace) %>% 
  filter(data$educ_regular_high_school_diploma_hpog == 0 & data$educ_regular_high_school_diploma_pace == 1)
data$educ_regular_high_school_diploma_hpog[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$educ_regular_high_school_diploma_pace[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$educ_regular_high_school_diploma_0 <- coalesce(data$educ_regular_high_school_diploma_hpog, data$educ_regular_high_school_diploma_pace)
show_educ_regular_high_school_diploma_0 <- data %>% 
  select(RID.x, ABTSRBIID, DEGREE_RECEIVED_GED, B17_EDUC, educ_regular_high_school_diploma_hpog, educ_regular_high_school_diploma_pace, educ_regular_high_school_diploma_0)
data <- data %>% 
  select(-educ_regular_high_school_diploma_hpog, -educ_regular_high_school_diploma_pace)

# educ_some_college_0
# print(sum(data$educ_some_college_hpog == 1 & data$educ_some_college_pace == 0, na.rm = TRUE)) # outputs 49
# print(sum(data$educ_some_college_hpog == 0 & data$educ_some_college_pace == 1, na.rm = TRUE)) # 175
data$educ_some_college_hpog <- ifelse(data$HIGHEST_EDUCATION < 6 | data$HIGHEST_EDUCATION > 17, NA, ifelse(data$HIGHEST_EDUCATION %in% c(13, 14, 15), 1, 0))
data$educ_some_college_pace <- ifelse(data$B17_EDUC %in% c(5, 6, 7), 1, ifelse(is.na(data$B17_EDUC), NA, 0))
filter1 <- data %>% 
  select(ABTSRBIID, educ_some_college_hpog, educ_some_college_pace) %>% 
  filter(data$educ_some_college_hpog == 1 & data$educ_some_college_pace == 0)
data$educ_some_college_hpog[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$educ_some_college_pace[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
filter2 <- data %>% 
  select(ABTSRBIID, educ_some_college_hpog, educ_some_college_pace) %>% 
  filter(data$educ_some_college_hpog == 0 & data$educ_some_college_pace == 1)
data$educ_some_college_hpog[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$educ_some_college_pace[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$educ_some_college_0 <- coalesce(data$educ_some_college_hpog, data$educ_some_college_pace)
show_educ_some_college_0 <- data %>% 
  select(RID.x, ABTSRBIID, HIGHEST_EDUCATION, B17_EDUC, educ_some_college_hpog, educ_some_college_pace, educ_some_college_0)
data <- data %>% 
  select(-educ_some_college_hpog, -educ_some_college_pace)

# educ_bachelors_degree_or_higher_0
# print(sum(data$educ_bachelors_degree_or_higher_hpog == 1 & data$educ_bachelors_degree_or_higher_pace == 0, na.rm = TRUE)) # 12
# print(sum(data$educ_bachelors_degree_or_higher_hpog == 0 & data$educ_bachelors_degree_or_higher_pace == 1, na.rm = TRUE)) # 19
data$educ_bachelors_degree_or_higher_hpog <- ifelse(data$HIGHEST_EDUCATION < 6 | data$HIGHEST_EDUCATION > 17, NA, ifelse(data$HIGHEST_EDUCATION %in% c(16, 17), 1, 0))
data$educ_bachelors_degree_or_higher_pace <- ifelse(data$B17_EDUC == 8, 1, ifelse(is.na(data$B17_EDUC), NA, 0))
filter1 <- data %>% 
  select(ABTSRBIID, educ_bachelors_degree_or_higher_hpog, educ_bachelors_degree_or_higher_pace) %>% 
  filter(data$educ_bachelors_degree_or_higher_hpog == 1 & data$educ_bachelors_degree_or_higher_pace == 0)
data$educ_bachelors_degree_or_higher_hpog[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$educ_bachelors_degree_or_higher_pace[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
filter2 <- data %>% 
  select(ABTSRBIID, educ_bachelors_degree_or_higher_hpog, educ_bachelors_degree_or_higher_pace) %>% 
  filter(data$educ_bachelors_degree_or_higher_hpog == 0 & data$educ_bachelors_degree_or_higher_pace == 1)
data$educ_bachelors_degree_or_higher_hpog[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$educ_bachelors_degree_or_higher_pace[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$educ_bachelors_degree_or_higher_0 <- coalesce(data$educ_bachelors_degree_or_higher_hpog, data$educ_bachelors_degree_or_higher_pace)
show_educ_bachelors_degree_or_higher_0 <- data %>% 
  select(RID.x, ABTSRBIID, HIGHEST_EDUCATION, B17_EDUC, educ_bachelors_degree_or_higher_hpog, educ_bachelors_degree_or_higher_pace, educ_bachelors_degree_or_higher_0)
data <- data %>% 
  select(-educ_bachelors_degree_or_higher_hpog, -educ_bachelors_degree_or_higher_pace)

# occupational_license_certification
data$occupational_license_certification <- ifelse(!data$DEGREE_RECEIVED_CERTIFICATE %in% c(1, 2), NA, ifelse(data$DEGREE_RECEIVED_CERTIFICATE == 1, 1, 0))
data <- data %>% 
  select(-DEGREE_RECEIVED_CERTIFICATE)

# postsec_voc_tech_certificate
data$postsec_voc_tech_certificate <- ifelse(data$B18_VOCTECH_CERT == 1, 1, ifelse(data$B18_VOCTECH_CERT == -95, NA, 0))
data <- data %>% 
  select(-B18_VOCTECH_CERT)

# sex_male_0
# print(sum(data$sex_male_hpog == 1 & data$sex_male_pace == 0, na.rm = TRUE)) # 1
# print(sum(data$sex_male_hpog == 0 & data$sex_male_pace == 1, na.rm = TRUE)) # 1
data$sex_male_hpog <- ifelse(!data$GENDER %in% c(1, 2), NA, ifelse(data$GENDER == 1, 1, 0))
data$sex_male_pace <- ifelse(data$B07_SEX == 0, 1, ifelse(data$B07_SEX == -95, NA, 0))
filter1 <- data %>% 
  select(ABTSRBIID, sex_male_hpog, sex_male_pace) %>% 
  filter(data$sex_male_hpog == 1 & data$sex_male_pace == 0)
data$sex_male_hpog[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$sex_male_pace[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
filter2 <- data %>% 
  select(ABTSRBIID, sex_male_hpog, sex_male_pace) %>% 
  filter(data$sex_male_hpog == 0 & data$sex_male_pace == 1)
data$sex_male_hpog[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$sex_male_pace[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$sex_male_0 <- coalesce(data$sex_male_hpog, data$sex_male_pace)
data <- data %>% 
  select(-GENDER, -sex_male_hpog, -sex_male_pace)

# marstat_married_0
data$marstat_married_hpog <- ifelse(!data$MARITAL %in% c(1, 2, 3, 4), NA, ifelse(data$MARITAL == 1, 1, 0))
data$marstat_married_pace <- ifelse(!data$B08_MARSTAT %in% c(1, 2, 3, 4, 5), NA, ifelse(data$B08_MARSTAT == 1, 1, 0))
# print(sum(data$marstat_married_hpog == 1 & data$marstat_married_pace == 0, na.rm = TRUE)) # 4
# print(sum(data$marstat_married_hpog == 0 & data$marstat_married_pace == 1, na.rm = TRUE)) # 14
filter1 <- data %>% 
  select(ABTSRBIID, marstat_married_hpog, marstat_married_pace) %>% 
  filter(data$marstat_married_hpog == 1 & data$marstat_married_pace == 0)
data$marstat_married_hpog[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$marstat_married_pace[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
filter2 <- data %>% 
  select(ABTSRBIID, marstat_married_hpog, marstat_married_pace) %>% 
  filter(data$marstat_married_hpog == 0 & data$marstat_married_pace == 1)
data$marstat_married_hpog[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$marstat_married_pace[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$marstat_married_0 <- coalesce(data$marstat_married_hpog, data$marstat_married_pace)
show_marstat_married_0 <- data %>% 
  select(RID.x, ABTSRBIID, MARITAL, B08_MARSTAT, marstat_married_hpog, marstat_married_pace, marstat_married_0)
data <- data %>% 
  select(-marstat_married_hpog, marstat_married_pace)

# marstat_widowed_0
data$marstat_widowed_hpog <- ifelse(!data$MARITAL %in% c(1, 2, 3, 4), NA, ifelse(data$MARITAL == 3, 1, 0))
data$marstat_widowed_pace <- ifelse(!data$B08_MARSTAT %in% c(1, 2, 3, 4, 5), NA, ifelse(data$B08_MARSTAT == 2, 1, 0))
# print(sum(data$marstat_widowed_hpog == 1 & data$marstat_widowed_pace == 0, na.rm = T)) # 0
# print(sum(data$marstat_widowed_hpog == 0 & data$marstat_widowed_pace == 1, na.rm = T)) # 1
filter1 <- data %>% 
  select(ABTSRBIID, marstat_widowed_hpog, marstat_widowed_pace) %>% 
  filter(data$marstat_widowed_hpog == 0 & data$marstat_widowed_pace == 1)
data$marstat_widowed_hpog[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$marstat_widowed_pace[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$marstat_widowed_0 <- coalesce(data$marstat_widowed_hpog, data$marstat_widowed_pace)
show_marstat_widowed_0 <- data %>% 
  select(RID.x, ABTSRBIID, MARITAL, B08_MARSTAT, marstat_widowed_hpog, marstat_widowed_pace, marstat_widowed_0)
data <- data %>% 
  select(-marstat_widowed_hpog, - marstat_widowed_pace)

# marstat_divorced_or_separated_0
data$marstat_divorced_or_separated_hpog <- ifelse(!data$MARITAL %in% c(1, 2, 3, 4), NA, ifelse(data$MARITAL == 2, 1, 0))
data$marstat_divorced_or_separated_pace <- ifelse(!data$B08_MARSTAT %in% c(1, 2, 3, 4, 5), NA, ifelse(data$B08_MARSTAT %in% c(3, 4), 1, 0))
# print(sum(data$marstat_divorced_or_separated_hpog == 1 & data$marstat_divorced_or_separated_pace == 0, na.rm = T)) # 5
# print(sum(data$marstat_divorced_or_separated_hpog == 0 & data$marstat_divorced_or_separated_pace == 1, na.rm = T)) # 5
filter1 <- data %>% 
  select(ABTSRBIID, marstat_divorced_or_separated_hpog, marstat_divorced_or_separated_pace) %>% 
  filter(data$marstat_divorced_or_separated_hpog == 1 & data$marstat_divorced_or_separated_pace == 0)
data$marstat_divorced_or_separated_hpog[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$marstat_divorced_or_separated_pace[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
filter2 <- data %>% 
  select(ABTSRBIID, marstat_divorced_or_separated_hpog, marstat_divorced_or_separated_pace) %>% 
  filter(data$marstat_divorced_or_separated_hpog == 0 & data$marstat_divorced_or_separated_pace == 1)
data$marstat_divorced_or_separated_hpog[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$marstat_divorced_or_separated_pace[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$marstat_divorced_or_separated_0 <- coalesce(data$marstat_divorced_or_separated_hpog, data$marstat_divorced_or_separated_pace)
show_marstat_divorced_or_separated_0 <- data %>% 
  select(RID.x, ABTSRBIID, MARITAL, B08_MARSTAT, marstat_divorced_or_separated_hpog, marstat_divorced_or_separated_pace, marstat_divorced_or_separated_0)
data <- data %>% 
  select(-marstat_divorced_or_separated_hpog, -marstat_divorced_or_separated_pace)

# marstat_never_married_0
data$marstat_never_married_hpog <- ifelse(!data$MARITAL %in% c(1, 2, 3, 4), NA, ifelse(data$MARITAL == 4, 1, 0))
data$marstat_never_married_pace <- ifelse(!data$B08_MARSTAT %in% c(1, 2, 3, 4, 5), NA, ifelse(data$B08_MARSTAT == 5, 1, 0))
# print(sum(data$marstat_never_married_hpog == 1 & data$marstat_never_married_pace == 0, na.rm = T)) # 15
# print(sum(data$marstat_never_married_hpog == 0 & data$marstat_never_married_pace == 1, na.rm = T)) # 1
filter1 <- data %>% 
  select(ABTSRBIID, marstat_never_married_hpog, marstat_never_married_pace) %>% 
  filter(data$marstat_never_married_hpog == 1 & data$marstat_never_married_pace == 0)
data$marstat_never_married_hpog[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$marstat_never_married_pace[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
filter2 <- data %>% 
  select(ABTSRBIID, marstat_never_married_hpog, marstat_never_married_pace) %>% 
  filter(data$marstat_never_married_hpog == 0 & data$marstat_never_married_pace == 1)
data$marstat_never_married_hpog[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$marstat_never_married_pace[data$ABTSRBIID %in% filter2$ABTSRBIID] <- NA
data$marstat_never_married_0 <- coalesce(data$marstat_never_married_hpog, data$marstat_never_married_pace)
show_marstat_never_married_0 <- data %>% 
  select(RID.x, ABTSRBIID, MARITAL, B08_MARSTAT, marstat_never_married_hpog, marstat_never_married_pace, marstat_never_married_0)
data <- data %>% 
  select(-marstat_never_married_hpog, -marstat_never_married_pace)

# number_of_children_0 -- 248 observations where number of children are reported differently across studies
data$number_of_children_hpog <- ifelse(!data$DEPENDENT_CHILDREN %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), NA, data$DEPENDENT_CHILDREN)
data$number_of_children_pace <- ifelse(!data$B15A_NUM_CHN %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 16), NA, ifelse(data$B15A_NUM_CHN %in% c(10, 11, 16), 9, data$B15A_NUM_CHN))
# print(sum(data$number_of_children_hpog != data$number_of_children_pace, na.rm = T)) # 248
filter1 <- data %>% 
  select(ABTSRBIID, number_of_children_hpog, number_of_children_pace) %>% 
  filter(data$number_of_children_hpog != data$number_of_children_pace, !is.na(data$number_of_children_hpog), !is.na(data$number_of_children_pace))
data$number_of_children_hpog[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$number_of_children_pace[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$number_of_children_0 <- coalesce(data$number_of_children_hpog, data$number_of_children_pace)
show_number_of_children_0 <- data %>% 
  select(RID.x, ABTSRBIID, DEPENDENT_CHILDREN, B15A_NUM_CHN, number_of_children_hpog, number_of_children_pace, number_of_children_0)
data <- data %>% 
  select(-number_of_children_hpog, -number_of_children_pace)
data <- data %>% 
  select(-DEPENDENT_CHILDREN, -B15A_NUM_CHN)

# birth_country_usa_0
data$birth_country_usa_hpog <- ifelse(!data$CITIZENSHIP %in% c(0, 1, 2, 3, 4, 5), NA, ifelse(data$CITIZENSHIP %in% c(1, 2), 1, 0))
data$birth_country_usa_pace <- ifelse(!data$B11_BORN_USA %in% c(0, 1), NA, ifelse(data$B11_BORN_USA == 1, 1, 0))
# print(sum(data$birth_country_usa_hpog != data$birth_country_usa_pace, na.rm = T)) # 77
filter1 <- data %>% 
  select(ABTSRBIID, birth_country_usa_hpog, birth_country_usa_pace) %>% 
  filter(data$birth_country_usa_hpog != data$birth_country_usa_pace, !is.na(data$birth_country_usa_hpog), !is.na(data$birth_country_usa_pace))
data$birth_country_usa_hpog[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$birth_country_usa_pace[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$birth_country_usa_0 <- coalesce(data$birth_country_usa_hpog, data$birth_country_usa_pace)
show_birth_country_usa_0 <- data %>% 
  select(RID.x, ABTSRBIID, CITIZENSHIP, B11_BORN_USA, birth_country_usa_hpog, birth_country_usa_pace, birth_country_usa_0)
data <- data %>% 
  select(-CITIZENSHIP, -B11_BORN_USA, -birth_country_usa_hpog, -birth_country_usa_pace)

# limited_english_0
data$limited_english_hpog <- ifelse(!data$LIMITED_ENGLISH %in% c(1, 2), NA, ifelse(data$LIMITED_ENGLISH == 1, 1, 0))
data$limited_english_pace <- ifelse(data$B12B_SPEAK_ENGLISH %in% c(3, 4) | data$B12C_READ_ENGLISH %in% c(3, 4), 1, 0)
# print(sum(data$limited_english_hpog != data$limited_english_pace, na.rm = T)) # 158
filter1 <- data %>% 
  select(ABTSRBIID, limited_english_hpog, limited_english_pace) %>% 
  filter(data$limited_english_hpog != data$limited_english_pace, !is.na(data$limited_english_hpog), !is.na(data$limited_english_pace))
data$limited_english_hpog[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$limited_english_pace[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$limited_english_0 <- coalesce(data$limited_english_hpog, data$limited_english_pace)
show_limited_english_0 <- data %>% 
  select(RID.x, ABTSRBIID, LIMITED_ENGLISH, B12B_SPEAK_ENGLISH, B12C_READ_ENGLISH, limited_english_hpog, limited_english_pace, limited_english_0)
data <- data %>% 
  select(-LIMITED_ENGLISH, -limited_english_hpog, -limited_english_pace)

# speak_only_english_at_home_0
data$speak_only_english_at_home_0 <- ifelse(!data$B12A_OTHER_LANG %in% c(0, 1), NA, ifelse(data$B12A_OTHER_LANG == 0, 1, 0))
data <- data %>% 
  select(-B12A_OTHER_LANG)

# spoken_english_proficiency_very_well_0 
# spoken_english_proficiency_well_0 
# spoken_english_proficiency_not_well_0 
# spoken_english_proficiency_not_at_all_0
data$spoken_english_proficiency_very_well_0 <- ifelse(!data$B12B_SPEAK_ENGLISH %in% c(1, 2, 3, 4), NA, ifelse(data$B12B_SPEAK_ENGLISH == 1, 1, 0))
data$spoken_english_proficiency_well_0 <- ifelse(!data$B12B_SPEAK_ENGLISH %in% c(1, 2, 3, 4), NA, ifelse(data$B12B_SPEAK_ENGLISH == 2, 1, 0))
data$spoken_english_proficiency_not_well_0 <- ifelse(!data$B12B_SPEAK_ENGLISH %in% c(1, 2, 3, 4), NA, ifelse(data$B12B_SPEAK_ENGLISH == 3, 1, 0))
data$spoken_english_proficiency_not_at_all_0 <- ifelse(!data$B12B_SPEAK_ENGLISH %in% c(1, 2, 3, 4), NA, ifelse(data$B12B_SPEAK_ENGLISH == 4, 1, 0))
data <- data %>% 
  select(-B12B_SPEAK_ENGLISH)

# reading_english_proficiency_very_well_0
# reading_english_proficiency_well_0
# reading_english_proficiency_not_well_0
# reading_english_proficiency_not_at_all_0
data$reading_english_proficiency_very_well_0 <- ifelse(!data$B12C_READ_ENGLISH %in% c(1, 2, 3, 4), NA, ifelse(data$B12C_READ_ENGLISH == 1, 1, 0))
data$reading_english_proficiency_well_0 <- ifelse(!data$B12C_READ_ENGLISH %in% c(1, 2, 3, 4), NA, ifelse(data$B12C_READ_ENGLISH == 2, 1, 0))
data$reading_english_proficiency_not_well_0 <- ifelse(!data$B12C_READ_ENGLISH %in% c(1, 2, 3, 4), NA, ifelse(data$B12C_READ_ENGLISH == 3, 1, 0))
data$reading_english_proficiency_not_at_all_0 <- ifelse(!data$B12C_READ_ENGLISH %in% c(1, 2, 3, 4), NA, ifelse(data$B12C_READ_ENGLISH == 4, 1, 0))
data <- data %>% 
  select(-B12C_READ_ENGLISH)

# currently_working_0
# print(table(data$CURRENT_EMPLOYMENT_ENROLL))
data$currently_working_hpog <- ifelse(!data$CURRENT_EMPLOYMENT_ENROLL %in% c(1, 2), NA, ifelse(data$CURRENT_EMPLOYMENT_ENROLL == 1, 1, 0))
data$currently_working_pace <- ifelse(!data$B24_CURR_WORK %in% c(1, 2, 3), NA, ifelse(data$B24_CURR_WORK == 1, 1, 0))
# print(sum(data$currently_working_hpog != data$currently_working_pace, na.rm = T)) # 41
filter1 <- data %>% 
  select(ABTSRBIID, currently_working_hpog, currently_working_pace) %>% 
  filter(data$currently_working_hpog != data$currently_working_pace, !is.na(currently_working_hpog), !is.na(currently_working_pace))
data$currently_working_hpog[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$currently_working_pace[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$currently_working_0 <- coalesce(data$currently_working_hpog, data$currently_working_pace)
show_currently_working_0 <- data %>% 
  select(RID.x, ABTSRBIID, CURRENT_EMPLOYMENT_ENROLL, B24_CURR_WORK, currently_working_hpog, currently_working_pace, currently_working_0)
data <- data %>% 
  select(-currently_working_hpog, -currently_working_pace, -CURRENT_EMPLOYMENT_ENROLL)

# worked_before_0
data$worked_before_hpog <- ifelse(!data$PAID_WORK %in% c(1, 2), NA, ifelse(data$PAID_WORK == 1, 1, 0))
data$worked_before_pace <- ifelse(!data$B24_CURR_WORK %in% c(1, 2, 3), NA, ifelse(data$B24_CURR_WORK == 2, 1, 0))
# print(sum(data$worked_before_hpog != data$worked_before_pace, na.rm = T)) # 514
filter1 <- data %>% 
  select(ABTSRBIID, worked_before_hpog, worked_before_pace) %>% 
  filter(data$worked_before_hpog != data$worked_before_pace, !is.na(worked_before_hpog), !is.na(worked_before_pace))
data$worked_before_hpog[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$worked_before_pace[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$worked_before_0 <- coalesce(data$worked_before_hpog, data$worked_before_pace)
show_worked_before_0 <- data %>% 
  select(RID.x, ABTSRBIID, PAID_WORK, B24_CURR_WORK, worked_before_hpog, worked_before_pace, worked_before_0)
data <- data %>% 
  select(-worked_before_hpog, -worked_before_pace)

# never_worked_0
data$never_worked_hpog <- ifelse(!data$PAID_WORK %in% c(1, 2), NA, ifelse(data$PAID_WORK == 2, 1, 0))
data$never_worked_pace <- ifelse(!data$B24_CURR_WORK %in% c(1, 2, 3), NA, ifelse(data$B24_CURR_WORK == 3, 1, 0))
# print(sum(data$never_worked_hpog != data$never_worked_pace, na.rm = T)) # 28
filter1 <- data %>% 
  select(ABTSRBIID, never_worked_hpog, never_worked_pace) %>% 
  filter(data$never_worked_hpog != data$never_worked_pace, !is.na(never_worked_hpog), !is.na(never_worked_pace))
data$never_worked_hpog[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$never_worked_pace[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$never_worked_0 <- coalesce(data$never_worked_hpog, data$never_worked_pace)
show_never_worked_0 <- data %>% 
  select(RID.x, ABTSRBIID, PAID_WORK, B24_CURR_WORK, never_worked_hpog, never_worked_pace, never_worked_0)
data <- data %>% 
  select(-never_worked_hpog, -never_worked_pace, -PAID_WORK, -B24_CURR_WORK)

# tanf_assistance_0 -- 0 difference
data$tanf_assistance_hpog <- ifelse(!data$IB_TANF %in% c(1, 2), NA, ifelse(data$IB_TANF == 1, 1, 0))
data$tanf_assistance_pace <- ifelse(!data$B26C_PA %in% c(0, 1), NA, ifelse(data$B26C_PA == 1, 1, 0))
# print(sum(data$tanf_assistance_hpog != data$tanf_assistance_pace, na.rm = T)) # 0
data$tanf_assistance_0 <- coalesce(data$tanf_assistance_hpog, data$tanf_assistance_pace)
show_tanf_assistance <- data %>% 
  select(RID.x, ABTSRBIID, IB_TANF, B26C_PA, tanf_assistance_hpog, tanf_assistance_pace, tanf_assistance_0)
data <- data %>% 
  select(-tanf_assistance_hpog, -tanf_assistance_pace, -IB_TANF, -B26C_PA)

# wic_or_snap_assistance_0 -- 0 difference
data$wic_or_snap_assist_hpog <- ifelse(!data$IB_WIC %in% c(1, 2) & !data$IB_SNAP %in% c(1, 2), NA, ifelse(data$IB_WIC == 1 | data$IB_SNAP == 1, 1, 0))
data$wic_or_snap_assist_pace <- ifelse(!data$B26B_WICSNAP %in% c(0, 1), NA, ifelse(data$B26B_WICSNAP == 1, 1, 0))
# print(sum(data$wic_or_snap_assist_hpog != data$wic_or_snap_assist_pace, na.rm = T)) # 0
data$wic_or_snap_assistance_0 <- coalesce(data$wic_or_snap_assist_hpog, data$wic_or_snap_assist_pace)
show_wic_or_snap_assistance_0 <-  data %>% 
  select(RID.x, ABTSRBIID, IB_WIC, IB_SNAP, B26B_WICSNAP, wic_or_snap_assist_hpog, wic_or_snap_assist_pace, wic_or_snap_assistance_0)
data <- data %>% 
  select(-IB_WIC, -IB_SNAP, -B26B_WICSNAP, -wic_or_snap_assist_hpog, -wic_or_snap_assist_pace)

data$future_school_part_time_hpog <- ifelse(!data$EXPECTED_SCHOOL_ENROLL %in% c(1, 2), NA, ifelse(data$EXPECTED_SCHOOL_ENROLL == 2, 1, 0))
data$future_school_part_time_pace <- ifelse(!data$S01_FUTURE_SCHOOL %in% c(1, 2), NA, ifelse(data$S01_FUTURE_SCHOOL == 1, 1, 0))
# print(sum(data$future_school_part_time_hpog != data$future_school_part_time_pace, na.rm = T)) # 0
data$future_school_part_time <- coalesce(data$future_school_part_time_hpog, data$future_school_part_time_pace)
show_future_school_part_time <- data %>% 
  select(RID.x, ABTSRBIID, EXPECTED_SCHOOL_ENROLL, S01_FUTURE_SCHOOL, future_school_part_time_hpog, future_school_part_time_pace, future_school_part_time)
data <- data %>% 
  select(-future_school_part_time_hpog, -future_school_part_time_pace)

# future_school_full_time
data$future_school_full_time_hpog <- ifelse(!data$EXPECTED_SCHOOL_ENROLL %in% c(1, 2), NA, ifelse(data$EXPECTED_SCHOOL_ENROLL == 1, 1, 0))
data$future_school_full_time_pace <- ifelse(!data$S01_FUTURE_SCHOOL %in% c(1, 2), NA, ifelse(data$S01_FUTURE_SCHOOL == 2, 1, 0))
# print(sum(data$future_school_full_time_hpog != data$future_school_full_time_pace, na.rm = T)) # 0
data$future_school_full_time <- coalesce(data$future_school_full_time_hpog, data$future_school_full_time_pace)
show_future_school_full_time <- data %>% 
  select(RID.x, ABTSRBIID, EXPECTED_SCHOOL_ENROLL, S01_FUTURE_SCHOOL, future_school_full_time_hpog, future_school_full_time_pace, future_school_full_time)
data <- data %>% 
  select(-future_school_full_time_hpog, -future_school_full_time_pace, -EXPECTED_SCHOOL_ENROLL, -S01_FUTURE_SCHOOL)

# future_work -- 0 difference
data$future_work_hpog <- ifelse(!data$EXPECTED_WORK %in% c(1, 2), NA, ifelse(data$EXPECTED_WORK == 1, 1, 0))
data$future_work_pace <- ifelse(!data$S02_FUTURE_WORK %in% c(1, 2), NA, ifelse(data$S02_FUTURE_WORK == 2, 1, 0))
# print(sum(data$future_work_hpog != data$future_work_pace, na.rm = T)) # 0
data$future_work <- coalesce(data$future_work_hpog, data$future_work_pace)
show_future_work <- data %>% 
  select(RID.x, ABTSRBIID, EXPECTED_WORK, S02_FUTURE_WORK, future_work_hpog, future_work_pace, future_work)
data <- data %>% 
  select(-future_work_hpog, -future_work_pace, -EXPECTED_WORK, -S02_FUTURE_WORK)

# work_hours
data$expected_hours_hpog <- ifelse(data$EXPECTED_HOURS < 0 | data$EXPECTED_HOURS > 75, NA, data$EXPECTED_HOURS)
data$expected_hours_pace <- ifelse(data$S02A_WORK_HOURS < 0 | data$S02A_WORK_HOURS > 40, NA, data$S02A_WORK_HOURS)
# print(sum(data$expected_hours_hpog != data$expected_hours_pace, na.rm = T)) # 0
data$work_hours <- coalesce(data$expected_hours_hpog, data$expected_hours_pace)
show_work_hours <- data %>% 
  select(RID.x, ABTSRBIID, EXPECTED_HOURS, S02A_WORK_HOURS, expected_hours_hpog, expected_hours_pace, work_hours)
data <- data %>% 
  select(-expected_hours_hpog, -expected_hours_pace, -EXPECTED_HOURS, -S02A_WORK_HOURS)

# financial_support_not_difficult_at_all
# financial_support_somewhat_difficult
# financial_support_very_difficult
# print(table(data$S05_FINANCIAL_SUPPORT))
data$financial_support_not_difficult_at_all <- ifelse(!data$S05_FINANCIAL_SUPPORT %in% c(1, 2, 3), NA, ifelse(data$S05_FINANCIAL_SUPPORT == 1, 1, 0))
data$financial_support_somewhat_difficult <- ifelse(!data$S05_FINANCIAL_SUPPORT %in% c(1, 2, 3), NA, ifelse(data$S05_FINANCIAL_SUPPORT == 2, 1, 0))
data$financial_support_very_difficult <- ifelse(!data$S05_FINANCIAL_SUPPORT %in% c(1, 2, 3), NA, ifelse(data$S05_FINANCIAL_SUPPORT == 3, 1, 0))
data <- data %>% 
  select(-S05_FINANCIAL_SUPPORT)

# career_knowledge_7_variables
data$S13A_ABILITIES[data$S13A_ABILITIES == -95] <- NA
data$S13B_PLAN[data$S13B_PLAN == -95] <- NA
data$S13C_HELP[data$S13C_HELP == -95] <- NA
data$S13D_JOB[data$S13D_JOB == -95] <- NA
data$S13E_ORGANIZATION[data$S13E_ORGANIZATION == -95] <- NA
data$S13F_OCCUPATION[data$S13F_OCCUPATION == -95] <- NA
data$S13G_EDUCATION[data$S13G_EDUCATION == -95] <- NA
data$career_knowledge_7_variables <- (data$S13A_ABILITIES + data$S13B_PLAN + data$S13C_HELP + data$S13D_JOB
                                      + data$S13E_ORGANIZATION + data$S13F_OCCUPATION + data$S13G_EDUCATION) / 7
show_career_knowledge_7_variables <- data %>% 
  select(RID.x, ABTSRBIID, S13A_ABILITIES, S13B_PLAN, S13C_HELP, S13D_JOB, S13E_ORGANIZATION, S13F_OCCUPATION, S13G_EDUCATION, career_knowledge_7_variables)

# career_knowledge_3_variables
data$GOALS_ORGANIZATION_TYPE[data$GOALS_ORGANIZATION_TYPE == -95] <- NA
data$GOALS_5YEAR_PLAN[data$GOALS_5YEAR_PLAN == -95] <- NA
data$GOALS_FUTURE_OCCUPATION[data$GOALS_FUTURE_OCCUPATION == -95] <- NA
data$career_knowledge_3_variables_hpog <- (data$GOALS_5YEAR_PLAN + data$GOALS_ORGANIZATION_TYPE + data$GOALS_FUTURE_OCCUPATION) / 3
data$career_knowledge_3_variables_pace <- (data$S13B_PLAN + data$S13F_OCCUPATION + data$S13E_ORGANIZATION) / 3
data$career_knowledge_3_variables <- coalesce(data$career_knowledge_3_variables_hpog, data$career_knowledge_3_variables_pace)
data <- data %>% 
  select(-career_knowledge_3_variables_pace, -career_knowledge_3_variables_hpog)

# training_commitment_index
data$S11CC_B_OTHER_THAN_SCHOOL[data$S11CC_B_OTHER_THAN_SCHOOL == -95] <- NA
data$S11CC_F_STOP_ATTENDING[data$S11CC_F_STOP_ATTENDING == -95] <- NA
data$S11CC_I_HOW_LONG[data$S11CC_I_HOW_LONG == -95] <- NA
data$S11CC_P_DO_WELL[data$S11CC_P_DO_WELL == -95] <- NA
data$S11CC_T_MOTIVIATED[data$S11CC_T_MOTIVIATED == -95] <- NA
data$S11CC_DD_SUCCEED[data$S11CC_DD_SUCCEED == -95] <- NA
data$S11CC_EE_BEST_CHOICE[data$S11CC_EE_BEST_CHOICE == -95] <- NA
data$S11CC_GG_COMMITTED[data$S11CC_GG_COMMITTED == -95] <- NA
data$S11CC_JJ_ACHIEVE_GOALS[data$S11CC_JJ_ACHIEVE_GOALS == -95] <- NA
data$S11CC_LL_RIGHT_FOR_ME[data$S11CC_LL_RIGHT_FOR_ME == -95] <- NA
data$training_commitment_index <- (data$S11CC_B_OTHER_THAN_SCHOOL + data$S11CC_F_STOP_ATTENDING + data$S11CC_I_HOW_LONG
                                   + data$S11CC_P_DO_WELL + data$S11CC_T_MOTIVIATED + data$S11CC_DD_SUCCEED
                                   + data$S11CC_EE_BEST_CHOICE + data$S11CC_GG_COMMITTED + data$S11CC_JJ_ACHIEVE_GOALS
                                   + data$S11CC_LL_RIGHT_FOR_ME) / 10

# academic_discipline_index
data$S11AD_D_DISCIPLINED[data$S11AD_D_DISCIPLINED == -95] <- NA
data$S11AD_J_SKIP_CLASSES[data$S11AD_J_SKIP_CLASSES == -95] <- NA
data$S11AD_M_ABILITIES[data$S11AD_M_ABILITIES == -95] <- NA
data$S11AD_N_NOTES[data$S11AD_N_NOTES == -95] <- NA
data$S11AD_R_DEADLINES[data$S11AD_R_DEADLINES == -95] <- NA
data$S11AD_U_PERFORMANCE[data$S11AD_U_PERFORMANCE == -95] <- NA
data$S11AD_II_DO_BEST[data$S11AD_II_DO_BEST == -95] <- NA
data$S11AD_KK_CONSISTENTLY[data$S11AD_KK_CONSISTENTLY == -95] <- NA
data$S11AD_NN_HARD_WORKING[data$S11AD_NN_HARD_WORKING == -95] <- NA
data$S11AD_PP_ASSIGNMENTS[data$S11AD_PP_ASSIGNMENTS == -95] <- NA
data$academic_discipline_index <-(data$S11AD_D_DISCIPLINED + data$S11AD_D_DISCIPLINED + data$S11AD_M_ABILITIES + data$S11AD_N_NOTES
                                  + data$S11AD_R_DEADLINES + data$S11AD_U_PERFORMANCE + data$S11AD_II_DO_BEST + data$S11AD_KK_CONSISTENTLY
                                  + data$S11AD_NN_HARD_WORKING + data$S11AD_PP_ASSIGNMENTS) / 10


# emotional_stability_index
data$S11ES_A_CALM[data$S11ES_A_CALM == -95] <- NA
data$S11ES_E_LOSE_CONTROL[data$S11ES_E_LOSE_CONTROL == -95] <- NA
data$S11ES_G_IRRITATED[data$S11ES_G_IRRITATED == -95] <- NA
data$S11ES_L_UPSET[data$S11ES_L_UPSET == -95] <- NA
data$S11ES_Q_EXPRESS_ANGER[data$S11ES_Q_EXPRESS_ANGER == -95] <- NA
data$S11ES_S_THINK_CLEARLY[data$S11ES_S_THINK_CLEARLY == -95] <- NA
data$S11ES_V_TEMPER[data$S11ES_V_TEMPER == -95] <- NA
data$S11ES_W_ANNOYED[data$S11ES_W_ANNOYED == -95] <- NA
data$S11ES_Y_PATIENT[data$S11ES_Y_PATIENT == -95] <- NA
data$S11ES_AA_ARGUMENTS[data$S11ES_AA_ARGUMENTS == -95] <- NA
data$S11ES_FF_FRUSTRATION[data$S11ES_FF_FRUSTRATION == -95] <- NA
data$S11ES_HH_OUT_OF_CONTROL[data$S11ES_HH_OUT_OF_CONTROL == -95] <- NA
data$emotional_stability_index <- (data$S11ES_A_CALM + data$S11ES_E_LOSE_CONTROL + data$S11ES_G_IRRITATED + data$S11ES_L_UPSET
                                   + data$S11ES_Q_EXPRESS_ANGER + data$S11ES_S_THINK_CLEARLY + data$S11ES_V_TEMPER + data$S11ES_W_ANNOYED
                                   + data$S11ES_Y_PATIENT + data$S11ES_AA_ARGUMENTS + data$S11ES_FF_FRUSTRATION + data$S11ES_HH_OUT_OF_CONTROL) / 12


# social_support_index
data$S12A_DEPEND[data$S12A_DEPEND == -95] <- NA
data$S12B_CLOSE[data$S12B_CLOSE == -95] <- NA
data$S12C_TURN_TO[data$S12C_TURN_TO == -95] <- NA
data$S12D_ACTIVITIES[data$S12D_ACTIVITIES == -95] <- NA
data$S12E_RESPECT[data$S12E_RESPECT == -95] <- NA
data$S12F_ASSISTANCE[data$S12F_ASSISTANCE == -95] <- NA
data$S12G_EMOTIONAL_SECURITY[data$S12G_EMOTIONAL_SECURITY == -95] <- NA
data$S12H_COMPENTENCE[data$S12H_COMPENTENCE == -95] <- NA
data$S12I_INTERESTS[data$S12I_INTERESTS == -95] <- NA
data$S12J_TRUSTWORTHY[data$S12J_TRUSTWORTHY == -95] <- NA
data$social_support_index <- (data$S12A_DEPEND + data$S12B_CLOSE + data$S12C_TURN_TO + data$S12D_ACTIVITIES + data$S12E_RESPECT
                              + data$S12F_ASSISTANCE + data$S12G_EMOTIONAL_SECURITY + data$S12H_COMPENTENCE + data$S12I_INTERESTS
                              + data$S12J_TRUSTWORTHY) / 10

# life_challenges_index_6_variables
data$S15A_CHILD_CARE[data$S15A_CHILD_CARE == -95] <- NA
data$S15B_TRANSPORTATION[data$S15B_TRANSPORTATION == -95] <- NA
data$S15C_ALCOHOL[data$S15C_ALCOHOL == -95] <- NA
data$S15D_ILLNESS[data$S15D_ILLNESS == -95] <- NA
data$S15E_ARGUMENTS[data$S15E_ARGUMENTS == -95] <- NA
data$S15F_VIOLENCE[data$S15F_VIOLENCE == -95] <- NA
data$life_challenges_index_6_variables <- (data$S15A_CHILD_CARE + data$S15B_TRANSPORTATION + data$S15C_ALCOHOL + data$S15D_ILLNESS
                                           + data$S15E_ARGUMENTS + data$S15F_VIOLENCE) / 6

# life_challenges_index_4_variables
data$OFTEN_TRANSPORTATION[data$OFTEN_TRANSPORTATION == -95] <- NA
data$OFTEN_ALCOHOL_DRUG[data$OFTEN_ALCOHOL_DRUG == -95] <- NA
data$OFTEN_CHILD_CARE[data$OFTEN_CHILD_CARE == -95] <- NA
data$OFTEN_ILLNESS[data$OFTEN_ILLNESS == -95] <- NA
data$life_challenges_index_4_variables_hpog <- (data$OFTEN_TRANSPORTATION + data$OFTEN_ALCOHOL_DRUG + data$OFTEN_CHILD_CARE + data$OFTEN_ILLNESS) / 4
data$life_challenges_index_4_variables_pace <- (data$S15A_CHILD_CARE + data$S15B_TRANSPORTATION + data$S15C_ALCOHOL + data$S15D_ILLNESS) / 4
data$life_challenges_index_4_variables <- coalesce(data$life_challenges_index_4_variables_hpog, data$life_challenges_index_4_variables_pace)
data <- data %>% 
  select(-life_challenges_index_4_variables_hpog, -life_challenges_index_4_variables_pace)

# stress_index
data$S14A_CONTROL[data$S14A_CONTROL == -95] <- NA
data$S14B_CONFIDENCE[data$S14B_CONFIDENCE == -95] <- NA
data$S14C_GOING_YOUR_WAY[data$S14C_GOING_YOUR_WAY == -95] <- NA
data$S14D_DIFFICULTIES[data$S14D_DIFFICULTIES == -95] <- NA
data$stress_index <- (data$S14A_CONTROL + data$S14B_CONFIDENCE + data$S14C_GOING_YOUR_WAY + data$S14D_DIFFICULTIES) / 4

data <- data %>% rename(number_of_services = COV_SVCS) # number_of_services
data <- data %>% rename(behavioral_incentive = COV_BEHAVIORAL_INCENTIVES) # behavioral_incentive
data <- data %>% rename(avg_fte_caseload = COV_AVG_FTE_CASELOAD) # avg_fte_caseload 
data <- data %>% rename(childcare_transport = COV_CHILDCARE_TRANSPORT) # childcare_transport
data <- data %>% rename(social_services = COV_SVCS_SOCIAL) # social_services
data <- data %>% rename(financial_services = COV_TUITION_FIN_SVCS) # financial_services
data <- data %>% rename(number_of_employment_supports = COV_EMPLOYMENT_SUPPORTS) # number_of_employment_supports
data <- data %>% rename(number_of_colocated_services = COV_LOCATION_OF_SERVICES) # number_of_colocated_servicesc
data <- data %>% rename(emergency_assistance = COV_EMERGENCY_ASSISTANCE) # emergency_assistance
data <- data %>% rename(number_of_cp_principles = COV_CP_PRINCIPLES) # number_of_cp_principles
# peer_support
data$COV_PEER_SUPPORT[data$COV_PEER_SUPPORT == -95] <- NA
data <- data %>% rename(peer_support = COV_PEER_SUPPORT)

data <- data %>% rename(proportion_local_some_college = LC_EDATTAIN) # proportion_local_some_college
data <- data %>% rename(proportion_local_jobs_health_care = LC_EMPPCT) # proportion_local_jobs_health_care
data <- data %>% rename(median_wage_local_health_care = LC_MEDWAGE) # median_wage_local_health_care
data <- data %>% rename(proportion_local_cash_assistance = LC_PUBASS) # proportion_local_cash_assistance
data <- data %>% rename(proportion_local_enrolled_school = LC_SCHEN) # proportion_local_enrolled_school
data <- data %>% rename(total_msa_population = LC_TOTPOP) # total_msa_population
data <- data %>% rename(percent_local_unemployed = LC_UERATE) # percent_local_unemployed

# program_id
# SITE_NUM.y is a subset of SITE_NUM.x
# print(table(data$SITE_NUM.x))
# print(table(data$SITE_NUM.y))
data$program_id <- coalesce(data$SITE_NUM.x, data$SITE_NUM.y)
data$program_id[data$STUDY_HPOG == 1 & data$STUDY_PACE == 0] <- 10
show_site_num <- data %>% 
  select(RID.x, ABTSRBIID, STUDY_HPOG, STUDY_PACE, SITE_NUM.x, SITE_NUM.y, program_id)
data <- data %>% 
  select(-SITE_NUM.x, -SITE_NUM.y)

# site_id
data <- data %>% 
  rename(site_id = USERSITE.x) %>% 
  select(-USERSITE)
# if STUDY_HPOG = 1 & STUDY_PACE = 1, then R_YEAR_UP_SUB = 0
# if STUDY_HPOG = 1 & STUDY_PACE = 0, then R_YEAR_UP_SUB = NA
# if STUDY_HPOG = 0 & STUDY_PACE = 1, then R_YEAR_UP_SUB is any of (1, 2, 3, 4, 5, 6, 7, 8)
# print(table(data$R_YEAR_UP_SUB))
data$R_YEAR_UP_SUB[data$R_YEAR_UP_SUB == 0] <- NA # Set to NA for proper coalescing
data$site_id <- coalesce(data$site_id, data$R_YEAR_UP_SUB)
show_site_id <- data %>% 
  select(STUDY_HPOG, STUDY_PACE, RID.x, ABTSRBIID, site_id, R_YEAR_UP_SUB)
data <- data %>% 
  select(-R_YEAR_UP_SUB)

# weight_15
data <- data %>% rename(weight_15 = WEIGHT15) 

# weight_36
data <- data %>% rename(weight_36 = ACE_SIMILARWT_3DIM) 

# weight_72
data$weight_72_hpog <- data$WEIGHT72NDNHPS
data$weight_72_pace <- data$WEIGHT
# print(sum(data$WEIGHT72NDNHPS != data$WEIGHT, na.rm = T)) # 29
filter1 <- data %>% 
  select(ABTSRBIID, WEIGHT72NDNHPS, WEIGHT) %>% 
  filter(data$WEIGHT72NDNHPS != data$WEIGHT, !is.na(data$WEIGHT72NDNHPS), !is.na(data$WEIGHT))
data$weight_72_hpog[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$weight_72_pace[data$ABTSRBIID %in% filter1$ABTSRBIID] <- NA
data$weight_72 <- coalesce(data$weight_72_hpog, data$weight_72_pace)
show_weight_72 <- data %>% 
  select(RID.x, ABTSRBIID, weight_72_hpog, weight_72_pace, weight_72)
# print(sum(show_weight_72$weight_72_hpog != show_weight_72$weight_72_pace, na.rm = T))
data <- data %>% 
  select(-weight_72_hpog, -weight_72_pace, -WEIGHT, -WEIGHT72NDNHPS)

# participant_id
# ABTSRBIID is NA for only 1 value from HPOG only study
show_participant_id <- data %>% 
  select(RID.x, ABTSRBIID, STUDY_HPOG, STUDY_PACE) %>% 
  filter(is.na(ABTSRBIID))
# Two approaches 
# - set participant ID = ABTSRBIID -- currently using
# - set participant ID = RID.x + ABTSRBIID 
data$participant_id <- data$ABTSRBIID

hpog_only <- data %>% 
  filter(STUDY_HPOG == 1, STUDY_PACE == 0)

pace_only <- data %>% 
  filter(STUDY_PACE == 1, STUDY_HPOG ==0)

hpog_and_pace <- data %>% 
  filter(STUDY_PACE == 1, STUDY_HPOG == 1)
