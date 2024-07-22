library(readr)
library(dplyr)
library(openxlsx)
library(rprojroot)

# Find the root of the current project
root <- find_root(is_rstudio_project)
PACE_DATA_PATH <- 'read-PACE-output/'

source("utils.R")

# ----------------- Read PACE Data Frame ----------------- 
data <- read.csv(paste(PACE_DATA_PATH, 'pace-joined-df.csv', sep = ""))

# ----------------- Data Manipulation ----------------- 

# participant_id
data <- data %>% rename(participant_id = ABTSRBIID)

# site_id
data$site_id <- ifelse(data$SITE_NUM.x != 9, data$SITE_NUM.x,
                       data$R_YEAR_UP_SUB + 8)

# surveyrespondent_15_pace
data <- data %>% rename(surveyrespondent_15_pace = SURVEYRESPONDENT15)

# surveyrespondent_36_pace
data <- data %>% rename(surveyrespondent_36_pace = C_SKIPOUT)

# surveyrespondent_72_pace -- already set to 1 in read-PACE.R file

# study_pace
data$study_pace <- 1

# study_year_up
data$study_year_up <- ifelse(data$SITE_NUM.x == 9, 1, 0)

# currently_working_15
data$currently_working_15 <- ifelse(data$CURRHOURLYWAGE < 0 | data$CURRHOURLYWAGE > 40, NA, 
                                    ifelse(data$CURRHOURLYWAGE == 0, 0, 1))

# currently_working_36
data$currently_working_36 <- ifelse(!data$EMPLOYEDATFOLLOWUP %in% c(0, 1), NA, data$EMPLOYEDATFOLLOWUP)

# currently_working_72
data$currently_working_72 <- ifelse(!data$EMPLOYEDATFOLLOWUP_LO %in% c(0, 1), NA, data$EMPLOYEDATFOLLOWUP_LO)

# hours_worked_per_week_15
data$hours_worked_per_week_15 <-ifelse(is.na(data$currently_working_15) | (data$currently_working_15 == 1 & data$CURRWORKHOURS < 0), NA,
                                       ifelse(data$currently_working_15 == 0, 0, data$CURRWORKHOURS))

# hourly_wage_rate_15
data$hourly_wage_rate_15 <- ifelse(data$CURRHOURLYWAGE <= 0 | data$CURRHOURLYWAGE > 40, NA, data$CURRHOURLYWAGE)

# weekly_earnings_15
data$weekly_earnings_15 <- data$hours_worked_per_week_15 * data$hourly_wage_rate_15

# hours_worked_per_week_36
data$hours_worked_per_week_36 <- ifelse(!data$EMPLOYEDATFOLLOWUP %in% c(0, 1), NA, 
                                        ifelse(data$EMPLOYEDATFOLLOWUP == 0, 0, 
                                               data$employment_weekhours_pace))

# hourly_wage_rate_36
data$hourly_wage_rate_36 <- data$employment_hourlywage_pace

# weekly_earnings_36
data$weekly_earnings_36 <- ifelse(!data$EMPLOYEDATFOLLOWUP %in% c(0, 1), NA,
                                  ifelse(data$EMPLOYEDATFOLLOWUP == 0, 0, 
                                         data$hours_worked_per_week_36 * data$houly_wage_rate_36))

# hours_worked_per_week_alt_36
data$hours_worked_per_week_alt_36 <- ifelse(!data$EMPLOYEDATFOLLOWUP %in% c(0, 1), NA,
                                            ifelse(data$EMPLOYEDATFOLLOWUP == 0, 0, 
                                                   data$employment_weekhours_pace_alt))
  
# hourly_wage_rate_alt_36
data$hourly_wage_rate_alt_36 <- data$employment_hourlywage_pace_alt

# weekly_earnings_alt_36
data$weekly_earnings_alt_36 <- ifelse(!data$EMPLOYEDATFOLLOWUP %in% c(0, 1), NA,
                                      ifelse(data$EMPLOYEDATFOLLOWUP == 0, 0, 
                                             data$hours_worked_per_week_alt_36 * data$hourly_wage_rate_alt_36))

# hours_worked_per_week_72
data$hours_worked_per_week_72 <- ifelse(!data$EMPLOYEDATFOLLOWUP_LO %in% c(0, 1), NA,
                                        ifelse(data$EMPLOYEDATFOLLOWUP_LO == 0, 0, 
                                               ifelse(data$CURRENTHOURS_LO < 0 | data$CURRENTHOURS_LO > 80, NA, 
                                                      data$CURRENTHOURS_LO)))

# hourly_wage_rate_72
data$hourly_wage_rate_72 <- ifelse(data$CURRENTWAGERATE_LO < 1.02 | data$CURRENTWAGERATE_LO > 96.16, NA, data$CURRENTWAGERATE_LO)

# weekly_earnings_72
data$weekly_earnings_72 <- ifelse(data$WEEKLYEARNINGS_LO < 0 | data$WEEKLYEARNINGS_LO > 4000, NA, data$WEEKLYEARNINGS_LO)

# treatment_group_0
data <- data %>% rename(treatment_group_0 = R_TREATMENT_STATUS)

# age_LT21_0
# age_21To24_0
# age_25To34_0
# age_GE35_0
data$age_LT21_0 <- ifelse(data$AGE == 1, 1, 0)
data$age_21To24_0 <- ifelse(data$AGE == 2, 1, 0)
data$age_25To34_0 <- ifelse(data$AGE == 3, 1, 0)
data$age_GE35_0 <- ifelse(data$AGE == 4, 1, 0)

# ethnicity_hispanic_0
data$ethnicity_hispanic_0 <- ifelse(!data$B09_HISPANIC %in% c(1, 2, 3, 4, 5), NA, ifelse(data$B09_HISPANIC %in% c(2, 3, 4, 5), 1, 0))

# race_white_0
# race_black_0
# race_asian_0
# race_american_indian_0
# race_pacific_islander_0
data$race_white_0 <- ifelse(data$B10_RACE_WHITE == 1, 1, 0)
data$race_black_0 <- ifelse(data$B10_RACE_BLACK == 1, 1, 0)
data$race_asian_0 <- ifelse(data$B10_RACE_ASIAN == 1, 1, 0)
data$race_american_indian_0 <- ifelse(data$B10_RACE_AM_INDIAN == 1, 1, 0)
data$race_pacific_islander_0 <- ifelse(data$B10_RACE_PACIFIC == 1, 1, 0)

# No_HS_in_US_0
data$B23_GRADES[!data$B23_GRADES %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$No_HS_in_US_0 <- ifelse(data$B23_GRADES == 1, 1, 0) 

# grades_mostly_A_0
# grades_mostly_B_0
# grades_mostly_C_0
# grades_mostly_D_0
# grades_mostly_F_0
data$grades_mostly_A_0 <- ifelse(data$B23_GRADES == 2, 1, 0) 
data$grades_mostly_B_0 <- ifelse(data$B23_GRADES == 3, 1, 0) 
data$grades_mostly_C_0 <- ifelse(data$B23_GRADES == 4, 1, 0) 
data$grades_mostly_D_0 <- ifelse(data$B23_GRADES == 5, 1, 0) 
data$grades_mostly_F_0 <- ifelse(data$B23_GRADES == 6, 1, 0)

# educ_no_hs_credential_0
data$educ_no_hs_credential_0 <- ifelse(!data$B17_EDUC %in% c(1, 2, 3, 4, 5, 6, 7, 8), NA, 
                                       ifelse(data$B17_EDUC %in% c(1, 2), 1, 0))

# educ_ged_or_alternative_credential_0
data$educ_ged_or_alternative_credential_0 <- ifelse(!data$B17_EDUC %in% c(1, 2, 3, 4, 5, 6, 7, 8), NA, 
                                                    ifelse(data$B17_EDUC == 3, 1, 0))

# educ_regular_high_school_diploma_0
data$educ_regular_high_school_diploma_0 <- ifelse(!data$B17_EDUC %in% c(1, 2, 3, 4, 5, 6, 7, 8), NA, 
                                                  ifelse(data$B17_EDUC == 4, 1, 0))

# educ_some_college_0
data$educ_some_college_0 <- ifelse(!data$B17_EDUC %in% c(1, 2, 3, 4, 5, 6, 7, 8), NA, 
                                   ifelse(data$B17_EDUC %in% c(5, 6, 7), 1, 0))

# educ_bachelors_degree_or_higher_0
data$educ_bachelors_degree_or_higher_0 <- ifelse(!data$B17_EDUC %in% c(1, 2, 3, 4, 5, 6, 7, 8), NA, 
                                                 ifelse(data$B17_EDUC == 8, 1, 0))

# postsec_voc_tech_certificate_0
data$postsec_voc_tech_certificate_0 <- ifelse(!data$B18_VOCTECH_CERT %in% c(0, 1), NA, 
                                              ifelse(data$B18_VOCTECH_CERT == 1, 1, 0))

# sex_male_0
# marstat_married_0
# marstat_widowed_0
# marstat_divorced_or_separated_0
# marstat_never_married_0
data$sex_male_0 <- ifelse(!data$B07_SEX %in% c(0, 1), NA, ifelse(data$B07_SEX == 0, 1, 0))
data$marstat_married_0 <- ifelse(!data$B08_MARSTAT %in% c(1, 2, 3, 4, 5), NA, ifelse(data$B08_MARSTAT == 1, 1, 0))
data$marstat_widowed_0 <- ifelse(!data$B08_MARSTAT %in% c(1, 2, 3, 4, 5), NA, ifelse(data$B08_MARSTAT == 2, 1, 0))
data$marstat_divorced_or_separated_0 <- ifelse(!data$B08_MARSTAT %in% c(1, 2, 3, 4, 5), NA, ifelse(data$B08_MARSTAT %in% c(3, 4), 1, 0))
data$marstat_never_married_0 <- ifelse(!data$B08_MARSTAT %in% c(1, 2, 3, 4, 5), NA, ifelse(data$B08_MARSTAT == 5, 1, 0))

# number_of_children_home_0
data$number_of_children_home_0 <- ifelse(!data$B15A_NUM_CHN %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 16), NA, data$B15A_NUM_CHN)

# any_children
data$any_children_0 <- ifelse(is.na(data$number_of_children_home_0) | data$number_of_children_home_0 == 0, 0, 1)

# birth_country_usa_0
data$birth_country_usa_0 <- ifelse(!data$B11_BORN_USA %in% c(0, 1), NA, 
                                   ifelse(data$B11_BORN_USA == 1, 1, 0))

# speak_only_english_at_home_0
data$speak_only_english_at_home_0 <- ifelse(!data$B12A_OTHER_LANG %in% c(0, 1), NA, 
                                            ifelse(data$B12A_OTHER_LANG == 0, 1, 0))

# spoken_english_proficiency_very_well_0 
data$spoken_english_proficiency_very_well_0 <- ifelse(data$speak_only_english_at_home_0 == 1, 1, 
                                                      ifelse(!data$B12B_SPEAK_ENGLISH %in% c(1, 2, 3, 4), NA, 
                                                             ifelse(data$B12B_SPEAK_ENGLISH == 1, 1, 0)))
# spoken_english_proficiency_well_0 
data$spoken_english_proficiency_well_0 <- ifelse(data$speak_only_english_at_home_0 == 1, 0, 
                                                 ifelse(!data$B12B_SPEAK_ENGLISH %in% c(1, 2, 3, 4), NA, 
                                                        ifelse(data$B12B_SPEAK_ENGLISH == 2, 1, 0)))

# spoken_english_proficiency_not_well_0 
data$spoken_english_proficiency_not_well_0 <- ifelse(data$speak_only_english_at_home_0 == 1, 0,
                                                     ifelse(!data$B12B_SPEAK_ENGLISH %in% c(1, 2, 3, 4), NA, 
                                                            ifelse(data$B12B_SPEAK_ENGLISH == 3, 1, 0)))

# spoken_english_proficiency_not_at_all_0
data$spoken_english_proficiency_not_at_all_0 <- ifelse(data$speak_only_english_at_home_0 == 1, 0,
                                                       ifelse(!data$B12B_SPEAK_ENGLISH %in% c(1, 2, 3, 4), NA, 
                                                              ifelse(data$B12B_SPEAK_ENGLISH == 4, 1, 0)))

# reading_english_proficiency_very_well_0
data$reading_english_proficiency_very_well_0 <- ifelse(data$speak_only_english_at_home_0 == 1, 1,
                                                       ifelse(!data$B12C_READ_ENGLISH %in% c(1, 2, 3, 4), NA, 
                                                              ifelse(data$B12C_READ_ENGLISH == 1, 1, 0)))

# reading_english_proficiency_well_0
data$reading_english_proficiency_well_0 <- ifelse(data$speak_only_english_at_home_0 == 1, 0,
                                                  ifelse(!data$B12C_READ_ENGLISH %in% c(1, 2, 3, 4), NA, 
                                                         ifelse(data$B12C_READ_ENGLISH == 2, 1, 0)))

# reading_english_proficiency_not_well_0
data$reading_english_proficiency_not_well_0 <- ifelse(data$speak_only_english_at_home_0 == 1, 0,
                                                      ifelse(!data$B12C_READ_ENGLISH %in% c(1, 2, 3, 4), NA, 
                                                             ifelse(data$B12C_READ_ENGLISH == 3, 1, 0)))

# reading_english_proficiency_not_at_all_0
data$reading_english_proficiency_not_at_all_0 <- ifelse(data$speak_only_english_at_home_0 == 1, 0,
                                                        ifelse(!data$B12C_READ_ENGLISH %in% c(1, 2, 3, 4), NA, 
                                                               ifelse(data$B12C_READ_ENGLISH == 4, 1, 0)))

# currently_working_0
data$currently_working_0 <- ifelse(!data$B24_CURR_WORK %in% c(1, 2, 3), NA, 
                                   ifelse(data$B24_CURR_WORK == 1, 1, 0))

# worked_before_0
data$worked_before_0 <- ifelse(!data$B24_CURR_WORK %in% c(1, 2, 3), NA, 
                               ifelse(data$B24_CURR_WORK == 2, 1, 0))

# never_worked_0
data$never_worked_0 <- ifelse(!data$B24_CURR_WORK %in% c(1, 2, 3), NA, 
                              ifelse(data$B24_CURR_WORK == 3, 1, 0))

# tanf_assistance_0
data$tanf_assistance_0 <- ifelse(!data$B26C_PA %in% c(0, 1), NA, 
                                 ifelse(data$B26C_PA == 1, 1, 0))

# wic_or_snap_assistance_0
data$wic_or_snap_assistance_0 <- ifelse(!data$B26B_WICSNAP %in% c(0, 1), NA, 
                                        ifelse(data$B26B_WICSNAP == 1, 1, 0))

# future_school_part_time
data$future_school_part_time <- ifelse(!data$S01_FUTURE_SCHOOL %in% c(1, 2), NA, 
                                       ifelse(data$S01_FUTURE_SCHOOL == 1, 1, 0))

# future_school_full_time
data$future_school_full_time <- ifelse(!data$S01_FUTURE_SCHOOL %in% c(1, 2), NA, 
                                       ifelse(data$S01_FUTURE_SCHOOL == 2, 1, 0))

# future_work
data$future_work <- ifelse(!data$S02_FUTURE_WORK %in% c(1, 2), NA, 
                           ifelse(data$S02_FUTURE_WORK == 2, 1, 0))

# work_hours
data$work_hours <- ifelse(data$S02A_WORK_HOURS < 0 | data$S02A_WORK_HOURS > 100, NA, data$S02A_WORK_HOURS)

# financial_support_not_difficult_at_all
# financial_support_somewhat_difficult
# financial_support_very_difficult
data$financial_support_not_difficult_at_all <- ifelse(!data$S05_FINANCIAL_SUPPORT %in% c(1, 2, 3), NA, ifelse(data$S05_FINANCIAL_SUPPORT == 1, 1, 0))
data$financial_support_somewhat_difficult <- ifelse(!data$S05_FINANCIAL_SUPPORT %in% c(1, 2, 3), NA, ifelse(data$S05_FINANCIAL_SUPPORT == 2, 1, 0))
data$financial_support_very_difficult <- ifelse(!data$S05_FINANCIAL_SUPPORT %in% c(1, 2, 3), NA, ifelse(data$S05_FINANCIAL_SUPPORT == 3, 1, 0))

# career_knowledge_7_variables
data$S13A_ABILITIES[!data$S13A_ABILITIES %in% c(1, 2, 3, 4)] <- NA
data$S13B_PLAN[!data$S13B_PLAN %in% c(1, 2, 3, 4)] <- NA
data$S13C_HELP[!data$S13C_HELP %in% c(1, 2, 3, 4)] <- NA
data$S13D_JOB[!data$S13D_JOB %in% c(1, 2, 3, 4)] <- NA
data$S13E_ORGANIZATION[!data$S13E_ORGANIZATION %in% c(1, 2, 3, 4)] <- NA
data$S13F_OCCUPATION[!data$S13F_OCCUPATION %in% c(1, 2, 3, 4)] <- NA
data$S13G_EDUCATION[!data$S13G_EDUCATION %in% c(1, 2, 3, 4)] <- NA
data$career_knowledge_7_variables <- (data$S13A_ABILITIES + data$S13B_PLAN + data$S13C_HELP + data$S13D_JOB
                                      + data$S13E_ORGANIZATION + data$S13F_OCCUPATION + data$S13G_EDUCATION) / 7

# career_knowledge_3_variables
data$career_knowledge_3_variables <- (data$S13B_PLAN + data$S13E_ORGANIZATION + data$S13F_OCCUPATION) / 3

# training_commitment_index
data$S11CC_B_OTHER_THAN_SCHOOL[!data$S11CC_B_OTHER_THAN_SCHOOL %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11CC_F_STOP_ATTENDING[!data$S11CC_F_STOP_ATTENDING %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11CC_I_HOW_LONG[!data$S11CC_I_HOW_LONG %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11CC_P_DO_WELL[!data$S11CC_P_DO_WELL %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11CC_T_MOTIVIATED[!data$S11CC_T_MOTIVIATED %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11CC_DD_SUCCEED[!data$S11CC_DD_SUCCEED %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11CC_EE_BEST_CHOICE[!data$S11CC_EE_BEST_CHOICE %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11CC_GG_COMMITTED[!data$S11CC_GG_COMMITTED %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11CC_JJ_ACHIEVE_GOALS[!data$S11CC_JJ_ACHIEVE_GOALS %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11CC_LL_RIGHT_FOR_ME[!data$S11CC_LL_RIGHT_FOR_ME %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$training_commitment_index <- (data$S11CC_B_OTHER_THAN_SCHOOL + data$S11CC_F_STOP_ATTENDING + data$S11CC_I_HOW_LONG
                                   + data$S11CC_P_DO_WELL + data$S11CC_T_MOTIVIATED + data$S11CC_DD_SUCCEED
                                   + data$S11CC_EE_BEST_CHOICE + data$S11CC_GG_COMMITTED + data$S11CC_JJ_ACHIEVE_GOALS
                                   + data$S11CC_LL_RIGHT_FOR_ME) / 10

# academic_discipline_index
data$S11AD_D_DISCIPLINED[!data$S11AD_D_DISCIPLINED %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11AD_J_SKIP_CLASSES[!data$S11AD_J_SKIP_CLASSES %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11AD_M_ABILITIES[!data$S11AD_M_ABILITIES %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11AD_N_NOTES[!data$S11AD_N_NOTES %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11AD_R_DEADLINES[!data$S11AD_R_DEADLINES %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11AD_U_PERFORMANCE[!data$S11AD_U_PERFORMANCE %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11AD_II_DO_BEST[!data$S11AD_II_DO_BEST %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11AD_KK_CONSISTENTLY[!data$S11AD_KK_CONSISTENTLY %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11AD_NN_HARD_WORKING[!data$S11AD_NN_HARD_WORKING %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11AD_PP_ASSIGNMENTS[!data$S11AD_PP_ASSIGNMENTS %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$academic_discipline_index <-(data$S11AD_D_DISCIPLINED + data$S11AD_D_DISCIPLINED + data$S11AD_M_ABILITIES + data$S11AD_N_NOTES
                                  + data$S11AD_R_DEADLINES + data$S11AD_U_PERFORMANCE + data$S11AD_II_DO_BEST + data$S11AD_KK_CONSISTENTLY
                                  + data$S11AD_NN_HARD_WORKING + data$S11AD_PP_ASSIGNMENTS) / 10

# emotional_stability_index
data$S11ES_A_CALM[!data$S11ES_A_CALM %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11ES_E_LOSE_CONTROL[!data$S11ES_E_LOSE_CONTROL %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11ES_G_IRRITATED[!data$S11ES_G_IRRITATED %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11ES_L_UPSET[!data$S11ES_L_UPSET %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11ES_Q_EXPRESS_ANGER[!data$S11ES_Q_EXPRESS_ANGER %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11ES_S_THINK_CLEARLY[!data$S11ES_S_THINK_CLEARLY %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11ES_V_TEMPER[!data$S11ES_V_TEMPER %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11ES_W_ANNOYED[!data$S11ES_W_ANNOYED %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11ES_Y_PATIENT[!data$S11ES_Y_PATIENT %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11ES_AA_ARGUMENTS[!data$S11ES_AA_ARGUMENTS %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11ES_FF_FRUSTRATION[!data$S11ES_FF_FRUSTRATION %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$S11ES_HH_OUT_OF_CONTROL[!data$S11ES_HH_OUT_OF_CONTROL %in% c(1, 2, 3, 4, 5, 6)] <- NA
data$emotional_stability_index <- (data$S11ES_A_CALM + data$S11ES_E_LOSE_CONTROL + data$S11ES_G_IRRITATED + data$S11ES_L_UPSET
                                   + data$S11ES_Q_EXPRESS_ANGER + data$S11ES_S_THINK_CLEARLY + data$S11ES_V_TEMPER + data$S11ES_W_ANNOYED
                                   + data$S11ES_Y_PATIENT + data$S11ES_AA_ARGUMENTS + data$S11ES_FF_FRUSTRATION + data$S11ES_HH_OUT_OF_CONTROL) / 12

# social_support_index
data$S12A_DEPEND[!data$S12A_DEPEND %in% c(1, 2, 3, 4)] <- NA
data$S12B_CLOSE[!data$S12B_CLOSE %in% c(1, 2, 3, 4)] <- NA
data$S12C_TURN_TO[!data$S12C_TURN_TO %in% c(1, 2, 3, 4)] <- NA
data$S12D_ACTIVITIES[!data$S12D_ACTIVITIES %in% c(1, 2, 3, 4)] <- NA
data$S12E_RESPECT[!data$S12E_RESPECT %in% c(1, 2, 3, 4)] <- NA
data$S12F_ASSISTANCE[!data$S12F_ASSISTANCE %in% c(1, 2, 3, 4)] <- NA
data$S12G_EMOTIONAL_SECURITY[!data$S12G_EMOTIONAL_SECURITY %in% c(1, 2, 3, 4)] <- NA
data$S12H_COMPENTENCE[!data$S12H_COMPENTENCE %in% c(1, 2, 3, 4)] <- NA
data$S12I_INTERESTS[!data$S12I_INTERESTS %in% c(1, 2, 3, 4)] <- NA
data$S12J_TRUSTWORTHY[!data$S12J_TRUSTWORTHY %in% c(1, 2, 3, 4)] <- NA
data$social_support_index <- (data$S12A_DEPEND + data$S12B_CLOSE + data$S12C_TURN_TO + data$S12D_ACTIVITIES + data$S12E_RESPECT
                              + data$S12F_ASSISTANCE + data$S12G_EMOTIONAL_SECURITY + data$S12H_COMPENTENCE + data$S12I_INTERESTS
                              + data$S12J_TRUSTWORTHY) / 10

# life_challenges_index_6_variables
data$S15A_CHILD_CARE[!data$S15A_CHILD_CARE %in% c(1, 2, 3, 4, 5)] <- NA
data$S15B_TRANSPORTATION[!data$S15B_TRANSPORTATION %in% c(1, 2, 3, 4, 5)] <- NA
data$S15C_ALCOHOL[!data$S15C_ALCOHOL %in% c(1, 2, 3, 4, 5)] <- NA
data$S15D_ILLNESS[!data$S15D_ILLNESS %in% c(1, 2, 3, 4, 5)] <- NA
data$S15E_ARGUMENTS[!data$S15E_ARGUMENTS %in% c(1, 2, 3, 4, 5)] <- NA
data$S15F_VIOLENCE[!data$S15F_VIOLENCE %in% c(1, 2, 3, 4, 5)] <- NA
data$life_challenges_index_6_variables <- (data$S15A_CHILD_CARE + data$S15B_TRANSPORTATION + data$S15C_ALCOHOL + data$S15D_ILLNESS
                                           + data$S15E_ARGUMENTS + data$S15F_VIOLENCE) / 6

# life_challenges_index_4_variables
data$life_challenges_index_4_variables <- (data$S15A_CHILD_CARE + data$S15B_TRANSPORTATION + data$S15C_ALCOHOL + data$S15D_ILLNESS) / 4

# stress_index
data$S14A_CONTROL[!data$S14A_CONTROL %in% c(1, 2, 3, 4, 5)] <- NA
data$S14B_CONFIDENCE[!data$S14B_CONFIDENCE %in% c(1, 2, 3, 4, 5)] <- NA
data$S14C_GOING_YOUR_WAY[!data$S14C_GOING_YOUR_WAY %in% c(1, 2, 3, 4, 5)] <- NA
data$S14D_DIFFICULTIES[!data$S14D_DIFFICULTIES %in% c(1, 2, 3, 4, 5)] <- NA
data$stress_index <- (data$S14A_CONTROL + data$S14B_CONFIDENCE + data$S14C_GOING_YOUR_WAY + data$S14D_DIFFICULTIES) / 4

# weight_15
# weight_36
# weight_72
data <- data %>% rename(weight_15 = WEIGHT15) 
data <- data %>% rename(weight_36 = ACE_SIMILARWT_3DIM) 
data <- data %>% rename(weight_72 = WEIGHT72NDNHPS)

# ----------------- Extract Required Columns (& some more for testing/logging purposes) ----------------- 
pace_df <- data %>% 
  select(participant_id, site_id, 
         surveyrespondent_15_pace, surveyrespondent_36_pace, surveyrespondent_72_pace, SITE_NUM.x, R_YEAR_UP_SUB, study_pace, study_year_up, 
         currently_working_15, currently_working_36, currently_working_72,
         hours_worked_per_week_15, hours_worked_per_week_36, hours_worked_per_week_alt_36, hours_worked_per_week_72,
         hourly_wage_rate_15, hourly_wage_rate_36, hourly_wage_rate_alt_36, hourly_wage_rate_72,
         weekly_earnings_15, weekly_earnings_36, weekly_earnings_alt_36, weekly_earnings_72, treatment_group_0, 
         age_LT21_0, age_21To24_0, age_25To34_0, age_GE35_0, AGE,
         ethnicity_hispanic_0, 
         race_white_0, B10_RACE_WHITE,
         race_black_0, B10_RACE_BLACK,
         race_asian_0, B10_RACE_ASIAN,
         race_american_indian_0, B10_RACE_AM_INDIAN,
         race_pacific_islander_0, B10_RACE_PACIFIC,
         No_HS_in_US_0, grades_mostly_A_0, grades_mostly_B_0, grades_mostly_C_0,
         grades_mostly_D_0, grades_mostly_F_0, B17_EDUC, educ_no_hs_credential_0, educ_ged_or_alternative_credential_0, educ_regular_high_school_diploma_0,
         educ_some_college_0, educ_bachelors_degree_or_higher_0, postsec_voc_tech_certificate_0, sex_male_0, 
         B08_MARSTAT,
         marstat_married_0, marstat_widowed_0,
         marstat_divorced_or_separated_0, marstat_never_married_0, number_of_children_home_0, any_children_0,
         birth_country_usa_0, 
         B12A_OTHER_LANG, speak_only_english_at_home_0,
         B12B_SPEAK_ENGLISH, spoken_english_proficiency_very_well_0, spoken_english_proficiency_well_0, spoken_english_proficiency_not_well_0, 
         spoken_english_proficiency_not_at_all_0, 
         B12C_READ_ENGLISH, reading_english_proficiency_very_well_0, reading_english_proficiency_well_0,
         reading_english_proficiency_not_well_0, reading_english_proficiency_not_at_all_0, currently_working_0, worked_before_0, never_worked_0,
         tanf_assistance_0, wic_or_snap_assistance_0, future_school_part_time, future_school_full_time, future_work, work_hours,
         financial_support_not_difficult_at_all, financial_support_somewhat_difficult, financial_support_very_difficult, career_knowledge_3_variables,
         career_knowledge_7_variables, training_commitment_index, academic_discipline_index, emotional_stability_index, social_support_index,
         life_challenges_index_4_variables, life_challenges_index_6_variables, stress_index, weight_15, weight_36, weight_72)


# Fetch summary for Code Book
stats <- sapply(pace_df, calculate_stats)

# ----------------- Write extracted info. to CSV ----------------- 
folder_path <- file.path(root, "read-PACE-output")

# Create the folder if it doesn't exist
if(!dir.exists(folder_path)) { 
  dir.create(folder_path)
}

write.csv(pace_df, file.path(folder_path,"PACE-DataFrame.csv"), row.names = FALSE)

# Remove all data frames
rm(list = ls())
gc()