library(readr)
library(dplyr)
library(openxlsx)
library(rprojroot)

# Find the root of the current project
root <- find_root(is_rstudio_project)
source("utils.R")

hpog_df <- read.csv('read-HPOG1.0-output/HPOG-1.0-DataFrame.csv')
pace_df <- read.csv('read-PACE-output/PACE-DataFrame.csv')

# Remove 1 row from HPOG where participant_id is NA
hpog_df <- hpog_df[!is.na(hpog_df$participant_id), ]

# FINAL MERGE: hpog_df + pace_df
data <- full_join(hpog_df, pace_df, by = c("participant_id"))

# Perform merging of columns and some more manipulation. Retaining HPOG 1.0 values.

# study_hpog & study_pace
data$study_hpog[is.na(data$study_hpog)] <- 0
data$study_pace[is.na(data$study_pace)] <- 0

# site_id
data$site_id <- coalesce(data$site_id.x, data$site_id.y) # preserve HPOG values

# currently_working_15.x
data$currently_working_15 <- coalesce(data$currently_working_15.x, data$currently_working_15.y)

# currently_working_36.x
data$currently_working_36 <- coalesce(data$currently_working_36.x, data$currently_working_36.y)

# currently_working_72.x
data$currently_working_72 <- coalesce(data$currently_working_72.x, data$currently_working_72.y)

# hours_worked_per_week_15.x
data$hours_worked_per_week_15 <- coalesce(data$hours_worked_per_week_15.x, data$hours_worked_per_week_15.y)

# hours_worked_per_week_36.x
data$hours_worked_per_week_36 <- coalesce(data$hours_worked_per_week_36.x, data$hours_worked_per_week_36.y)

# hours_worked_per_week_alt_36.x
# print(sum(data$hours_worked_per_week_alt_36.x != data$hours_worked_per_week_alt_36.y, na.rm = T)) # 1
data$hours_worked_per_week_alt_36 <- coalesce(data$hours_worked_per_week_alt_36.x, data$hours_worked_per_week_alt_36.y)

# hours_worked_per_week_72.x
data$hours_worked_per_week_72 <- coalesce(data$hours_worked_per_week_72.x, data$hours_worked_per_week_72.y)

# hourly_wage_rate_15.x
data$hourly_wage_rate_15 <- coalesce(data$hourly_wage_rate_15.x, data$hourly_wage_rate_15.y)

# hourly_wage_rate_36.x
# print(sum(data$hourly_wage_rate_36.x != data$hourly_wage_rate_36.y, na.rm = T)) # 20
data$hourly_wage_rate_36 <- coalesce(data$hourly_wage_rate_36.x, data$hourly_wage_rate_36.y)

# hourly_wage_rate_alt_36.x
# print(sum(data$hourly_wage_rate_alt_36.x != data$hourly_wage_rate_alt_36.y, na.rm = T)) # 17
data$hourly_wage_rate_alt_36 <- coalesce(data$hourly_wage_rate_alt_36.x, data$hourly_wage_rate_alt_36.y)

# hourly_wage_rate_72.x
# print(sum(data$hourly_wage_rate_72.x != data$hourly_wage_rate_72.y, na.rm = T)) # 9
data$hourly_wage_rate_72 <- coalesce(data$hourly_wage_rate_72.x, data$hourly_wage_rate_72.y)

# weekly_earnings_15.x
data$weekly_earnings_15 <- coalesce(data$weekly_earnings_15.x, data$weekly_earnings_15.y)

# weekly_earnings_36.x
data$weekly_earnings_36 <- coalesce(data$weekly_earnings_36.x, data$weekly_earnings_36.y)

# weekly_earnings_alt_36.x
# print(sum(data$weekly_earnings_alt_36.x != data$weekly_earnings_alt_36.y, na.rm = T)) # 14
data$weekly_earnings_alt_36 <- coalesce(data$weekly_earnings_alt_36.x, data$weekly_earnings_alt_36.y)

# weekly_earnings_72.x
# print(sum(data$weekly_earnings_72.x != data$weekly_earnings_72.y, na.rm = T)) # 12
data$weekly_earnings_72 <- coalesce(data$weekly_earnings_72.x, data$weekly_earnings_72.y)

# treatment_group_0.x
data$treatment_group_0 <- coalesce(data$treatment_group_0.x, data$treatment_group_0.y)

# age_LT21_0.x
data$age_LT21_0 <- coalesce(data$age_LT21_0.x, data$age_LT21_0.y)

# age_21To24_0.x
data$age_21To24_0 <- coalesce(data$age_21To24_0.x, data$age_21To24_0.y)

# age_25To34_0.x
data$age_25To34_0 <- coalesce(data$age_25To34_0.x, data$age_25To34_0.y)

# age_GE35_0.x
data$age_GE35_0 <- coalesce(data$age_GE35_0.x, data$age_GE35_0.y)

# ethnicity_hispanic_0.x
# print(sum(data$ethnicity_hispanic_0.x != data$ethnicity_hispanic_0.y, na.rm = T)) # 3
data$ethnicity_hispanic_0 <- coalesce(data$ethnicity_hispanic_0.x, data$ethnicity_hispanic_0.y)

# race_white_0.x
# print(sum(data$race_white_0.x != data$race_white_0.y, na.rm = T)) # 35
data$race_white_0 <- coalesce(data$race_white_0.x, data$race_white_0.y)

# race_black_0.x
# print(sum(data$race_black_0.x != data$race_black_0.y, na.rm = T)) # 13
data$race_black_0 <- coalesce(data$race_black_0.y, data$race_black_0.x)

# race_asian_0.x
# print(sum(data$race_asian_0.x != data$race_asian_0.y, na.rm = T)) # 4
data$race_asian_0 <- coalesce(data$race_asian_0.x, data$race_asian_0.y)

# race_american_indian_0.x
# print(sum(data$race_american_indian_0.x != data$race_american_indian_0.y, na.rm = T)) # 4
data$race_american_indian_0 <- coalesce(data$race_american_indian_0.x, data$race_american_indian_0.y)

# race_pacific_islander_0.x
# print(sum(data$race_pacific_islander_0.x != data$race_pacific_islander_0.y, na.rm = T)) # 1
data$race_pacific_islander_0 <- coalesce(data$race_pacific_islander_0.x, data$race_pacific_islander_0.y)

# educ_no_hs_credential_0.x
# print(sum(data$educ_no_hs_credential_0.x != data$educ_no_hs_credential_0.y, na.rm = T)) # 53
data$educ_no_hs_credential_0 <- coalesce(data$educ_no_hs_credential_0.x, data$educ_no_hs_credential_0.y)

# educ_ged_or_alternative_credential_0.x
# print(sum(data$educ_ged_or_alternative_credential_0.x != data$educ_ged_or_alternative_credential_0.y, na.rm = T)) # 150
data$educ_ged_or_alternative_credential_0 <- coalesce(data$educ_ged_or_alternative_credential_0.x, data$educ_ged_or_alternative_credential_0.y)

# educ_regular_high_school_diploma_0.x
# print(sum(data$educ_regular_high_school_diploma_0.x != data$educ_regular_high_school_diploma_0.y, na.rm = T)) # 685
data$educ_regular_high_school_diploma_0 <- coalesce(data$educ_regular_high_school_diploma_0.x, data$educ_regular_high_school_diploma_0.y)

# educ_some_college_0.x               
# print(sum(data$educ_some_college_0.x != data$educ_some_college_0.y, na.rm = T)) # 224
data$educ_some_college_0 <- coalesce(data$educ_some_college_0.x, data$educ_some_college_0.y)

# educ_bachelors_degree_or_higher_0.x
# print(sum(data$educ_bachelors_degree_or_higher_0.x != data$educ_bachelors_degree_or_higher_0.y, na.rm = T)) # 31
data$educ_bachelors_degree_or_higher_0 <- coalesce(data$educ_bachelors_degree_or_higher_0.x, data$educ_bachelors_degree_or_higher_0.y)

# sex_male_0.x                    
# print(sum(data$sex_male_0.x != data$sex_male_0.y, na.rm = T)) # 2
data$sex_male_0 <- coalesce(data$sex_male_0.x, data$sex_male_0.y)

# marstat_married_0.x             
# print(sum(data$marstat_married_0.x != data$marstat_married_0.y, na.rm = T)) # 18
data$marstat_married_0 <- coalesce(data$marstat_married_0.x, data$marstat_married_0.y)

# marstat_widowed_0.x             
# print(sum(data$marstat_widowed_0.x != data$marstat_widowed_0.y, na.rm = T)) # 1
data$marstat_widowed_0 <- coalesce(data$marstat_widowed_0.x, data$marstat_widowed_0.y)

# marstat_divorced_or_separated_0.x
# print(sum(data$marstat_divorced_or_separated_0.x != data$marstat_divorced_or_separated_0.y, na.rm = T)) # 13
data$marstat_divorced_or_separated_0 <- coalesce(data$marstat_divorced_or_separated_0.x, data$marstat_divorced_or_separated_0.y)

# marstat_never_married_0.x        
# print(sum(data$marstat_never_married_0.x != data$marstat_never_married_0.y, na.rm = T)) # 16
data$marstat_never_married_0 <- coalesce(data$marstat_never_married_0.x, data$marstat_never_married_0.y)

# currently_working_0.x              
# print(sum(data$currently_working_0.x != data$currently_working_0.y, na.rm = T)) # 41
data$currently_working_0 <- coalesce(data$currently_working_0.x, data$currently_working_0.y)

# worked_before_0.x                  
# print(sum(data$worked_before_0.x != data$worked_before_0.y, na.rm = T)) # 514
data$worked_before_0 <- coalesce(data$worked_before_0.x, data$worked_before_0.y)

# never_worked_0.x                   
# print(sum(data$never_worked_0.x != data$never_worked_0.y, na.rm = T)) # 28
data$never_worked_0 <- coalesce(data$never_worked_0.x, data$never_worked_0.y)

# tanf_assistance_0.x                
data$tanf_assistance_0 <- coalesce(data$tanf_assistance_0.x, data$tanf_assistance_0.y)

# wic_or_snap_assistance_0.x         
data$wic_or_snap_assistance_0 <- coalesce(data$wic_or_snap_assistance_0.x, data$tanf_assistance_0.y)

# future_school_part_time.x          
data$future_school_part_time <- coalesce(data$future_school_part_time.x, data$future_school_part_time.y)

# future_school_full_time.x         
data$future_school_full_time <- coalesce(data$future_school_full_time.x, data$future_school_full_time.y)

# future_work.x                      
data$future_work <- coalesce(data$future_work.x, data$future_work.y)

# work_hours.x                
data$work_hours <- coalesce(data$work_hours.x, data$work_hours.y)

# career_knowledge_3_variables
data$career_knowledge_3_variables <- coalesce(data$career_knowledge_3_variables.x, data$career_knowledge_3_variables.y)

# life_challenges_index_4_variables.x
data$life_challenges_index_4_variables <- coalesce(data$life_challenges_index_4_variables.x, data$life_challenges_index_4_variables.y)

# birth_country_usa_0.x
# print(sum(data$birth_country_usa_0.x != data$birth_country_usa_0.y, na.rm = T)) # 77
data$birth_country_usa_0 <- coalesce(data$birth_country_usa_0.x, data$birth_country_usa_0.y)

# study_year_up
data$study_year_up[is.na(data$study_year_up)] <- 0

# any_children_0
data$any_children_0 <- coalesce(data$any_children_0.x, data$any_children_0.y)

# write_tsv(output, "educ_XX_HPOG_ONLY_variables[AFTER_CHANGE].tsv")

# ----------------- Extract Required Columns ONLY ----------------- 
df <- data %>% 
  select(participant_id, site_id, study_hpog, study_pace, study_year_up, 
         surveyrespondent_15_hpog, surveyrespondent_36_hpog, surveyrespondent_72_hpog, 
         surveyrespondent_15_pace, surveyrespondent_36_pace, surveyrespondent_72_pace,
         credential_15, credential_36, credential_72, currently_working_15, 
         currently_working_36, currently_working_72, hours_worked_per_week_15, hours_worked_per_week_36, hours_worked_per_week_alt_36,
         hours_worked_per_week_72, hourly_wage_rate_15, hourly_wage_rate_36, hourly_wage_rate_alt_36, hourly_wage_rate_72,
         weekly_earnings_15, weekly_earnings_36, weekly_earnings_alt_36, weekly_earnings_72, treatment_group_0, age_in_years_0,
         age_LT21_0, age_21To24_0, age_25To34_0, age_GE35_0, ethnicity_hispanic_0, race_white_0, race_black_0, race_asian_0,
         race_american_indian_0, race_pacific_islander_0, No_HS_in_US_0, grades_mostly_A_0, grades_mostly_B_0, grades_mostly_C_0,
         grades_mostly_D_0, grades_mostly_F_0, educ_no_hs_credential_0, educ_ged_or_alternative_credential_0,
         educ_regular_high_school_diploma_0, educ_some_college_0, educ_bachelors_degree_or_higher_0, postsec_voc_tech_certificate_0, occupational_license_certification_0,
         sex_male_0, marstat_married_0, marstat_widowed_0, marstat_divorced_or_separated_0, marstat_never_married_0, 
         number_of_children_dependent_0, number_of_children_home_0, any_children_0,
         birth_country_usa_0, speak_only_english_at_home_0,
         spoken_english_proficiency_very_well_0, spoken_english_proficiency_well_0, spoken_english_proficiency_not_well_0, 
         spoken_english_proficiency_not_at_all_0, reading_english_proficiency_very_well_0, reading_english_proficiency_well_0,
         reading_english_proficiency_not_well_0, reading_english_proficiency_not_at_all_0, limited_english_0, currently_working_0, worked_before_0, never_worked_0, tanf_assistance_0, wic_or_snap_assistance_0,
         future_school_part_time, future_school_full_time, future_work, work_hours, financial_support_not_difficult_at_all, financial_support_somewhat_difficult, financial_support_very_difficult,
         career_knowledge_3_variables, career_knowledge_7_variables, life_challenges_index_4_variables, life_challenges_index_6_variables, stress_index,
         training_commitment_index, academic_discipline_index, emotional_stability_index, social_support_index, 
         weight_15, weight_36, weight_72,
         number_of_services, behavioral_incentives, avg_fte_caseload, childcare_transport, social_services, financial_services, 
         number_of_employment_supports, number_of_colocated_services, emergency_assistance, peer_support, number_of_cp_principles, 
         proportion_local_some_college, proportion_local_jobs_health_care, median_wage_local_health_care, proportion_local_cash_assistance,
         proportion_local_enrolled_school, total_msa_population, percent_local_unemployed)


df$surveyrespondent_72_hpog[is.na(df$surveyrespondent_72_hpog)] <- 0
df$surveyrespondent_72_pace[is.na(df$surveyrespondent_72_pace)] <- 0

# 1 Indicates HPOG 1.0 (including the joint HPOG1.0/PACE records)
# 2 Indicates Year Up
# 3 Indicates PACE but not Year Up and not joint with HPOG1.0/PACE records
df$study_type <- NA
df$study_type[df$study_hpog == 1] <- 1
df$study_type[df$study_year_up == 1] <- 2
df$study_type[df$study_hpog == 0 & df$study_pace == 1 & df$study_year_up == 0] <- 3

# Extract 3 studies
all_hpog <- df %>% filter(study_type == 1)
year_up <- df %>% filter(study_type == 2)
pace_only <- df %>% filter(study_type == 3)

all_hpog_df <- all_hpog %>% 
  select(participant_id, site_id, study_hpog, surveyrespondent_15_hpog, surveyrespondent_36_hpog, surveyrespondent_72_hpog, 
         credential_15, credential_36, credential_72, 
         currently_working_15,currently_working_36, currently_working_72, 
         hours_worked_per_week_15, hours_worked_per_week_36, hours_worked_per_week_alt_36, hours_worked_per_week_72,
         hourly_wage_rate_15, hourly_wage_rate_36, hourly_wage_rate_alt_36, hourly_wage_rate_72,
         weekly_earnings_15, weekly_earnings_36, weekly_earnings_alt_36, weekly_earnings_72, 
         treatment_group_0, age_in_years_0, age_LT21_0, age_21To24_0, age_25To34_0, age_GE35_0, 
         ethnicity_hispanic_0, race_white_0, race_black_0, race_asian_0, race_american_indian_0, race_pacific_islander_0,
         educ_no_hs_credential_0, educ_ged_or_alternative_credential_0, educ_regular_high_school_diploma_0, 
         educ_some_college_0, educ_bachelors_degree_or_higher_0, occupational_license_certification_0,
         sex_male_0, 
         marstat_married_0, marstat_widowed_0, marstat_divorced_or_separated_0, marstat_never_married_0, 
         number_of_children_dependent_0, any_children_0,
         birth_country_usa_0, limited_english_0, currently_working_0, worked_before_0, never_worked_0, tanf_assistance_0, wic_or_snap_assistance_0,
         future_school_part_time, future_school_full_time, future_work, work_hours, career_knowledge_3_variables, life_challenges_index_4_variables,
         number_of_services, behavioral_incentives, avg_fte_caseload, childcare_transport, social_services, financial_services, 
         number_of_employment_supports, number_of_colocated_services, emergency_assistance, peer_support, number_of_cp_principles, 
         proportion_local_some_college, proportion_local_jobs_health_care, median_wage_local_health_care, proportion_local_cash_assistance,
         proportion_local_enrolled_school, total_msa_population, percent_local_unemployed)

pace_only_df <- pace_only %>% 
  select(participant_id, site_id, surveyrespondent_15_pace, surveyrespondent_36_pace, surveyrespondent_72_pace, study_pace, study_year_up, 
         currently_working_15, currently_working_36, currently_working_72,
         hours_worked_per_week_15, hours_worked_per_week_36, hours_worked_per_week_alt_36, hours_worked_per_week_72,
         hourly_wage_rate_15, hourly_wage_rate_36, hourly_wage_rate_alt_36, hourly_wage_rate_72,
         weekly_earnings_15, weekly_earnings_36, weekly_earnings_alt_36, weekly_earnings_72, treatment_group_0, 
         age_LT21_0, age_21To24_0, age_25To34_0, age_GE35_0,
         ethnicity_hispanic_0, race_white_0, race_black_0, race_asian_0, race_american_indian_0, race_pacific_islander_0,
         No_HS_in_US_0, 
         grades_mostly_A_0, grades_mostly_B_0, grades_mostly_C_0, grades_mostly_D_0, grades_mostly_F_0, 
         educ_no_hs_credential_0, educ_ged_or_alternative_credential_0, educ_regular_high_school_diploma_0,
         educ_some_college_0, educ_bachelors_degree_or_higher_0, postsec_voc_tech_certificate_0, 
         sex_male_0, 
         marstat_married_0, marstat_widowed_0, marstat_divorced_or_separated_0, marstat_never_married_0, 
         number_of_children_home_0, any_children_0, birth_country_usa_0, speak_only_english_at_home_0, 
         spoken_english_proficiency_very_well_0, spoken_english_proficiency_well_0, spoken_english_proficiency_not_well_0, spoken_english_proficiency_not_at_all_0,  
         reading_english_proficiency_very_well_0, reading_english_proficiency_well_0, reading_english_proficiency_not_well_0, reading_english_proficiency_not_at_all_0, 
         currently_working_0, worked_before_0, never_worked_0,
         tanf_assistance_0, wic_or_snap_assistance_0, future_school_part_time, future_school_full_time, future_work, work_hours,
         financial_support_not_difficult_at_all, financial_support_somewhat_difficult, financial_support_very_difficult, career_knowledge_3_variables,
         career_knowledge_7_variables, training_commitment_index, academic_discipline_index, emotional_stability_index, social_support_index,
         life_challenges_index_4_variables, life_challenges_index_6_variables, stress_index, weight_15, weight_36, weight_72)

year_up_df <- year_up %>% 
  select(participant_id, site_id, 
         surveyrespondent_15_pace, surveyrespondent_36_pace, surveyrespondent_72_pace, study_pace, study_year_up, 
         currently_working_15, currently_working_36, currently_working_72,
         hours_worked_per_week_15, hours_worked_per_week_36, hours_worked_per_week_alt_36, hours_worked_per_week_72,
         hourly_wage_rate_15, hourly_wage_rate_36, hourly_wage_rate_alt_36, hourly_wage_rate_72,
         weekly_earnings_15, weekly_earnings_36, weekly_earnings_alt_36, weekly_earnings_72, treatment_group_0, 
         age_LT21_0, age_21To24_0, age_25To34_0, age_GE35_0, 
         ethnicity_hispanic_0, race_white_0, race_black_0, race_asian_0, race_american_indian_0, race_pacific_islander_0,
         No_HS_in_US_0, 
         grades_mostly_A_0, grades_mostly_B_0, grades_mostly_C_0, grades_mostly_D_0, grades_mostly_F_0, 
         educ_no_hs_credential_0, educ_ged_or_alternative_credential_0, educ_regular_high_school_diploma_0,
         educ_some_college_0, educ_bachelors_degree_or_higher_0, postsec_voc_tech_certificate_0, 
         sex_male_0, 
         marstat_married_0, marstat_widowed_0, marstat_divorced_or_separated_0, marstat_never_married_0, 
         number_of_children_home_0, any_children_0, birth_country_usa_0, speak_only_english_at_home_0, 
         spoken_english_proficiency_very_well_0, spoken_english_proficiency_well_0, spoken_english_proficiency_not_well_0, spoken_english_proficiency_not_at_all_0,  
         reading_english_proficiency_very_well_0, reading_english_proficiency_well_0, reading_english_proficiency_not_well_0, reading_english_proficiency_not_at_all_0, 
         currently_working_0, worked_before_0, never_worked_0,
         tanf_assistance_0, wic_or_snap_assistance_0, future_school_part_time, future_school_full_time, future_work, work_hours,
         financial_support_not_difficult_at_all, financial_support_somewhat_difficult, financial_support_very_difficult, career_knowledge_3_variables,
         career_knowledge_7_variables, training_commitment_index, academic_discipline_index, emotional_stability_index, social_support_index,
         life_challenges_index_4_variables, life_challenges_index_6_variables, stress_index, weight_15, weight_36, weight_72)


hpog_survey_response_15 <- all_hpog_df %>% 
  filter(surveyrespondent_15_hpog == 1)

hpog_survey_response_36 <- all_hpog_df %>% 
  filter(surveyrespondent_36_hpog == 1)

hpog_survey_response_72 <- all_hpog_df %>% 
  filter(surveyrespondent_72_hpog == 1)

year_up_15 <- year_up_df %>% 
  filter(surveyrespondent_15_pace == 1)

year_up_36 <- year_up_df %>% 
  filter(surveyrespondent_36_pace == 1)

year_up_72 <- year_up_df %>% 
  filter(surveyrespondent_72_pace == 1)

pace_survey_response_15 <- pace_only_df %>% 
  filter(surveyrespondent_15_pace == 1)

pace_survey_response_36 <- pace_only_df %>% 
  filter(surveyrespondent_36_pace == 1)

pace_survey_response_72 <- pace_only_df %>% 
  filter(surveyrespondent_72_pace == 1)

# Fetch summary for Code Book
# sapply(df, calculate_stats)

sapply(hpog_survey_response_36, calculate_stats)

# ----------------- Write extracted info. to CSV ----------------- 
write.csv(df, file.path(root,"Analysis-DataFrame.csv"), row.names = FALSE)
write.csv(all_hpog_df, file.path(root,"ALL-HPOG-1.0-DataFrame.csv"), row.names = FALSE)
write.csv(pace_only_df, file.path(root,"PACE-ONLY-DataFrame.csv"), row.names = FALSE)
write.csv(year_up_df, file.path(root,"YEAR-UP-DataFrame.csv"), row.names = FALSE)

# Remove all data frames
rm(list = ls())
gc()