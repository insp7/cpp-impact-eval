library(readr)
library(dplyr)
library(openxlsx)
library(rprojroot)

# Find the root of the current project
root <- find_root(is_rstudio_project)

source("utils.R")

HPOG_DATA_PATH <- 'read-HPOG1.0-output/'

# ----------------- Read HPOG 1.0 Data Frame ----------------- 
data <- read.csv(paste(HPOG_DATA_PATH, 'hpog1.0-joined-df.csv', sep = ""))

# ----------------- Data Manipulation ----------------- 

# participant_id
data <- data %>% rename(participant_id = ABTSRBIID)

# site_id
data <- data %>% rename(site_id = USERSITE.x)

# surveyrespondent_15_hpog
data <- data %>% rename(surveyrespondent_15_hpog = surveyrespondent_15)

# surveyrespondent_36_hpog
data <- data %>% rename(surveyrespondent_36_hpog = surveyrespondent_36)

# surveyrespondent_72_hpog
data <- data %>% rename(surveyrespondent_72_hpog = surveyrespondent_72)

# study_hpog
data$study_hpog <- 1

# credential_15
data$credential_15 <- ifelse(!data$CREDENTIAL %in% c(0, 1), NA, data$CREDENTIAL)

# credential_36
data$credential_36 <- ifelse(!data$LICENSE36 %in% c(0, 1) & data$CREDENTIAL == 0, NA,
                             ifelse(data$LICENSE36 == 0 & !data$CREDENTIAL %in% c(0, 1), NA,
                                    ifelse(!data$LICENSE36 %in% c(0, 1) & !data$CREDENTIAL %in% c(0, 1), NA,
                                           ifelse(data$CREDENTIAL == 1 | data$LICENSE36 == 1, 1, 0))))

# credential_72
data$credential_72 <- ifelse(!data$EDPROGRESS_ANYCREDENTIAL %in% c(0, 1), NA, data$EDPROGRESS_ANYCREDENTIAL)

# currently_working_15
data$currently_working_15 <- ifelse(!data$CURRENTWORK %in% c(0, 1), NA, data$CURRENTWORK)

# currently_working_36
data$currently_working_36 <- ifelse(!data$EMPLOYEDATFOLLOWUP %in% c(0, 1), NA, data$EMPLOYEDATFOLLOWUP)

# currently_working_72
data$currently_working_72 <- ifelse(!data$EMPLOYMENT_EMPLOYED %in% c(0, 1), NA, data$EMPLOYMENT_EMPLOYED)

# hours_worked_per_week_15
data$hours_worked_per_week_15 <- ifelse(!data$Q36 %in% c(1, 2), NA,
                                        ifelse(data$Q36 == 2, 0,
                                               ifelse(data$Q36A < 5 | data$Q36A > 81, NA,
                                                      data$Q36A)))

# hourly_wage_rate_15
data$hourly_wage_rate_15 <- ifelse(!data$Q36 %in% c(1, 2), NA,
                                   ifelse(data$Q36 == 2, 0,
                                   ifelse(data$Q36B < 2.5 | data$Q36B > 40.01, NA,
                                          data$Q36B)))

# weekly_earnings_15
data$weekly_earnings_15 <- data$hours_worked_per_week_15 * data$hourly_wage_rate_15

# hours_worked_per_week_36
data$hours_worked_per_week_36 <- ifelse(!data$EMPLOYEDATFOLLOWUP %in% c(0, 1), NA,
                                        ifelse(data$EMPLOYEDATFOLLOWUP == 0, 0,
                                               data$employment_weekhours))

# hourly_wage_rate_36
data$hourly_wage_rate_36 <- data$employment_hourlywage

# weekly_earnings_36
data$weekly_earnings_36 <- ifelse(!data$EMPLOYEDATFOLLOWUP %in% c(0, 1), NA,
                                  ifelse(data$EMPLOYEDATFOLLOWUP == 0, 0,
                                         data$hours_worked_per_week_36 * data$hourly_wage_rate_36))

# hours_worked_per_week_alt_36
data$hours_worked_per_week_alt_36 <- ifelse(!data$EMPLOYEDATFOLLOWUP %in% c(0, 1), NA,
                                            ifelse(data$EMPLOYEDATFOLLOWUP == 0, 0,
                                                   data$employment_weekhours_alt))

# hourly_wage_rate_alt_36
data$hourly_wage_rate_alt_36 <- data$employment_hourlywage_alt

# weekly_earnings_alt_36
data$weekly_earnings_alt_36 <- ifelse(!data$EMPLOYEDATFOLLOWUP %in% c(0, 1), NA,
                                      ifelse(data$EMPLOYEDATFOLLOWUP == 0, 0,
                                             data$hours_worked_per_week_alt_36 * data$hourly_wage_rate_alt_36))

# hours_worked_per_week_72
data$hours_worked_per_week_72 <- ifelse(!data$EMPLOYMENT_EMPLOYED %in% c(0, 1), NA,
                                        ifelse(data$EMPLOYMENT_EMPLOYED == 0, 0,
                                               ifelse(data$EMPLOYMENT_WEEKHOURS < 0 | data$EMPLOYMENT_WEEKHOURS > 80, NA,
                                                      data$EMPLOYMENT_WEEKHOURS)))

# hourly_wage_rate_72
data$hourly_wage_rate_72 <- ifelse(data$EMPLOYMENT_HOURLYWAGE < 1 | data$EMPLOYMENT_HOURLYWAGE > 75, NA, data$EMPLOYMENT_HOURLYWAGE)

# weekly_earnings_72
data$weekly_earnings_72 <- ifelse(data$EMPLOYMENT_WEEKEARNINGS < 0 | data$EMPLOYMENT_WEEKEARNINGS > 3500, NA, data$EMPLOYMENT_WEEKEARNINGS)

# treatment_group_0
data$treatment_group_0 <- ifelse(data$RANDOM_ASSIGNMENT %in% c(1, 2), 1, 0)

# age: TODO
data$ra_month_year <- str_match(data$DATE_RANDOM, "(\\d{2})/(\\d{4})")
data$ra_month <- as.numeric(data$ra_month_year[,2])
data$ra_year <- as.numeric(data$ra_month_year[,3])
data$DOB_YEAR[data$DOB_YEAR == -95] <- NA
data$age_in_years_0 <- data$ra_year - data$DOB_YEAR
data$age_in_years_0 <- data$age_in_years_0 - (data$ra_month < data$DOB_MONTH)

# age_LT21_0
# age_21To24_0
# age_25To34_0
# age_GE35_0
data$age_LT21_0 <- ifelse(data$age_in_years_0 >= 0 & data$age_in_years_0 < 21, 1, 0)
data$age_21To24_0 <- ifelse(data$age_in_years_0 >= 21 & data$age_in_years_0 < 25, 1, 0)
data$age_25To34_0 <- ifelse(data$age_in_years_0 >= 25 & data$age_in_years_0 < 35, 1, 0)
data$age_GE35_0 <- ifelse(data$age_in_years_0 >= 35, 1, 0)

# ethnicity_hispanic_0
# race_white_0
# race_black_0
# race_asian_0
# race_american_indian_0
# race_pacific_islander_0
data$ethnicity_hispanic_0 <- ifelse(!data$ETHNICITY %in% c(1, 2), NA, ifelse(data$ETHNICITY == 2, 0, 1))
data$race_white_0 <- ifelse(!data$RACE_W %in% c(1, 2), NA, ifelse(data$RACE_W == 1, 1, 0))
data$race_black_0 <- ifelse(!data$RACE_B %in% c(1, 2), NA, ifelse(data$RACE_B == 1, 1, 0))
data$race_asian_0 <- ifelse(!data$RACE_A %in% c(1, 2), NA, ifelse(data$RACE_A == 1, 1, 0))
data$race_american_indian_0 <- ifelse(!data$RACE_NA %in% c(1, 2), NA, ifelse(data$RACE_NA == 1, 1, 0))
data$race_pacific_islander_0 <- ifelse(!data$RACE_HPI %in% c(1, 2), NA, ifelse(data$RACE_HPI == 1, 1, 0))

# educ_no_hs_credential_0
data$educ_no_hs_credential_0 <- ifelse((!data$DEGREE_RECEIVED_GED %in% c(1, 2) & data$DEGREE_RECEIVED_HS == 2) |
                                         (!data$DEGREE_RECEIVED_HS %in% c(1, 2) & data$DEGREE_RECEIVED_GED == 2) |
                                         (!data$DEGREE_RECEIVED_HS %in% c(1, 2) & !data$DEGREE_RECEIVED_GED %in% c(1, 2)), NA,
                                       ifelse(data$DEGREE_RECEIVED_GED == 2 & data$DEGREE_RECEIVED_HS == 2, 1, 0))

# educ_ged_or_alternative_credential_0
data$educ_ged_or_alternative_credential_0 <- ifelse(!data$DEGREE_RECEIVED_GED %in% c(1, 2), NA,
                                                    ifelse(data$DEGREE_RECEIVED_GED == 1 
                                                           & data$HIGHEST_EDUCATION > 5 
                                                           & data$HIGHEST_EDUCATION < 13, 1, 0))

# educ_regular_high_school_diploma_0
data$educ_regular_high_school_diploma_0 <- ifelse(!data$DEGREE_RECEIVED_HS %in% c(1,2), NA,
                                                  ifelse(data$DEGREE_RECEIVED_HS == 1
                                                         & data$HIGHEST_EDUCATION > 5 
                                                         & data$HIGHEST_EDUCATION < 13, 1, 0))

# educ_some_college_0
data$educ_some_college_0 <- ifelse(data$HIGHEST_EDUCATION < 6 | data$HIGHEST_EDUCATION > 17, NA, 
                                   ifelse(data$HIGHEST_EDUCATION %in% c(13, 14, 15), 1, 0))

# educ_bachelors_degree_or_higher_0
data$educ_bachelors_degree_or_higher_0 <- ifelse(data$HIGHEST_EDUCATION < 6 | data$HIGHEST_EDUCATION > 17, NA,
                                                    ifelse(data$HIGHEST_EDUCATION %in% c(16, 17), 1, 0))

# occupational_license_certification_0
data$occupational_license_certification_0 <- ifelse(!data$DEGREE_RECEIVED_CERTIFICATE %in% c(1, 2), NA,
                                                    ifelse(data$DEGREE_RECEIVED_CERTIFICATE == 1, 1, 0))

# sex_male_0
data$sex_male_0 <- ifelse(!data$GENDER %in% c(1, 2), NA, ifelse(data$GENDER == 1, 1, 0))

# marstat_married_0
# marstat_widowed_0
# marstat_divorced_or_separated_0
# marstat_never_married_0
data$marstat_married_0 <- ifelse(!data$MARITAL %in% c(1, 2, 3, 4), NA, ifelse(data$MARITAL == 1, 1, 0))
data$marstat_widowed_0 <- ifelse(!data$MARITAL %in% c(1, 2, 3, 4), NA, ifelse(data$MARITAL == 3, 1, 0))
data$marstat_divorced_or_separated_0 <- ifelse(!data$MARITAL %in% c(1, 2, 3, 4), NA, ifelse(data$MARITAL == 2, 1, 0))
data$marstat_never_married_0 <- ifelse(!data$MARITAL %in% c(1, 2, 3, 4), NA, ifelse(data$MARITAL == 4, 1, 0))

# number_of_children_dependent_0
data$number_of_children_dependent_0 <- ifelse(!data$DEPENDENT_CHILDREN %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), NA, data$DEPENDENT_CHILDREN)

# any_children_0
data$any_children_0 <- ifelse(is.na(data$number_of_children_dependent_0) | data$number_of_children_dependent_0 == 0, 1, 0)
  
# birth_country_usa_0
data$birth_country_usa_0 <- ifelse(!data$CITIZENSHIP %in% c(0, 1, 2, 3, 4, 5), NA, 
                                      ifelse(data$CITIZENSHIP %in% c(1, 2), 1, 0))

# limited_english_0
data$limited_english_0 <- ifelse(!data$LIMITED_ENGLISH %in% c(1, 2), NA,
                                 ifelse(data$LIMITED_ENGLISH == 1, 1, 0))

# currently_working_0
data$currently_working_0 <- ifelse(!data$CURRENT_EMPLOYMENT_ENROLL %in% c(1, 2), NA,
                                   ifelse(data$CURRENT_EMPLOYMENT_ENROLL == 1, 1, 0))

# worked_before_0
data$worked_before_0 <- ifelse(!data$PAID_WORK %in% c(1, 2), NA,
                               ifelse(data$PAID_WORK == 1, 1, 0))

# never_worked_0
data$never_worked_0 <- ifelse(!data$PAID_WORK %in% c(1, 2), NA,
                              ifelse(data$PAID_WORK == 2, 1, 0))

# tanf_assistance_0
data$tanf_assistance_0 <- ifelse(!data$IB_TANF %in% c(1, 2), NA,
                                 ifelse(data$IB_TANF == 1, 1, 0))

# wic_or_snap_assistance_0
data$wic_or_snap_assistance_0 <- ifelse((!data$IB_WIC %in% c(1, 2) & data$IB_SNAP == 2) |
                                        (!data$IB_SNAP %in% c(1, 2) & data$IB_WIC == 2) |
                                        (!data$IB_SNAP %in% c(1, 2) & !data$IB_WIC %in% c(1, 2)), NA, 
                                        ifelse(data$IB_WIC == 1 | data$IB_TANF == 1, 1, 0))

# future_school_part_time
data$future_school_part_time <- ifelse(!data$EXPECTED_SCHOOL_ENROLL %in% c(1, 2), NA,
                                       ifelse(data$EXPECTED_SCHOOL_ENROLL == 2, 1, 0))

# future_school_full_time
data$future_school_full_time <- ifelse(!data$EXPECTED_SCHOOL_ENROLL %in% c(1, 2), NA,
                                       ifelse(data$EXPECTED_SCHOOL_ENROLL == 1, 1, 0))

# future_work
data$future_work <- ifelse(!data$EXPECTED_WORK %in% c(1, 2), NA,
                           ifelse(data$EXPECTED_WORK == 1, 1, 0))

# work_hours
data$work_hours <- ifelse(!data$EXPECTED_WORK %in% c(1, 2), NA,
                          ifelse(data$EXPECTED_WORK == 2, 0,
                                 ifelse(data$EXPECTED_HOURS < 0 | data$EXPECTED_HOURS > 70, NA, 
                                        data$EXPECTED_HOURS)))

# career_knowledge_3_variables
data$GOALS_ORGANIZATION_TYPE[!data$GOALS_ORGANIZATION_TYPE %in% c(1, 2, 3, 4)] <- NA
data$GOALS_5YEAR_PLAN[!data$GOALS_5YEAR_PLAN %in% c(1, 2, 3, 4)] <- NA
data$GOALS_FUTURE_OCCUPATION[!data$GOALS_FUTURE_OCCUPATION %in% c(1, 2, 3, 4)] <- NA
data$career_knowledge_3_variables <- (data$GOALS_5YEAR_PLAN + data$GOALS_ORGANIZATION_TYPE + data$GOALS_FUTURE_OCCUPATION) / 3

# life_challenges_index_4_variables
data$OFTEN_TRANSPORTATION[!data$OFTEN_TRANSPORTATION %in% c(1, 2, 3, 4, 5)] <- NA
data$OFTEN_ALCOHOL_DRUG[!data$OFTEN_ALCOHOL_DRUG %in% c(1, 2, 3, 4)] <- NA
data$OFTEN_CHILD_CARE[!data$OFTEN_CHILD_CARE %in% c(1, 2, 3, 4, 5)] <- NA
data$OFTEN_ILLNESS[!data$OFTEN_ILLNESS %in% c(1, 2, 3, 4, 5)] <- NA
data$life_challenges_index_4_variables <- (data$OFTEN_TRANSPORTATION + data$OFTEN_ALCOHOL_DRUG + data$OFTEN_CHILD_CARE + data$OFTEN_ILLNESS) / 4

data <- data %>% rename(number_of_services = COV_SVCS) # number_of_services
data <- data %>% rename(behavioral_incentives = COV_BEHAVIORAL_INCENTIVES) # behavioral_incentives
data <- data %>% rename(avg_fte_caseload = COV_AVG_FTE_CASELOAD) # avg_fte_caseload 
data <- data %>% rename(childcare_transport = COV_CHILDCARE_TRANSPORT) # childcare_transport
data <- data %>% rename(social_services = COV_SVCS_SOCIAL) # social_services
data <- data %>% rename(financial_services = COV_TUITION_FIN_SVCS) # financial_services
data <- data %>% rename(number_of_employment_supports = COV_EMPLOYMENT_SUPPORTS) # number_of_employment_supports
data <- data %>% rename(number_of_colocated_services = COV_LOCATION_OF_SERVICES) # number_of_colocated_servicesc
data <- data %>% rename(emergency_assistance = COV_EMERGENCY_ASSISTANCE) # emergency_assistance
data <- data %>% rename(number_of_cp_principles = COV_CP_PRINCIPLES) # number_of_cp_principles
data <- data %>% rename(peer_support = COV_PEER_SUPPORT) # peer_support
data <- data %>% rename(proportion_local_some_college = LC_EDATTAIN) # proportion_local_some_college
data <- data %>% rename(proportion_local_jobs_health_care = LC_EMPPCT) # proportion_local_jobs_health_care
data <- data %>% rename(median_wage_local_health_care = LC_MEDWAGE) # median_wage_local_health_care
data <- data %>% rename(proportion_local_cash_assistance = LC_PUBASS) # proportion_local_cash_assistance
data <- data %>% rename(proportion_local_enrolled_school = LC_SCHEN) # proportion_local_enrolled_school
data <- data %>% rename(total_msa_population = LC_TOTPOP) # total_msa_population
data <- data %>% rename(percent_local_unemployed = LC_UERATE) # percent_local_unemployed

# write_tsv(output, "educ_XX_HPOG_ONLY_variables[Ver.2].tsv")

# ----------------- Extract Required Columns (& some more for testing/logging) ----------------- 
hpog_df <- data %>% 
  select(participant_id, site_id, study_hpog,
         surveyrespondent_15_hpog, surveyrespondent_36_hpog, surveyrespondent_72_hpog, 
         credential_15, credential_36, credential_72, currently_working_15, 
         currently_working_36, currently_working_72, hours_worked_per_week_15, hours_worked_per_week_36, hours_worked_per_week_alt_36,
         hours_worked_per_week_72, hourly_wage_rate_15, hourly_wage_rate_36, hourly_wage_rate_alt_36, hourly_wage_rate_72,
         weekly_earnings_15, weekly_earnings_36, weekly_earnings_alt_36, weekly_earnings_72, treatment_group_0, 
         age_in_years_0, DATE_RANDOM, DOB_MONTH, DOB_YEAR,
         age_LT21_0, age_21To24_0, age_25To34_0, age_GE35_0, ethnicity_hispanic_0, 
         race_white_0, race_black_0, race_asian_0, race_american_indian_0, race_pacific_islander_0, RACE_W, RACE_B, RACE_A, RACE_NA, RACE_HPI,
         educ_no_hs_credential_0, DEGREE_RECEIVED_GED, DEGREE_RECEIVED_HS, educ_ged_or_alternative_credential_0, educ_regular_high_school_diploma_0, 
         educ_some_college_0, educ_bachelors_degree_or_higher_0, HIGHEST_EDUCATION,
         occupational_license_certification_0,
         sex_male_0, MARITAL, marstat_married_0, marstat_widowed_0, marstat_divorced_or_separated_0, marstat_never_married_0, 
         number_of_children_dependent_0, any_children_0,
         birth_country_usa_0, limited_english_0, currently_working_0, worked_before_0, never_worked_0, tanf_assistance_0, wic_or_snap_assistance_0,
         future_school_part_time, future_school_full_time, future_work, work_hours, career_knowledge_3_variables, life_challenges_index_4_variables,
         number_of_services, behavioral_incentives, avg_fte_caseload, childcare_transport, social_services, financial_services, 
         number_of_employment_supports, number_of_colocated_services, emergency_assistance, peer_support, number_of_cp_principles, 
         proportion_local_some_college, proportion_local_jobs_health_care, median_wage_local_health_care, proportion_local_cash_assistance,
         proportion_local_enrolled_school, total_msa_population, percent_local_unemployed)

# Fetch summary for Code Book
# summary_stats <- sapply(hpog_df, calculate_stats)
# print(summary_stats)

# ----------------- Write extracted info. to CSV ----------------- 
folder_path <- file.path(root, "read-HPOG1.0-output")

if(!dir.exists(folder_path)) { # Create the folder if it doesn't exist
  dir.create(folder_path)
}

write.csv(hpog_df, file.path(folder_path,"HPOG-1.0-DataFrame.csv"), row.names = FALSE)

# Remove all data frames
rm(list = ls())
gc()