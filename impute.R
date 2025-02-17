library(readr)
library(rprojroot)
library(tidyverse)
library(mice)
library(dplyr)
library(openxlsx)
# Find the root of the current project
root <- find_root(is_rstudio_project)

source("utils.R")

all_hpog_df <- read.csv("AF2--ALL-HPOG.csv")
pace_only_df <- read.csv("AF2--PACE-ONLY-DataFrame.csv")
year_up_df <- read.csv("AF2--YEAR-UP-DataFrame.csv")

# Discarding variables that I think are unnecessary for the imputation. (Will be confirmed after discussing with JAK)
all_hpog_imp_set <- all_hpog_df %>% 
  dplyr::select(-surveyrespondent_15_hpog, -surveyrespondent_36_hpog, -surveyrespondent_72_hpog,
         -number_of_services, -behavioral_incentives, -avg_fte_caseload, -childcare_transport, -social_services, -financial_services, 
         -number_of_employment_supports, -number_of_colocated_services, -emergency_assistance, -peer_support, -number_of_cp_principles, 
         -proportion_local_some_college, -proportion_local_jobs_health_care, -median_wage_local_health_care, -proportion_local_cash_assistance,
         -proportion_local_enrolled_school, -total_msa_population, -percent_local_unemployed, -future_school_full_time)

pace_only_imp_set <- pace_only_df %>% 
  dplyr::select(-surveyrespondent_15_pace, -surveyrespondent_36_pace, -surveyrespondent_72_pace, -future_school_full_time)

year_up_imp_set <- year_up_df %>% 
  dplyr::select(-surveyrespondent_15_pace, -surveyrespondent_36_pace, -surveyrespondent_72_pace, -future_school_full_time)

# HPOG Imputation
all_hpog_imp <- mice(all_hpog_imp_set, method = 'pmm', m = 1, maxit = 50, seed = 7)
imputed_hpog_df <- complete(all_hpog_imp)
# Since future_school_full_time & future_school_part_time are mutually exclusive and exhaustive,
# Coding future_school_full_time as the binary opposite of future_school_part_time when its NA
imputed_hpog_df$future_school_full_time <- ifelse(imputed_hpog_df$future_school_part_time == 1, 0, 1)
# Add previously removed variables back to HPOG dataframe
imputed_hpog_df$number_of_services <- all_hpog_df$number_of_services
imputed_hpog_df$behavioral_incentives <- all_hpog_df$behavioral_incentives
imputed_hpog_df$avg_fte_caseload <- all_hpog_df$avg_fte_caseload
imputed_hpog_df$childcare_transport <- all_hpog_df$childcare_transport
imputed_hpog_df$social_services <- all_hpog_df$social_services
imputed_hpog_df$financial_services <- all_hpog_df$financial_services
imputed_hpog_df$number_of_employment_supports <- all_hpog_df$number_of_employment_supports
imputed_hpog_df$number_of_colocated_services <- all_hpog_df$number_of_colocated_services
imputed_hpog_df$emergency_assistance <- all_hpog_df$emergency_assistance
imputed_hpog_df$peer_support <- all_hpog_df$peer_support
imputed_hpog_df$number_of_cp_principles <- all_hpog_df$number_of_cp_principles
imputed_hpog_df$proportion_local_some_college <- all_hpog_df$proportion_local_some_college
imputed_hpog_df$proportion_local_jobs_health_care <- all_hpog_df$proportion_local_jobs_health_care
imputed_hpog_df$median_wage_local_health_care <- all_hpog_df$median_wage_local_health_care
imputed_hpog_df$proportion_local_cash_assistance <- all_hpog_df$proportion_local_cash_assistance
imputed_hpog_df$proportion_local_enrolled_school <- all_hpog_df$proportion_local_enrolled_school
imputed_hpog_df$total_msa_population <- all_hpog_df$total_msa_population
imputed_hpog_df$percent_local_unemployed <- all_hpog_df$percent_local_unemployed
imputed_hpog_df$future_school_full_time <- all_hpog_df$future_school_full_time
# Create impute flag
hpog_df <- imputed_hpog_df
for(col in names(all_hpog_imp_set)) {
  imputed_flag <- ifelse(is.na(all_hpog_imp_set[[col]]), T, F)
  hpog_df[[paste0(col, ".imputed")]] <- imputed_flag
}

# PACE Imputation
imp <- mice(pace_only_imp_set, method = 'pmm', m = 1, maxit = 50, seed = 7)
imputed_pace_only_df <- complete(imp)
imputed_pace_only_df$future_school_full_time <- ifelse(imputed_pace_only_df$future_school_part_time == 1, 0, 1)
# Create impute flag
pace_df <- imputed_pace_only_df
for(col in names(pace_only_imp_set)) {
  imputed_flag <- ifelse(is.na(pace_only_imp_set[[col]]), T, F)
  pace_df[[paste0(col, ".imputed")]] <- imputed_flag
}

# YEARUP Imputation
year_up_imp <- mice(year_up_imp_set, method = 'pmm', m = 1, maxit = 50, seed = 7)
imputed_year_up_df <- complete(year_up_imp)
# This variable equals 0 for all Year Up cases without missing values. Only 22 cases are NA. 
# Filling them with 0 as per suggested by RO
imputed_year_up_df$reading_english_proficiency_not_at_all_0_vc <- 0 
# Since future_school_full_time & future_school_part_time are mutually exclusive and exhaustive,
# Coding future_school_full_time as the binary opposite of future_school_part_time when its NA
imputed_year_up_df$future_school_full_time <- ifelse(imputed_year_up_df$future_school_part_time == 1, 0, 1)
# Create impute flag
year_up <- imputed_year_up_df
for(col in names(year_up_imp_set)) {
  imputed_flag <- ifelse(is.na(year_up_imp_set[[col]]), T, F)
  year_up[[paste0(col, ".imputed")]] <- imputed_flag
}

# xv_AF3 <- bind_rows(imputed_hpog_df, imputed_pace_only_df, imputed_year_up_df)
xv_AF3 <- bind_rows(hpog_df, pace_df, year_up)

# Re-introduce the individual study variables
xv_AF3$study_hpog <- ifelse(xv_AF3$study_type == 1, 1, 0)
xv_AF3$study_pace <- ifelse(xv_AF3$study_type == 3, 1, 0)
xv_AF3$study_year_up <- ifelse(xv_AF3$study_type == 2, 1, 0)

write.csv(all_hpog_imp_set, "all_hpog_imp_set.csv", row.names = F)
write.csv(pace_only_imp_set, "pace_only_imp_set.csv", row.names = F)
write.csv(year_up_imp_set, "year_up_imp_set.csv", row.names = F)
write.csv(hpog_df, "hpog_df.csv", row.names = F)
write.csv(pace_df, "pace_df.csv", row.names = F)
write.csv(year_up, "year_up.csv", row.names = F)
write.csv(xv_AF3, "xv_AF3.2.csv", row.names = F)

xv_AF3 <- read.csv("xv_AF3.2.csv") # 3.2 = all studies merged so no 3 .imputed variables