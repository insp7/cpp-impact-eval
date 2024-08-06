library(readr)
library(rprojroot)
library(tidyverse)
library(mice)

# Find the root of the current project
root <- find_root(is_rstudio_project)

source("utils.R")

all_hpog_df <- read.csv("AF2--ALL-HPOG.csv")
pace_only_df <- read.csv("AF2--PACE-ONLY-DataFrame.csv")
year_up_df <- read.csv("AF2--YEAR-UP-DataFrame.csv")

# Discarding variables that I think are unnecessary for the imputation. (Will be confirmed after discussing with JAK)
all_hpog_imp_set <- all_hpog_df %>% 
  select(-surveyrespondent_15_hpog, -surveyrespondent_36_hpog, -surveyrespondent_72_hpog,
         -age_LT21_0_vc, -age_21To24_0_vc, -age_GE35_0_vc, -number_of_services, -behavioral_incentives, -avg_fte_caseload, -childcare_transport, -social_services, -financial_services, 
         -number_of_employment_supports, -number_of_colocated_services, -emergency_assistance, -peer_support, -number_of_cp_principles, 
         -proportion_local_some_college, -proportion_local_jobs_health_care, -median_wage_local_health_care, -proportion_local_cash_assistance,
         -proportion_local_enrolled_school, -total_msa_population, -percent_local_unemployed)

pace_only_imp_set <- pace_only_df %>% 
  select(-study_year_up, -surveyrespondent_15_pace, -surveyrespondent_36_pace, -surveyrespondent_72_pace)

year_up_imp_set <- year_up_df %>% 
  select(-study_year_up, -surveyrespondent_15_pace, -surveyrespondent_36_pace, -surveyrespondent_72_pace)

# Imputation for Pace Only WORKS! (TODO: Need to find optimal number of iterations to match closely with the mean. Decide with JAK)
imp <- mice(pace_only_imp_set, method = 'pmm', m = 1, maxit = 40, seed = 123)
imputed_pace_only_df <- complete(imp)
sapply(pace_only_df, calculate_stats)
sapply(imputed_pace_only_df, calculate_stats)

# Imputation for Year Up WORKS! (TODO: Need to find optimal number of iterations to match closely with the mean. Decide with JAK)
year_up_imp <- mice(year_up_imp_set, method = 'pmm', m = 1, maxit = 40, seed = 123)
imputed_year_up_df <- complete(year_up_imp)
sapply(year_up_imp_set, calculate_stats)
sapply(imputed_year_up_df, calculate_stats)

# HPOG Imputation: moment of truth
all_hpog_imp <- mice(all_hpog_imp_set, method = 'pmm', m = 1, maxit = 40, seed = 123)
imputed_hpog_df <- complete(all_hpog_imp)
sapply(all_hpog_imp_set, calculate_stats)
sapply(imputed_hpog_df, calculate_stats)
