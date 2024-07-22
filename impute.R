library(readr)
library(rprojroot)
library(tidyverse)
library(mice)

# Find the root of the current project
root <- find_root(is_rstudio_project)

source("utils.R")

all_hpog <- read.csv("ALL-HPOG-1.0-DataFrame.csv")
pace_only <- read.csv("PACE-ONLY-DataFrame.csv")
year_up <- read.csv("YEAR-UP-DataFrame.csv")

# Discarding variables that I think are unnecessary for the imputation. (Will be fixed after discussing with JAK)
all_hpog_imp_set <- all_hpog %>% 
  select(-study_hpog, -surveyrespondent_15_hpog, -surveyrespondent_36_hpog, -surveyrespondent_72_hpog,
         -age_LT21_0, -age_21To24_0, -age_25To34_0, -age_GE35_0)

pace_only_imp_set <- pace_only %>% 
  select(-study_pace, -study_year_up, -surveyrespondent_15_pace, -surveyrespondent_36_pace, -surveyrespondent_72_pace)

year_up_imp_set <- year_up %>% 
  select(-study_pace, -study_year_up, -surveyrespondent_15_pace, -surveyrespondent_36_pace, -surveyrespondent_72_pace)

# outputs error:
# iter imp variable
# 1   1  credential_15Error in solve.default(xtx + diag(pen)) : 
#   system is computationally singular: reciprocal condition number = 1.55865e-18
# imp <- mice(all_hpog_imp_set, method = 'pmm', m = 1, maxit = 40, seed = 123)

# Imputation for Pace Only WORKS! (TODO: Need to find optimal number of iterations to match closely with the mean. Decide with JAK)
imp <- mice(pace_only_imp_set, method = 'pmm', m = 1, maxit = 40, seed = 123)
imputed_pace_only_df <- complete(imp)
sapply(pace_only, calculate_stats)
sapply(imputed_pace_only_df, calculate_stats)

# Imputation for Year Up WORKS! (TODO: Need to find optimal number of iterations to match closely with the mean. Decide with JAK)
year_up_imp <- mice(year_up_imp_set, method = 'pmm', m = 1, maxit = 40, seed = 123)
imputed_year_up_df <- complete(year_up_imp)
sapply(year_up, calculate_stats)
sapply(imputed_year_up_df, calculate_stats)
