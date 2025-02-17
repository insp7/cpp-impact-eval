# -- Calculate statistics for all three studies

library(tidyverse)
source("utils.R")

xv_AF1 <- read.csv("AF1.csv") # Pre valence coding data frame
all_hpog_imp_set <- read.csv("all_hpog_imp_set.csv")
pace_only_imp_set <- read.csv("pace_only_imp_set.csv")
year_up_imp_set <- read.csv("year_up_imp_set.csv")
hpog_df <- read.csv("hpog_df.csv")
pace_df <- read.csv("pace_df.csv")
year_up <- read.csv("year_up.csv")

# HPOG Calculation:

# Filter out columns with .imputed sufix
hpog <- hpog_df[, !grepl("\\.imputed$", names(hpog_df))]

hpog <- as.data.frame(hpog)
hpog <- hpog %>% 
  select(weekly_earnings_15, weekly_earnings_36, weekly_earnings_72,
         credential_15, credential_36, credential_72, 
         age_in_years_0, ethnicity_hispanic_0, 
         race_black_0, race_asian_0, race_american_indian_0, race_pacific_islander_0,
         occupational_license_certification_0, sex_male_0, 
         marstat_married_0, marstat_widowed_0, marstat_divorced_or_separated_0,
         number_of_children_dependent_0, birth_country_usa_0, limited_english_0,
         tanf_assistance_0, wic_or_snap_assistance_0,
         future_school_part_time, future_school_full_time, future_work, work_hours,
         career_knowledge_3_variables, life_challenges_index_4_variables)

# Non Valence coded HPOG variables
non_vc_hpog_cols <- c("age_LT21_0", "age_21To24_0", "age_25To34_0", "age_GE35_0", "race_white_0",
                      "educ_some_college_0", "educ_regular_high_school_diploma_0", 
                      "educ_ged_or_alternative_credential_0", "educ_no_hs_credential_0",
                      "educ_bachelors_degree_or_higher_0", "marstat_never_married_0",
                      "worked_before_0", "never_worked_0", "currently_working_0")

# Probably unnecessary but still adding for now as a safeguard. Similar for other 2 studies
xv_AF1_hpog_only <- xv_AF1 %>% 
  filter(study_type == 1)

# Copy these non_vc variables
hpog[non_vc_hpog_cols] <- xv_AF1_hpog_only[non_vc_hpog_cols]

# Rearrange
hpog <- hpog %>% 
  select(weekly_earnings_15, weekly_earnings_36, weekly_earnings_72,
         credential_15, credential_36, credential_72, 
         age_in_years_0, age_LT21_0, age_21To24_0, age_25To34_0, age_GE35_0,
         ethnicity_hispanic_0, 
         race_white_0, race_black_0, race_asian_0, race_american_indian_0, race_pacific_islander_0,
         educ_no_hs_credential_0, educ_ged_or_alternative_credential_0, educ_regular_high_school_diploma_0,
         educ_some_college_0, educ_bachelors_degree_or_higher_0,
         occupational_license_certification_0, sex_male_0, 
         marstat_married_0, marstat_widowed_0, marstat_divorced_or_separated_0, marstat_never_married_0,
         number_of_children_dependent_0, birth_country_usa_0, limited_english_0, 
         currently_working_0, worked_before_0, never_worked_0,
         tanf_assistance_0, wic_or_snap_assistance_0,
         future_school_part_time, future_school_full_time, future_work, work_hours,
         career_knowledge_3_variables, life_challenges_index_4_variables)

stats_hpog <- data.frame(
  "Variable" = names(hpog),
  "Mean" = sapply(seq_along(hpog), function(i) custom_mean(hpog[[i]], colnames(hpog)[i], 1)),
  "Standard Deviation" = sapply(hpog, function(x) round(sd(x, na.rm = T), 4)),
  "Total Non-missing" = colSums(!is.na(hpog))
)

write.xlsx(stats_hpog, "HPOG_STATS[Rev.3].xlsx")

# YEAR UP Calculation:

# Filter out columns with .imputed sufix
yu <- year_up[, !grepl("\\.imputed$", names(year_up))]

yu <- yu %>% 
  select(weekly_earnings_15, weekly_earnings_36, weekly_earnings_72,
         age_in_years_0, ethnicity_hispanic_0, 
         race_black_0, race_asian_0, race_american_indian_0, race_pacific_islander_0,
         sex_male_0, 
         marstat_married_0, marstat_widowed_0, marstat_divorced_or_separated_0,
         number_of_children_home_0, birth_country_usa_0,
         tanf_assistance_0, wic_or_snap_assistance_0,
         future_school_part_time, future_school_full_time, future_work, work_hours,
         career_knowledge_3_variables, life_challenges_index_4_variables)

# Non Valence coded Year-up variables
non_vc_yu_cols <- c("age_LT21_0", "age_21To24_0", "age_25To34_0", "age_GE35_0", "race_white_0",
                    "educ_some_college_0", "educ_regular_high_school_diploma_0", 
                    "educ_ged_or_alternative_credential_0", "educ_no_hs_credential_0",
                    "educ_bachelors_degree_or_higher_0", "marstat_never_married_0",
                    "worked_before_0", "never_worked_0", "currently_working_0")

xv_AF1_yu <- xv_AF1 %>% 
  filter(study_type == 2)

# Copy these non_vc variables
yu[non_vc_yu_cols] <- xv_AF1_yu[non_vc_yu_cols]

# Rearrange
yu <- yu %>% 
  select(weekly_earnings_15, weekly_earnings_36, weekly_earnings_72,
         age_in_years_0, age_LT21_0, age_21To24_0, age_25To34_0, age_GE35_0,
         ethnicity_hispanic_0, 
         race_white_0, race_black_0, race_asian_0, race_american_indian_0, race_pacific_islander_0,
         educ_no_hs_credential_0, educ_ged_or_alternative_credential_0, educ_regular_high_school_diploma_0,
         educ_some_college_0, educ_bachelors_degree_or_higher_0,
         sex_male_0, 
         marstat_married_0, marstat_widowed_0, marstat_divorced_or_separated_0, marstat_never_married_0,
         number_of_children_home_0, birth_country_usa_0,
         currently_working_0, worked_before_0, never_worked_0,
         tanf_assistance_0, wic_or_snap_assistance_0,
         future_school_part_time, future_school_full_time, future_work, work_hours,
         career_knowledge_3_variables, life_challenges_index_4_variables)

stats_yu <- data.frame(
  "Variable" = names(yu),
  "Mean" = sapply(seq_along(yu), function(i) custom_mean(yu[[i]], colnames(yu)[i], 2)),
  "Standard Deviation" = sapply(yu, function(x) round(sd(x, na.rm = T), 4)),
  "Total Non-missing" = colSums(!is.na(yu))
)

write.xlsx(stats_yu, "YU_STATS[Rev.3].xlsx")

# PACE Calculation:

# Filter out columns with .imputed sufix
pace <- pace_df[, !grepl("\\.imputed$", names(pace_df))]

pace <- pace %>% 
  select(weekly_earnings_15, weekly_earnings_36, weekly_earnings_72,
         age_in_years_0, ethnicity_hispanic_0, 
         race_black_0, race_asian_0, race_american_indian_0, race_pacific_islander_0,
         sex_male_0, 
         marstat_married_0, marstat_widowed_0, marstat_divorced_or_separated_0,
         number_of_children_home_0, birth_country_usa_0,
         tanf_assistance_0, wic_or_snap_assistance_0,
         future_school_part_time, future_school_full_time, future_work, work_hours,
         career_knowledge_3_variables, life_challenges_index_4_variables)

# Non Valence coded PACE variables
non_vc_pace_cols <- c("age_LT21_0", "age_21To24_0", "age_25To34_0", "age_GE35_0", "race_white_0",
                    "educ_some_college_0", "educ_regular_high_school_diploma_0", 
                    "educ_ged_or_alternative_credential_0", "educ_no_hs_credential_0",
                    "educ_bachelors_degree_or_higher_0", "marstat_never_married_0",
                    "worked_before_0", "never_worked_0", "currently_working_0")

xv_AF1_pace <- xv_AF1 %>% 
  filter(study_type == 3)

# Copy these non_vc variables
pace[non_vc_pace_cols] <- xv_AF1_pace[non_vc_pace_cols]

# Rearrange
pace <- pace %>% 
  select(weekly_earnings_15, weekly_earnings_36, weekly_earnings_72,
         age_in_years_0, age_LT21_0, age_21To24_0, age_25To34_0, age_GE35_0,
         ethnicity_hispanic_0, 
         race_white_0, race_black_0, race_asian_0, race_american_indian_0, race_pacific_islander_0,
         educ_no_hs_credential_0, educ_ged_or_alternative_credential_0, educ_regular_high_school_diploma_0,
         educ_some_college_0, educ_bachelors_degree_or_higher_0,
         sex_male_0, 
         marstat_married_0, marstat_widowed_0, marstat_divorced_or_separated_0, marstat_never_married_0,
         number_of_children_home_0, birth_country_usa_0,
         currently_working_0, worked_before_0, never_worked_0,
         tanf_assistance_0, wic_or_snap_assistance_0,
         future_school_part_time, future_school_full_time, future_work, work_hours,
         career_knowledge_3_variables, life_challenges_index_4_variables)

stats_pace <- data.frame(
  "Variable" = names(pace),
  "Mean" = sapply(seq_along(pace), function(i) custom_mean(pace[[i]], colnames(pace)[i], 3)),
  "Standard Deviation" = sapply(pace, function(x) round(sd(x, na.rm = T), 4)),
  "Total Non-missing" = colSums(!is.na(pace))
)

write.xlsx(stats_pace, "PACE_STATS[Rev.3].xlsx")