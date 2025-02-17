# create 3 subsets of AF2 for imputation

df <- read.csv("AF2.csv")

# Extract 3 studies
all_hpog <- df %>% filter(study_type == 1)
year_up <- df %>% filter(study_type == 2)
pace_only <- df %>% filter(study_type == 3)

# Not Including these variables for HPOG: 
# -surveyrespondent_15_hpog, -surveyrespondent_36_hpog, -surveyrespondent_72_hpog,
# -number_of_services, -behavioral_incentives, -avg_fte_caseload, -childcare_transport, -social_services, -financial_services, 
# -number_of_employment_supports, -number_of_colocated_services, -emergency_assistance, -peer_support, -number_of_cp_principles, 
# -proportion_local_some_college, -proportion_local_jobs_health_care, -median_wage_local_health_care, -proportion_local_cash_assistance,
# -proportion_local_enrolled_school, -total_msa_population, -percent_local_unemployed

all_hpog_df <- all_hpog %>% 
  select(participant_id, site_id, 
         credential_15, credential_36, credential_72, 
         currently_working_15,currently_working_36, currently_working_72, 
         hours_worked_per_week_15, hours_worked_per_week_36, hours_worked_per_week_alt_36, hours_worked_per_week_72,
         hourly_wage_rate_15, hourly_wage_rate_36, hourly_wage_rate_alt_36, hourly_wage_rate_72,
         weekly_earnings_15, weekly_earnings_36, weekly_earnings_alt_36, weekly_earnings_72, 
         treatment_group_0, age_LT21_0_vc, age_21To24_0_vc, age_GE35_0_vc, 
         ethnicity_hispanic_0, race_black_0, race_asian_0, race_american_indian_0, race_pacific_islander_0,
         educ_no_hs_credential_0_vc, educ_ged_or_alternative_credential_0_vc, educ_regular_high_school_diploma_0_vc, 
         educ_bachelors_degree_or_higher_0_vc, occupational_license_certification_0,
         sex_male_0, 
         marstat_married_0, marstat_widowed_0, marstat_divorced_or_separated_0,
         number_of_children_dependent_0, any_children_0,
         birth_country_usa_0, limited_english_0, worked_before_0_vc, never_worked_0_vc, tanf_assistance_0, wic_or_snap_assistance_0,
         future_school_part_time, future_school_full_time, future_work, work_hours, career_knowledge_3_variables, life_challenges_index_4_variables)


# Not including these variables for PACE:
# -study_year_up, -surveyrespondent_15_pace, -surveyrespondent_36_pace, -surveyrespondent_72_pace

pace_only_df <- pace_only %>% 
  select(participant_id, site_id, 
         currently_working_15, currently_working_36, currently_working_72,
         hours_worked_per_week_15, hours_worked_per_week_36, hours_worked_per_week_alt_36, hours_worked_per_week_72,
         hourly_wage_rate_15, hourly_wage_rate_36, hourly_wage_rate_alt_36, hourly_wage_rate_72,
         weekly_earnings_15, weekly_earnings_36, weekly_earnings_alt_36, weekly_earnings_72, treatment_group_0, 
         age_LT21_0_vc, age_21To24_0_vc, age_GE35_0_vc,
         ethnicity_hispanic_0, race_black_0, race_asian_0, race_american_indian_0, race_pacific_islander_0,
         No_HS_in_US_0, 
         grades_mostly_A_0_vc, grades_mostly_C_0_vc, grades_mostly_D_0_vc, grades_mostly_F_0_vc, 
         educ_no_hs_credential_0_vc, educ_ged_or_alternative_credential_0_vc, educ_regular_high_school_diploma_0_vc,
         educ_bachelors_degree_or_higher_0_vc, postsec_voc_tech_certificate_0, 
         sex_male_0, 
         marstat_married_0, marstat_widowed_0, marstat_divorced_or_separated_0, 
         number_of_children_home_0, any_children_0, birth_country_usa_0, speak_only_english_at_home_0, 
         spoken_english_proficiency_well_0_vc, spoken_english_proficiency_not_well_0_vc, spoken_english_proficiency_not_at_all_0_vc,  
         reading_english_proficiency_well_0_vc, reading_english_proficiency_not_well_0_vc, reading_english_proficiency_not_at_all_0_vc, 
         worked_before_0_vc, never_worked_0_vc,
         tanf_assistance_0, wic_or_snap_assistance_0, future_school_part_time, future_school_full_time, future_work, work_hours,
         financial_support_not_difficult_at_all_vc, financial_support_very_difficult_vc, career_knowledge_3_variables,
         career_knowledge_7_variables, training_commitment_index, academic_discipline_index, emotional_stability_index, social_support_index,
         life_challenges_index_4_variables, life_challenges_index_6_variables, stress_index, weight_15, weight_36, weight_72)


# Not including these variables for YearUp:
# -study_year_up, -surveyrespondent_15_pace, -surveyrespondent_36_pace, -surveyrespondent_72_pace

year_up_df <- year_up %>% 
  select(participant_id, site_id, 
         currently_working_15, currently_working_36, currently_working_72,
         hours_worked_per_week_15, hours_worked_per_week_36, hours_worked_per_week_alt_36, hours_worked_per_week_72,
         hourly_wage_rate_15, hourly_wage_rate_36, hourly_wage_rate_alt_36, hourly_wage_rate_72,
         weekly_earnings_15, weekly_earnings_36, weekly_earnings_alt_36, weekly_earnings_72, treatment_group_0, 
         age_LT21_0_vc, age_21To24_0_vc, age_GE35_0_vc,
         ethnicity_hispanic_0, race_black_0, race_asian_0, race_american_indian_0, race_pacific_islander_0,
         No_HS_in_US_0, 
         grades_mostly_A_0_vc, grades_mostly_C_0_vc, grades_mostly_D_0_vc, grades_mostly_F_0_vc, 
         educ_no_hs_credential_0_vc, educ_ged_or_alternative_credential_0_vc, educ_regular_high_school_diploma_0_vc,
         educ_bachelors_degree_or_higher_0_vc, postsec_voc_tech_certificate_0, 
         sex_male_0, 
         marstat_married_0, marstat_widowed_0, marstat_divorced_or_separated_0, 
         number_of_children_home_0, any_children_0, birth_country_usa_0, speak_only_english_at_home_0, 
         spoken_english_proficiency_well_0_vc, spoken_english_proficiency_not_well_0_vc, spoken_english_proficiency_not_at_all_0_vc,  
         reading_english_proficiency_well_0_vc, reading_english_proficiency_not_well_0_vc, reading_english_proficiency_not_at_all_0_vc, 
         worked_before_0_vc, never_worked_0_vc,
         tanf_assistance_0, wic_or_snap_assistance_0, future_school_part_time, future_school_full_time, future_work, work_hours,
         financial_support_not_difficult_at_all_vc, financial_support_very_difficult_vc, career_knowledge_3_variables,
         career_knowledge_7_variables, training_commitment_index, academic_discipline_index, emotional_stability_index, social_support_index,
         life_challenges_index_4_variables, life_challenges_index_6_variables, stress_index, weight_15, weight_36, weight_72)


# ----------------- Write extracted info. to CSV ----------------- 
write.csv(all_hpog_df, "AF2--ALL-HPOG.csv", row.names = FALSE)
write.csv(pace_only_df, "AF2--PACE-ONLY-DataFrame.csv", row.names = FALSE)
write.csv(year_up_df, "AF2--YEAR-UP-DataFrame.csv", row.names = FALSE)