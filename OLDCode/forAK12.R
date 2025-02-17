### forAK12.R -- recode using "patterns" coding
# Code Author: J.A. Klerman (https://github.com/jklerman)
# Full Code Repo: https://github.com/jklerman/OPRE-XVAL

xv_AF1 <- read.csv("AF1.csv")

sum(xv_AF1$currently_working_0 == 1 & xv_AF1$worked_before_0 == 1, na.rm=T) 
sum(xv_AF1$currently_working_0 == 1 
    & xv_AF1$worked_before_0 == 1 
    & xv_AF1$study_pace == 1, na.rm = T) # PACE Only: hpog = 0, pace = 1, yearup = 0

xv_AF1 %>% 
  filter(study_hpog == 1, study_pace == 1, currently_working_0 == 1, worked_before_0 == 1) %>% 
  nrow()

# When strictly checking for pace:
sum(xv_AF1$currently_working_0 == 1 
    & xv_AF1$worked_before_0 == 1 
    & xv_AF1$study_type == 3, na.rm = T) # PACE Only: hpog = 0, pace = 1, yearup = 0

table(xv_AF1$currently_working_0)

xv_doPatterns <- function(fp_inX,fp_inColNames,fp_outColNames,
                          fp_inPatterns,fp_outPatterns){
  ### recode the patterns
  # initialize to all missing
  # one less column, dropping excluded category (usually center of valence)
  fi_inX <-subset(fp_inX,select=fp_inColNames)
  fi_outX <- matrix(NA,nrow=nrow(fi_inX),ncol=ncol(fi_inX)-1)
  print(dim(fi_outX))
  mode(fi_outX) <- "integer"
  print(dim(fi_outX))
  print(dim(fp_outPatterns))
  for (irow in 1:nrow(fi_inX)) {   # for each input row 
    for (jrow in 1:nrow(fp_inPatterns)){  # check equality with each inPattern
      if (identical(as.integer(fi_inX[irow,]),
                    as.integer(fp_inPatterns[jrow,])) )
        fi_outX[irow,] <- as.integer(fp_outPatterns[jrow,])
    }
  }
  colnames(fi_outX) <- fp_outColNames
  print("input vNames")
  print(fp_inColNames)
  print("output vNames")
  print(fp_outColNames)
  print(data.frame(fp_inPatterns," <- in /// out ->",
                   fp_outPatterns))
  xv_doPatterns <-fi_outX
}


### not IV/independent variable
xv_CVn1_notIV <- 
  c("participant_id","site_id",
    "currently_working_15", "currently_working_36", "currently_working_72", 
    "hours_worked_per_week_15", "hours_worked_per_week_36", 
    "hours_worked_per_week_alt_36", "hours_worked_per_week_72", 
    "hourly_wage_rate_15", "hourly_wage_rate_36", 
    "hourly_wage_rate_alt_36", "hourly_wage_rate_72",
    "weekly_earnings_15", "weekly_earnings_36", 
    "weekly_earnings_alt_36", "weekly_earnings_72", 
    "treatment_group_0")
xv_HVn1_notIV <- 
  c("credential_15", "credential_36", "credential_72",
    "surveyrespondent_15_hpog", "surveyrespondent_36_hpog", 
         "surveyrespondent_72_hpog")
xv_PVn1_notIV <- 
  c("surveyrespondent_15_pace", "surveyrespondent_36_pace", 
    "surveyrespondent_72_pace",
    "weight_15", "weight_36", "weight_72")

## IVs unchanged
xv_CVn1_IV_unchanged <- 
  c("ethnicity_hispanic_0", 
    "sex_male_0",
    "any_children_0", "birth_country_usa_0",
    "tanf_assistance_0", "wic_or_snap_assistance_0",
    "future_school_part_time", "future_school_full_time", 
    "future_work", "work_hours", 
    "career_knowledge_3_variables","life_challenges_index_4_variables"
  )

xv_HVn1_IV_unchanged <- 
  c("age_in_years_0", 
    "occupational_license_certification_0",
    "number_of_children_dependent_0", "limited_english_0",
    
    "number_of_services", "behavioral_incentives", "avg_fte_caseload", 
    "childcare_transport", "social_services", "financial_services", 
    "number_of_employment_supports", "number_of_colocated_services", 
    "emergency_assistance", "peer_support", "number_of_cp_principles", 
    "proportion_local_some_college", "proportion_local_jobs_health_care", 
    "median_wage_local_health_care", "proportion_local_cash_assistance",
    "proportion_local_enrolled_school", "total_msa_population", 
    "percent_local_unemployed" 
  )
xv_PVn1_IV_unchanged <- 
  c("No_HS_in_US_0", "postsec_voc_tech_certificate_0",
    "number_of_children_home_0",
    "speak_only_english_at_home_0", 
    "career_knowledge_7_variables", "training_commitment_index", 
    "academic_discipline_index", "emotional_stability_index", 
    "social_support_index", "life_challenges_index_6_variables", 
    "stress_index"  
  )

# was 21 vars
xv_CVn1_IV_changed <- 
  c("race_white_0", "race_black_0", 
    "race_asian_0", "race_american_indian_0", "race_pacific_islander_0", 
    "marstat_married_0", "marstat_widowed_0", "marstat_divorced_or_separated_0", 
    "marstat_never_married_0"
  )
# dropping: "race_white_0", "marstat_never_married_0"
xv_CVn2_IV_changed <- 
  c(                "race_black_0", 
    "race_asian_0", "race_american_indian_0", "race_pacific_islander_0", 
    "marstat_married_0", "marstat_widowed_0", "marstat_divorced_or_separated_0"
    
  )


xv_CVn1_IV_age <- 
  c("age_LT21_0"   , "age_21To24_0"   , "age_25To34_0"   , "age_GE35_0"   )
xv_CVn2_IV_age <- 
  c("age_LT21_0_vc", "age_21To24_0_vc",                    "age_GE35_0_vc")
# valence coding, excluding the third category
xv_inPatterns_age  <- rbind(c(1,0,0,0),
                            c(0,1,0,0),
                            c(0,0,1,0),
                            c(0,0,0,1) )
xv_outPatterns_age <- rbind(c(1,1,0),
                            c(0,1,0),
                            c(0,0,0),
                            c(0,0,1) )
xv_AF2_IV_age <-
  xv_doPatterns(xv_AF1, xv_CVn1_IV_age,     xv_CVn2_IV_age,
                     xv_inPatterns_age, xv_outPatterns_age ) 
rm(xv_inPatterns_age,xv_outPatterns_age)
rm(xv_CVn1_IV_age,xv_CVn2_IV_age)

xv_CVn1_IV_educ <- 
  c("educ_no_hs_credential_0"   , "educ_ged_or_alternative_credential_0"   , 
    "educ_regular_high_school_diploma_0"   , "educ_some_college_0"   , 
    "educ_bachelors_degree_or_higher_0"
  )
xv_CVn2_IV_educ <- 
  c("educ_no_hs_credential_0_vc", "educ_ged_or_alternative_credential_0_vc", 
    "educ_regular_high_school_diploma_0_vc",  
    "educ_bachelors_degree_or_higher_0_vc"
  )
xv_inPatterns_educ  <- rbind(c(1,0,0,0,0),
                             c(0,1,0,0,0),
                             c(0,0,1,0,0),
                             c(0,0,0,1,0),
                             c(0,0,0,0,1) )
xv_outPatterns_educ <- rbind(c(1,1,1,0),
                             c(0,1,1,0),
                             c(0,0,1,0),
                             c(0,0,0,0),
                             c(0,0,0,1) )
xv_AF2_IV_educ <-
  xv_doPatterns(xv_AF1, xv_CVn1_IV_educ,     xv_CVn2_IV_educ,
                     xv_inPatterns_educ, xv_outPatterns_educ ) 
rm(xv_inPatterns_educ,xv_outPatterns_educ)
rm(xv_CVn1_IV_educ,xv_CVn2_IV_educ)

xv_CVn1_IV_worked <- 
  c("currently_working_0"   , "worked_before_0"   , "never_worked_0"   )
xv_CVn2_IV_worked <- 
  c(                          "worked_before_0_vc", "never_worked_0_vc")
# valence coding, excluding the first category
# questionairre skip patterns imply a hierarchy
# many people did not follow the hierarchy
# in partiuclar, 24 percent answer BOTH currently_working and worked_before
# coding them as currently working (as of 8/2/2024)
# patterns below impose the hierarchy
xv_inPatterns_worked  <- rbind(c(1,1,0),
                               c(1,0,1),
                               c(1,0,0),
                               c(0,1,0),
                               c(0,0,1) )
xv_outPatterns_worked <- rbind(c(0,0),
                               c(0,0),
                               c(0,0),
                               c(1,0),
                               c(1,1) )
xv_AF2_IV_worked <-
  xv_doPatterns(xv_AF1, xv_CVn1_IV_worked,     xv_CVn2_IV_worked,
                     xv_inPatterns_worked, xv_outPatterns_worked ) 
rm(xv_inPatterns_worked,xv_outPatterns_worked)
rm(xv_CVn1_IV_worked,xv_CVn2_IV_worked)

## was 0 vars
xv_HVn1_IV_changed <- c()
## was 17 vars
xv_PVn1_IV_grades <- 
  c("grades_mostly_A_0", "grades_mostly_B_0", "grades_mostly_C_0", 
    "grades_mostly_D_0", "grades_mostly_F_0")
xv_PVn2_IV_grades <- 
  c("grades_mostly_A_0_vc",                   "grades_mostly_C_0_vc", 
    "grades_mostly_D_0_vc", "grades_mostly_F_0_vc")
## valence coding, excluding second category
## "Did not attend high school in US" is an answer
## leaving that dummy variable in the model
## coding them as the excluded category (as of 8/2/2024)
xv_inPatterns_grades  <- rbind(c(0,0,0,0,0),
                               c(1,0,0,0,0),
                               c(0,1,0,0,0),
                               c(0,0,1,0,0),
                               c(0,0,0,1,0),
                               c(0,0,0,0,1) )
xv_outPatterns_grades <- rbind(c(0,0,0,0),
                               c(1,0,0,0),
                               c(0,0,0,0),
                               c(0,1,0,0),
                               c(0,1,1,0),
                               c(0,1,1,1))
xv_AF2_IV_grades <-
  xv_doPatterns(xv_AF1, xv_PVn1_IV_grades,     xv_PVn2_IV_grades,
                     xv_inPatterns_grades, xv_outPatterns_grades ) 
rm(xv_inPatterns_grades,xv_outPatterns_grades)
rm(xv_PVn1_IV_grades,xv_PVn2_IV_grades)

xv_PVn1_IV_speak <- 
  c("spoken_english_proficiency_very_well_0", 
    "spoken_english_proficiency_well_0", 
    "spoken_english_proficiency_not_well_0", 
    "spoken_english_proficiency_not_at_all_0")
xv_PVn2_IV_speak <- 
  c(
    "spoken_english_proficiency_well_0_vc", 
    "spoken_english_proficiency_not_well_0_vc", 
    "spoken_english_proficiency_not_at_all_0_vc"    )
## valence coding, excluding first category
xv_inPatterns_speak  <- rbind(c(1,0,0,0),
                              c(0,1,0,0),
                              c(0,0,1,0),
                              c(0,0,0,1) )
xv_outPatterns_speak <- rbind(c(0,0,0),
                              c(1,0,0),
                              c(1,1,0),
                              c(1,1,1))
xv_AF2_IV_speak <-
  xv_doPatterns(xv_AF1, xv_PVn1_IV_speak,     xv_PVn2_IV_speak,
                     xv_inPatterns_speak, xv_outPatterns_speak ) 
rm(xv_inPatterns_speak,xv_outPatterns_speak)
rm(xv_PVn1_IV_speak,xv_PVn2_IV_speak)

xv_PVn1_IV_read <- 
  c("reading_english_proficiency_very_well_0",
    "reading_english_proficiency_well_0", 
    "reading_english_proficiency_not_well_0", 
    "reading_english_proficiency_not_at_all_0")
xv_PVn2_IV_read <- 
  c(
    "reading_english_proficiency_well_0_vc", 
    "reading_english_proficiency_not_well_0_vc", 
    "reading_english_proficiency_not_at_all_0_vc")
## valence coding, excluding first category
xv_inPatterns_read  <- rbind(c(1,0,0,0),
                             c(0,1,0,0),
                             c(0,0,1,0),
                             c(0,0,0,1) )
xv_outPatterns_read <- rbind(c(0,0,0),
                             c(1,0,0),
                             c(1,1,0),
                             c(1,1,1))
xv_AF2_IV_read <-
  xv_doPatterns(xv_AF1, xv_PVn1_IV_read,     xv_PVn2_IV_read,
                     xv_inPatterns_read, xv_outPatterns_read ) 
rm(xv_inPatterns_read,xv_outPatterns_read)
rm(xv_PVn1_IV_read,xv_PVn2_IV_read)

xv_PVn1_IV_support <- 
  c("financial_support_not_difficult_at_all", 
    "financial_support_somewhat_difficult", 
    "financial_support_very_difficult"  )
xv_PVn2_IV_support <- 
  c("financial_support_not_difficult_at_all_vc", 
 
    "financial_support_very_difficult_vc"  )
# valence coding, excluding middle/second category
xv_inPatterns_support  <- rbind(c(1,0,0),
                                c(0,1,0),
                                c(0,0,1) )
xv_outPatterns_support <- rbind(c(1,0),
                                c(0,0),
                                c(0,1) )
xv_AF2_IV_support <-
   xv_doPatterns(xv_AF1, xv_PVn1_IV_support,    xv_PVn2_IV_support,
                      xv_inPatterns_support, xv_outPatterns_support )                          
rm(xv_inPatterns_support,xv_outPatterns_support)
rm(xv_PVn1_IV_support,xv_PVn2_IV_support)

### create AF2

## handle variables that are unchanged from AF1 (dropping some)
xv_Vn_simple <- 
  c(xv_CVn1_notIV       ,xv_HVn1_notIV       ,xv_PVn1_notIV,
    xv_CVn1_IV_unchanged,xv_HVn1_IV_unchanged,xv_PVn1_IV_unchanged,
    xv_CVn2_IV_changed)
xv_AF2_simple <- data.frame(xv_AF1[,xv_Vn_simple])
print(length(xv_Vn_simple))
print(dim(xv_AF2_simple))
rm(xv_CVn1_notIV       ,xv_HVn1_notIV       ,xv_PVn1_notIV,
   xv_CVn1_IV_unchanged,xv_HVn1_IV_unchanged,xv_PVn1_IV_unchanged,
   xv_CVn2_IV_changed)
rm(xv_CVn1_IV_changed,xv_HVn1_IV_changed,xv_Vn_simple)

## create AF2 combining simple (drops) and recodes
xv_AF2 <- data.frame(xv_AF2_simple,
   xv_AF2_IV_age   , xv_AF2_IV_educ,  xv_AF2_IV_worked,
   xv_AF2_IV_grades, xv_AF2_IV_speak, xv_AF2_IV_read,   xv_AF2_IV_support)
print(dim(xv_AF1))
print(dim(xv_AF2))

## clean up
rm(xv_AF2_simple,
   xv_AF2_IV_age   , xv_AF2_IV_educ,  xv_AF2_IV_worked,
   xv_AF2_IV_grades, xv_AF2_IV_speak, xv_AF2_IV_read,   xv_AF2_IV_support)
rm(xv_doPatterns)

xv_AF2$study_type <- xv_AF1$study_type
xv_AF2$study_pace <- xv_AF1$study_pace
xv_AF2$study_hpog <- xv_AF1$study_hpog
xv_AF2$study_year_up <- xv_AF1$study_year_up

write.csv(xv_AF2, "AF2.csv", row.names=F)
