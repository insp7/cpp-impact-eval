### forAK11.R -- generate test data 
# Code Author: J.A. Klerman


set.seed(04151959)

############# test data for grand recode

xv_mkAF2 <- function(fp_nobs,fp_vnames){
  fi_nVars <- length(fp_vnames) # number of vars, excluding id
  fi_AF2 <- cbind(seq(1:fp_nobs),seq(1:4),
                  matrix(rbinom(fp_nobs*fi_nVars,1,0.5),
                         nrow=fp_nobs,ncol=fi_nVars) ) 
  colnames(fi_AF2) <- c("participant_id","site_id",fp_vnames)
  xv_mkAF2 <- as.data.frame(fi_AF2)
}

# C/common, H/HPOG, P/PACE, Vn/Variable names
xv_CVn1 <- 
  c("currently_working_15", "currently_working_36", "currently_working_72", 
    "hours_worked_per_week_15", "hours_worked_per_week_36", 
         "hours_worked_per_week_alt_36", "hours_worked_per_week_72", 
    "hourly_wage_rate_15", "hourly_wage_rate_36", 
         "hourly_wage_rate_alt_36", "hourly_wage_rate_72",
    "weekly_earnings_15", "weekly_earnings_36", 
         "weekly_earnings_alt_36", "weekly_earnings_72", 
    "treatment_group_0", 
    "age_LT21_0", "age_21To24_0", "age_25To34_0", "age_GE35_0", 
    "ethnicity_hispanic_0", "race_white_0", "race_black_0", 
         "race_asian_0", "race_american_indian_0", "race_pacific_islander_0", 
    "educ_no_hs_credential_0", "educ_ged_or_alternative_credential_0", 
         "educ_regular_high_school_diploma_0", "educ_some_college_0", 
         "educ_bachelors_degree_or_higher_0",
    "sex_male_0",
    "marstat_married_0", "marstat_widowed_0", "marstat_divorced_or_separated_0", 
         "marstat_never_married_0", 
    "any_children_0", "birth_country_usa_0",
    "currently_working_0", "worked_before_0", "never_worked_0", 
    "tanf_assistance_0", "wic_or_snap_assistance_0",
    "future_school_part_time", "future_school_full_time", 
         "future_work", "work_hours", 
    "career_knowledge_3_variables","life_challenges_index_4_variables"
    )

xv_HVn1 <- 
  c("surveyrespondent_15_hpog", "surveyrespondent_36_hpog", 
         "surveyrespondent_72_hpog", 
    "credential_15", "credential_36", "credential_72", 
    "age_in_years_0", 
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

xv_PVn1 <- 
  c("surveyrespondent_15_pace", "surveyrespondent_36_pace", 
         "surveyrespondent_72_pace",
    "No_HS_in_US_0", "postsec_voc_tech_certificate_0",
    "grades_mostly_A_0", "grades_mostly_B_0", "grades_mostly_C_0", 
         "grades_mostly_D_0", "grades_mostly_F_0", 
    "number_of_children_home_0",
    "speak_only_english_at_home_0", "spoken_english_proficiency_very_well_0", 
         "spoken_english_proficiency_well_0", 
         "spoken_english_proficiency_not_well_0", 
         "spoken_english_proficiency_not_at_all_0",
    "reading_english_proficiency_very_well_0", 
         "reading_english_proficiency_well_0", 
         "reading_english_proficiency_not_well_0", 
         "reading_english_proficiency_not_at_all_0",
    "financial_support_not_difficult_at_all", 
         "financial_support_somewhat_difficult", 
         "financial_support_very_difficult",
    "career_knowledge_7_variables", "training_commitment_index", 
         "academic_discipline_index", "emotional_stability_index", 
         "social_support_index", "life_challenges_index_6_variables", 
         "stress_index", "weight_15", "weight_36", "weight_72" 
  )

xv_AF1vnames <- c(xv_CVn1,xv_HVn1,xv_PVn1)

# xv_AF2vnames <- c("xb1","xb2","xb3")
xv_AF1 <- xv_mkAF2(100,xv_AF1vnames)
print(dim(xv_AF1))
print(head(xv_AF1,n=10))


############# test data for valence coding

xv_mkBinaries <- function(fp_colNames){
  ### generate test matricies
  ### all possible combinations of binary variables, incl NAs
  nV <- 3      # number values
  # nD <- fp_nD # number decimal places (i.e., variables)
  nD <- length(fp_colNames)
  nP <- nV**nD # number of possible combination
  fi_X <- matrix(nrow=nP,ncol=nD)
  # dim(fi_X)
  for (i in 1:nP){      # for each possibility
    # vd[i,1] <- i
    z <- i-1
    for (j in 1:nD){    # for each digit
      d     <- 1+nD-j   # largest to smallest digits
      fi_X[i,j] <- as.integer(z %/% nV**(d-1))
      # cat(j,d,z,vd,"\n")  
      z     <- z - fi_X[i,j] * nV**(d-1)
    }
    # cat(i,i-1,vd,"\n")
  }
  colnames(fi_X)  <- fp_colNames
  fi_X <- ifelse(fi_X==2,NA,fi_X)
  xv_mkBinaries <- fi_X
}
# v_cnames <- c("vb1","vb2","vb3","vb4","vb5")
# testX <- xv_mkBinaries(5,v_cnames)
# print(testX)
# dim(testX)
