### forAK13.R -- sample statistics to check recodes
# Code Author: J.A. Klerman

xv_sampStat <- function(fp_inX,fp_inVList){
  # get notNA, mean for var list
  # print(fp_inVList)
  fi_mean <- colMeans(fp_inX[fp_inVList],na.rm=TRUE)
  # print(fi_mean)
  fi_notNA <- colSums(!is.na(fp_inX[fp_inVList]))
  fi_SD    <- apply(fp_inX[,fp_inVList],2,sd,na.rm=TRUE)
  # print(fi_notNA)
  fi_result <- data.frame(fi_notNA,fi_mean,fi_SD)
  colnames(fi_result) <- c("N","mean","SD")
  print(fi_result)
  xv_sampStat <- fi_result
}


## test function (using created data)
xv_ftest <- FALSE
if (xv_ftest){
  a<-data.frame(cbind(c(11,12,13,NA),c(21,23,NA,NA),c(32,NA,NA,NA)))
  print(a)
  ac <- c("X1","X3")
  xv_result <- xv_sampStat(a,ac)
  # print(xv_result)  
  rm(a,ac)
}

xv_CVn1_IV_age <- 
  c("age_LT21_0"   , "age_21To24_0"   , "age_25To34_0"   , "age_GE35_0"   )
xv_result_AF1 <- xv_sampStat(xv_AF1,xv_CVn1_IV_age)
xv_CVn2_IV_age <- 
  c("age_LT21_0_vc", "age_21To24_0_vc",                    "age_GE35_0_vc")
xv_result_AF2 <- xv_sampStat(xv_AF2,xv_CVn2_IV_age)

xv_CVn1_IV_educ <- 
  c("educ_no_hs_credential_0"   , "educ_ged_or_alternative_credential_0"   , 
    "educ_regular_high_school_diploma_0"   , "educ_some_college_0"   , 
    "educ_bachelors_degree_or_higher_0"
  )
xv_result_AF1 <- xv_sampStat(xv_AF1,xv_CVn1_IV_educ)
xv_CVn2_IV_educ <- 
  c("educ_no_hs_credential_0_vc", "educ_ged_or_alternative_credential_0_vc", 
    "educ_regular_high_school_diploma_0_vc",  
    "educ_bachelors_degree_or_higher_0_vc"
  )
xv_result_AF2 <- xv_sampStat(xv_AF2,xv_CVn2_IV_educ)

xv_CVn1_IV_worked <- 
  c("currently_working_0"   , "worked_before_0"   , "never_worked_0"   )
xv_result_AF1 <- xv_sampStat(xv_AF1,xv_CVn1_IV_worked)
xv_CVn2_IV_worked <- 
  c(                          "worked_before_0_vc", "never_worked_0_vc")
xv_result_AF2 <- xv_sampStat(xv_AF2,xv_CVn2_IV_worked)

xv_PVn1_IV_grades <- 
  c("grades_mostly_A_0", "grades_mostly_B_0", "grades_mostly_C_0", 
    "grades_mostly_D_0", "grades_mostly_F_0")
xv_result_AF1 <- xv_sampStat(xv_AF1,xv_PVn1_IV_grades)
xv_PVn2_IV_grades <- 
  c("grades_mostly_A_0_vc",                   "grades_mostly_C_0_vc", 
    "grades_mostly_D_0_vc", "grades_mostly_F_0_vc")
xv_result_AF2 <- xv_sampStat(xv_AF2,xv_PVn2_IV_grades)

xv_PVn1_IV_speak <- 
c("spoken_english_proficiency_very_well_0", 
  "spoken_english_proficiency_well_0", 
  "spoken_english_proficiency_not_well_0", 
  "spoken_english_proficiency_not_at_all_0")
xv_result_AF1 <- xv_sampStat(xv_AF1,xv_PVn1_IV_speak)
xv_PVn2_IV_speak <- 
  c(
    "spoken_english_proficiency_well_0_vc", 
    "spoken_english_proficiency_not_well_0_vc", 
    "spoken_english_proficiency_not_at_all_0_vc"    )
xv_result_AF2 <- xv_sampStat(xv_AF2,xv_PVn2_IV_speak)

xv_PVn1_IV_read <- 
  c("reading_english_proficiency_very_well_0",
    "reading_english_proficiency_well_0", 
    "reading_english_proficiency_not_well_0", 
    "reading_english_proficiency_not_at_all_0")
xv_result_AF1 <- xv_sampStat(xv_AF1,xv_PVn1_IV_read)
xv_PVn2_IV_read <- 
  c(
    "reading_english_proficiency_well_0_vc", 
    "reading_english_proficiency_not_well_0_vc", 
    "reading_english_proficiency_not_at_all_0_vc")
xv_result_AF2 <- xv_sampStat(xv_AF2,xv_PVn2_IV_read)

xv_PVn1_IV_support <- 
  c("financial_support_not_difficult_at_all", 
    "financial_support_somewhat_difficult", 
    "financial_support_very_difficult"  )
xv_result_AF1 <- xv_sampStat(xv_AF1,xv_PVn1_IV_support)
xv_PVn2_IV_support <- 
  c("financial_support_not_difficult_at_all_vc", 
    
    "financial_support_very_difficult_vc"  )
xv_result_AF2 <- xv_sampStat(xv_AF2,xv_PVn2_IV_support)


