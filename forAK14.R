### forAK13.R -- checking problems in recodes
# Code Author: J.A. Klerman

library(dplyr)


xv_nWayTable <- function(fp_df,fp_vList){
  ### generate  SAS PROC FREQ list format output
  ### i.e., one row for each combination of cross-classifying variables
  ### row has values for cross-classifying variable, N, and percent
  # code based on 
  # https://stackoverflow.com/questions/62924862/
  # r-count-using-dynamically-generated-list-of-variables-columns
  fi_result <- fp_df %>% 
    count(!!!syms(fp_vList)) %>% 
    mutate(prop=prop.table(n))
  fi_result[,"prop"] <- 100 * fi_result[,"prop"]
  fi_nCol <- ncol(fi_result)
  colnames(fi_result)[(fi_nCol-1)] <- "N"
  colnames(fi_result)[ fi_nCol   ] <- "%"
  fi_result[,(fi_nCol-1)] <- 
    format(fi_result[,(fi_nCol-1)],big.mark=",",scientific=FALSE)
  fi_result[, fi_nCol   ] <- 
    format(round(fi_result[,fi_nCol],2),nsmall=2)
  print(fi_result)
  xv_nWayTable <- fi_result
}


## test function (using created data)
xv_ftest <- FALSE
if (xv_ftest){
  a<-data.frame(cbind(c(11,12,13,NA),c(21,23,NA,NA),c(32,NA,NA,NA)))
  print(a)
  av <- c("X1","X3")
  xv_result <- xv_nWayTable(a[1:3,],av)
  # print(xv_result)
  # rm(a,av)
}

xv_ftest <- FALSE
if (xv_ftest){
  a <- data.frame(rbind(c(1,0,0),c(0,1,0),c(0,0,1)))
  a <- rbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
  print(dim(a))
  a <- rbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
  a <- rbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
  print(dim(a))  
  av <- c("X1","X3")
  xv_result <- xv_nWayTable(a,av)
  # print(xv_result)
  # rm(a,av)
}

xv_CVn1_IV_worked <- 
  c("currently_working_0"   , "worked_before_0"   , "never_worked_0"   )
xv_result <- xv_nWayTable(xv_AF1,xv_CVn1_IV_worked)
xv_CVn2_IV_worked <- 
  c(                          "worked_before_0_vc", "never_worked_0_vc")
xv_result <- xv_nWayTable(xv_AF2,xv_CVn2_IV_worked)

xv_PVn1_IV_grades <- 
  c("grades_mostly_A_0", "grades_mostly_B_0", "grades_mostly_C_0", 
    "grades_mostly_D_0", "grades_mostly_F_0")
xv_result <- xv_nWayTable(xv_AF1,xv_PVn1_IV_grades)
xv_PVn2_IV_grades <- 
  c("grades_mostly_A_0_vc",                   "grades_mostly_C_0_vc", 
    "grades_mostly_D_0_vc", "grades_mostly_F_0_vc")
xv_result <- xv_nWayTable(xv_AF2,xv_PVn2_IV_grades)