library(readr)
library(dplyr)
library(openxlsx)
library(rprojroot)

# Find the root of the current project
root <- find_root(is_rstudio_project)

PACE_DS10_PATH <- 'read-PACE-output/pace_ds10.csv'
pace_ds10 <- read.csv(PACE_DS10_PATH)
pace_ds10_alt <- pace_ds10 # for alternative version

# ------- Pre-processing for PACE DS10 ------- 

calculate_employment_weekhours_pace <- function() {
  employment_weekhours_pace <- numeric(nrow(pace_ds10)) # initialize a vector to store the calculated values
  
  # iterate over unique values of abtsrbiid
  unique_abtsrbiid <- unique(pace_ds10$ABTSRBIID)
  for(id in unique_abtsrbiid) {
    # subset the data for the current abtsrbiid
    pace_ds10_subset <- pace_ds10[pace_ds10$ABTSRBIID == id, ]
    # calculate the sum of f6 when closed = 0
    employment_weekhours_pace[pace_ds10$ABTSRBIID == id] <- sum(pace_ds10_subset$F6[pace_ds10_subset$CLOSED == 0])
  }  
  return (employment_weekhours_pace)
}

pace_ds10 <- pace_ds10[pace_ds10$CLOSED == 0, ] # select all rows where closed == 0
pace_ds10$F5[pace_ds10$F5 < 0 | pace_ds10$F5 > 75] <- NA # =NA if (F5 < 0 or > 75)
pace_ds10$F6[pace_ds10$F6 < 1 | pace_ds10$F6 > 80] <- NA # =NA if (F6 < 1 or > 80)
pace_ds10$employment_weekhours_pace <- calculate_employment_weekhours_pace()
pace_ds10 <- pace_ds10 %>% 
  mutate(weekly_earnings_36_variation = case_when(
    F1A_2 == 1 ~ F1A * 5,
    F1A_2 == 2 ~ F1A,
    F1A_2 == 3 ~ F1A / 2,
    F1A_2 == 4 ~ F1A / 4,
    F1A_2 == 5 | F1A_2 == 6 | F1A_2 == 97 | F1A_2 == 98 ~ NA_real_,
    F1A_2 == 7 ~ F1A / 52,
    TRUE ~ NA_real_
  ))
pace_ds10$employment_weekearnings_pace <- ifelse(!is.na(pace_ds10$F5) & !is.na(pace_ds10$F6), 
                                                 pace_ds10$F5 * pace_ds10$F6, 
                                                 ifelse((is.na(pace_ds10$F5) | is.na(pace_ds10$F6)) & !is.na(pace_ds10$F1A), 
                                                        pace_ds10$weekly_earnings_36_variation, NA))
pace_ds10 <- pace_ds10 %>% 
  group_by(ABTSRBIID, employment_weekhours_pace) %>% 
  summarise(employment_weekearnings_pace = sum(employment_weekearnings_pace, na.rm = T))
pace_ds10$employment_weekhours_pace[pace_ds10$employment_weekhours_pace == 0] <- NA # if employment_weekhours_pace == 0, set it to NA -- to avoid divide by zero error.
pace_ds10$employment_weekearnings_pace[pace_ds10$employment_weekearnings_pace == 0] <- NA # set employment_weekearnings_pace to NA, if employment_weekearnings_pace is 0
pace_ds10$employment_hourlywage_pace <- ifelse(!is.na(pace_ds10$employment_weekearnings_pace) & !is.na(pace_ds10$employment_weekhours_pace), 
                                               pace_ds10$employment_weekearnings_pace / pace_ds10$employment_weekhours_pace, NA)
pace_ds10 <- pace_ds10 %>% 
  select(ABTSRBIID, employment_hourlywage_pace, employment_weekhours_pace, employment_weekearnings_pace)


# -------  ALTERNATIVE PACE DS10 PREPROCESSING ------- 
pace_ds10_alt <- pace_ds10_alt[pace_ds10_alt$CLOSED == 0, ] # select all rows where closed == 0
pace_ds10_alt$F5[pace_ds10_alt$F5 < 0 | pace_ds10_alt$F5 > 75] <- NA # =NA if (F5 < 0 or > 75)
pace_ds10_alt$F6[pace_ds10_alt$F6 < 1 | pace_ds10_alt$F6 > 80] <- NA # =NA if (F6 < 1 or > 80)

# Retain latest open jobs -- 
# when a participant has more than one job starting on the same earliest STARTDATE, 
# randomly pick one open job
set.seed(7)
pace_ds10_alt <- pace_ds10_alt %>% 
  group_by(ABTSRBIID) %>% 
  filter(STARTDATE == max(STARTDATE)) %>% # MAX = earliest STARTDATE, MIN = latest STARTDATE
  ungroup() %>% 
  group_by(ABTSRBIID) %>% 
  sample_n(1) %>% 
  ungroup()
pace_ds10_alt$employment_weekhours_pace_alt <- pace_ds10_alt$F6
pace_ds10_alt <- pace_ds10_alt %>% 
  mutate(weekly_earnings_36_variation = case_when(
    F1A_2 == 1 ~ F1A * 5,
    F1A_2 == 2 ~ F1A,
    F1A_2 == 3 ~ F1A / 2,
    F1A_2 == 4 ~ F1A / 4,
    F1A_2 == 5 | F1A_2 == 6 | F1A_2 == 97 | F1A_2 == 98 ~ NA_real_,
    F1A_2 == 7 ~ F1A / 52,
    TRUE ~ NA_real_
  ))
pace_ds10_alt$employment_weekearnings_pace_alt <- ifelse(!is.na(pace_ds10_alt$F5) & !is.na(pace_ds10_alt$F6), 
                                                pace_ds10_alt$F5 * pace_ds10_alt$F6, 
                                                ifelse((is.na(pace_ds10_alt$F5) | is.na(pace_ds10_alt$F6)) & !is.na(pace_ds10_alt$F1A), 
                                                       pace_ds10_alt$weekly_earnings_36_variation, NA))

pace_ds10_alt <- pace_ds10_alt %>% 
  group_by(ABTSRBIID, employment_weekhours_pace_alt) %>% 
  summarise(employment_weekearnings_pace_alt = sum(employment_weekearnings_pace_alt, na.rm = T))
pace_ds10_alt$employment_weekhours_pace_alt[pace_ds10_alt$employment_weekhours_pace_alt == 0] <- NA # if employment_weekhours == 0, set it to NA -- to avoid divide by zero error.
pace_ds10_alt$employment_weekearnings_pace_alt[pace_ds10_alt$employment_weekearnings_pace_alt == 0] <- NA # set employment_weekearnings to NA, if employment_weekearnings is 0
pace_ds10_alt$employment_hourlywage_pace_alt <- ifelse(!is.na(pace_ds10_alt$employment_weekearnings_pace_alt) & !is.na(pace_ds10_alt$employment_weekhours_pace_alt), 
                                                       pace_ds10_alt$employment_weekearnings_pace_alt / pace_ds10_alt$employment_weekhours_pace_alt, NA)
pace_ds10 <- left_join(pace_ds10_alt, pace_ds10, by = c("ABTSRBIID"))

# ------- Write extracted info. to CSV ------- 
folder_path <- file.path(root, "read-PACE-output")

# Create the folder if it doesn't exist
if(!dir.exists(folder_path)) { 
  dir.create(folder_path)
}
write.csv(pace_ds10, file.path(folder_path,"pace_ds10-participant-level.csv"), row.names = FALSE)

# Remove all data frames
rm(list = ls())
gc()