library(readr)
library(dplyr)
library(openxlsx)
library(rprojroot)

# Find the root of the current project
root <- find_root(is_rstudio_project)

HPOG_DS17_PATH <- 'read-HPOG1.0-output/hpog_ds17.csv'
hpog_ds17 <- read.csv(HPOG_DS17_PATH)
hpog_ds17_alt <- hpog_ds17 # for alternative version

# -------  HPOG 1.0 DS17 PREPROCESSING ------- 

#' Function to calculate employment_weekhours as specified:
#' 
#' employment_weekhours: For each unique person(i.e. unique value of ABTSRBIID), 
#' this variable should be set to 0, if CLOSED = 1 (i.e. all job spells for that person have ended),
#' otherwise, set to the sum of F6 across all "open" job spells i.e. when CLOSED = 0
#' 
#' @return A vector of employment_weekhours calculated as per the construction logic.
calculate_employment_weekhours <- function() {
  employment_weekhours <- numeric(nrow(hpog_ds17)) # initialize a vector to store the calculated values
  unique_abtsrbiid <- unique(hpog_ds17$ABTSRBIID) 
  
  # iterate over unique values of abtsrbiid
  for(id in unique_abtsrbiid) { 
    # subset the data for the current abtsrbiid
    hpog_ds17_subset <- hpog_ds17[hpog_ds17$ABTSRBIID == id, ]
    # calculate the sum of f6 when closed = 0
    employment_weekhours[hpog_ds17$ABTSRBIID == id] <- sum(hpog_ds17_subset$F6[hpog_ds17_subset$CLOSED == 0])
  }  
  return (employment_weekhours)
}

hpog_ds17 <- hpog_ds17[hpog_ds17$CLOSED == 0, ] # select all rows where closed == 0 i.e. select all open job spells
hpog_ds17$F5[hpog_ds17$F5 < 0 | hpog_ds17$F5 > 75] <- NA # =NA if (F5 < 0 or > 75)
hpog_ds17$F6[hpog_ds17$F6 < 1 | hpog_ds17$F6 > 80] <- NA # =NA if (F6 < 1 or > 80)
hpog_ds17$employment_weekhours <- calculate_employment_weekhours()
hpog_ds17 <- hpog_ds17 %>% 
  mutate(weekly_earnings_36_variation = case_when(
    F5A_2 == 1 ~ F5A * 5,
    F5A_2 == 2 ~ F5A,
    F5A_2 == 3 ~ F5A / 2,
    F5A_2 == 4 ~ F5A / 4,
    F5A_2 == 5 | F5A_2 == 6 | F5A_2 == 97 | F5A_2 == 98 ~ NA_real_,
    F5A_2 == 7 ~ F5A / 52,
    TRUE ~ NA_real_
  ))
hpog_ds17$employment_weekearnings <- ifelse(!is.na(hpog_ds17$F5) & !is.na(hpog_ds17$F6), 
                                            hpog_ds17$F5 * hpog_ds17$F6, 
                                            ifelse((is.na(hpog_ds17$F5) | is.na(hpog_ds17$F6)) & !is.na(hpog_ds17$F5A), 
                                                   hpog_ds17$weekly_earnings_36_variation, NA))
hpog_ds17 <- hpog_ds17 %>% 
  group_by(ABTSRBIID, employment_weekhours) %>% 
  summarise(employment_weekearnings = sum(employment_weekearnings, na.rm = T)) %>% 
  ungroup()
hpog_ds17$employment_weekhours[hpog_ds17$employment_weekhours == 0] <- NA # if employment_weekhours == 0, set it to NA -- to avoid divide by zero error.
hpog_ds17$employment_weekearnings[hpog_ds17$employment_weekearnings == 0] <- NA # set employment_weekearnings to NA, if employment_weekearnings is 0
hpog_ds17$employment_hourlywage <- ifelse(!is.na(hpog_ds17$employment_weekearnings) & !is.na(hpog_ds17$employment_weekhours), 
                                          hpog_ds17$employment_weekearnings / hpog_ds17$employment_weekhours, NA)
hpog_ds17 <- hpog_ds17 %>% 
  select(ABTSRBIID, employment_hourlywage, employment_weekhours, employment_weekearnings)

# -------  ALTERNATIVE HPOG 1.0 DS17 PREPROCESSING ------- 
hpog_ds17_alt <- hpog_ds17_alt[hpog_ds17_alt$CLOSED == 0, ] # select all rows where closed == 0
hpog_ds17_alt$F5[hpog_ds17_alt$F5 < 0 | hpog_ds17_alt$F5 > 75] <- NA # =NA if (F5 < 0 or > 75)
hpog_ds17_alt$F6[hpog_ds17_alt$F6 < 1 | hpog_ds17_alt$F6 > 80] <- NA # =NA if (F6 < 1 or > 80)

# Retain latest open jobs -- 
# when a participant has more than one job starting on the same earliest STARTDATE, 
# randomly pick one open job
set.seed(7)
hpog_ds17_alt <- hpog_ds17_alt %>% 
  group_by(ABTSRBIID) %>% 
  filter(STARTDATE == max(STARTDATE)) %>% # MAX = earliest startdate
  ungroup() %>% 
  group_by(ABTSRBIID) %>% 
  sample_n(1) %>% 
  ungroup()
hpog_ds17_alt$employment_weekhours <- hpog_ds17_alt$F6
hpog_ds17_alt <- hpog_ds17_alt %>% 
  mutate(weekly_earnings_36_variation = case_when(
    F5A_2 == 1 ~ F5A * 5,
    F5A_2 == 2 ~ F5A,
    F5A_2 == 3 ~ F5A / 2,
    F5A_2 == 4 ~ F5A / 4,
    F5A_2 == 5 | F5A_2 == 6 | F5A_2 == 97 | F5A_2 == 98 ~ NA_real_,
    F5A_2 == 7 ~ F5A / 52,
    TRUE ~ NA_real_
  ))
hpog_ds17_alt$employment_weekearnings <- ifelse(!is.na(hpog_ds17_alt$F5) & !is.na(hpog_ds17_alt$F6), 
                                                hpog_ds17_alt$F5 * hpog_ds17_alt$F6, 
                                                ifelse((is.na(hpog_ds17_alt$F5) | is.na(hpog_ds17_alt$F6)) & !is.na(hpog_ds17_alt$F5A), 
                                                       hpog_ds17_alt$weekly_earnings_36_variation, NA))

hpog_ds17_alt <- hpog_ds17_alt %>% 
  group_by(ABTSRBIID, employment_weekhours) %>% 
  summarise(employment_weekearnings = sum(employment_weekearnings, na.rm = T))
hpog_ds17_alt$employment_weekhours[hpog_ds17_alt$employment_weekhours == 0] <- NA # if employment_weekhours == 0, set it to NA -- to avoid divide by zero error.
hpog_ds17_alt$employment_weekearnings[hpog_ds17_alt$employment_weekearnings == 0] <- NA # set employment_weekearnings to NA, if employment_weekearnings is 0
hpog_ds17_alt$employment_hourlywage <- ifelse(!is.na(hpog_ds17_alt$employment_weekearnings) & !is.na(hpog_ds17_alt$employment_weekhours), 
                                              hpog_ds17_alt$employment_weekearnings / hpog_ds17_alt$employment_weekhours, NA)
hpog_ds17_alt <- hpog_ds17_alt %>% rename(employment_hourlywage_alt = employment_hourlywage)
hpog_ds17_alt <- hpog_ds17_alt %>% rename(employment_weekhours_alt = employment_weekhours)
hpog_ds17_alt <- hpog_ds17_alt %>% rename(employment_weekearnings_alt = employment_weekearnings)
hpog_ds17 <- left_join(hpog_ds17_alt, hpog_ds17, by = c("ABTSRBIID"))

# ------- Write extracted info. to CSV ------- 
folder_path <- file.path(root, "read-HPOG1.0-output")

# Create the folder if it doesn't exist
if(!dir.exists(folder_path)) { 
  dir.create(folder_path)
}
write.csv(hpog_ds17, file.path(folder_path,"hpog_ds17-participant-level.csv"), row.names = FALSE)

# Remove all data frames
rm(list = ls())
gc()