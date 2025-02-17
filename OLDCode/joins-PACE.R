library(readr)
library(dplyr)
library(openxlsx)
library(rprojroot)

# Find the root of the current project
root <- find_root(is_rstudio_project)
PACE_DATA_PATH <- 'read-PACE-output/'

# Read PACE data
pace_ds1 <- read.csv(paste(PACE_DATA_PATH, 'pace_ds1.csv', sep = ""))
pace_ds2 <- read.csv(paste(PACE_DATA_PATH, 'pace_ds2.csv', sep = ""))
pace_ds7 <- read.csv(paste(PACE_DATA_PATH, 'pace_ds7.csv', sep = ""))
pace_ds8 <- read.csv(paste(PACE_DATA_PATH, 'pace_ds8.csv', sep = ""))
pace_ds10 <- read.csv(paste(PACE_DATA_PATH, 'pace_ds10-participant-level.csv', sep = ""))
pace_ds15 <- read.csv(paste(PACE_DATA_PATH, 'pace_ds15.csv', sep = ""))

# ----------------- CREATE PACE DATA FRAME -----------------

# ALL PACE DATA in one table joined on ISISID = B_UNIQUE_ID1 = ABTSRBIID = ABTID
pace_df1 <- inner_join(pace_ds1, pace_ds2, by = c("ISISID" = "B_UNIQUE_ID1"))
pace_df2 <- inner_join(pace_ds7, pace_df1, by = c("B_UNIQUE_ID1" = "ISISID"))
pace_df3 <- inner_join(pace_ds8, pace_df2, by = c("ABTSRBIID" = "B_UNIQUE_ID1"))
pace_df4 <- left_join(pace_df3, pace_ds10, by = c("ABTSRBIID"))
pace_df <- full_join(pace_df4, pace_ds15, by = c("ABTSRBIID" = "ABTID"))  # ALL PACE DATA in 1 Table

# ------- Write extracted info. to CSV ------- 
folder_path <- file.path(root, "read-PACE-output")

# Create the folder if it doesn't exist
if(!dir.exists(folder_path)) { 
  dir.create(folder_path)
}
write.csv(pace_df, file.path(folder_path,"pace-joined-df.csv"), row.names = FALSE)

# Remove all data frames
rm(list = ls())
gc()