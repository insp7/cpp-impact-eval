library(readr)
library(dplyr)
library(openxlsx)
library(rprojroot)

# Find the root of the current project
root <- find_root(is_rstudio_project)

source("utils.R")

HPOG_DATA_PATH <- 'read-HPOG1.0-output/'

# ----------------- Read HPOG1.0 data ----------------- 
hpog_ds4 <- read.csv(paste(HPOG_DATA_PATH, 'hpog_ds4.csv', sep = ""))
hpog_ds5 <- read.csv(paste(HPOG_DATA_PATH, 'hpog_ds5.csv', sep = ""))
hpog_ds6 <- read.csv(paste(HPOG_DATA_PATH, 'hpog_ds6.csv', sep = ""))
hpog_ds15 <- read.csv(paste(HPOG_DATA_PATH, 'hpog_ds15.csv', sep = ""))
hpog_ds17 <- read.csv(paste(HPOG_DATA_PATH, 'hpog_ds17-participant-level.csv', sep = ""))
hpog_ds21 <- read.csv(paste(HPOG_DATA_PATH, 'hpog_ds21.csv', sep = ""))

# ----------------- CREATE HPOG 1.0 DATA FRAME -----------------
df1 <- left_join(hpog_ds5, hpog_ds4, by = c("RID"))
df2 <- left_join(df1, hpog_ds6, by = c("RID"))
df3 <- left_join(df2, hpog_ds15, by = c("PACE_ID" = "ABTSRBIID")) %>% 
  select(RID.x, PACE_ID, USERSITE.x, USERSITE.y, LICENSE36, EMPLOYEDATFOLLOWUP, everything())
df4 <- left_join(df3, hpog_ds15, by = c("RID.x" = "RID"))
df4$surveyrespondent_36 <- coalesce(df4$surveyrespondent_36.x, df4$surveyrespondent_36.y)
df4 <- df4 %>% 
  select(RID.x, PACE_ID, ABTSRBIID, USERSITE.x, USERSITE.y, USERSITE, LICENSE36.x, LICENSE36.y, EMPLOYEDATFOLLOWUP.x, EMPLOYEDATFOLLOWUP.y, everything())
# At this point you can drop usersite.y, usersite
df5 <- df4 %>% 
  select(-USERSITE.y, -USERSITE)

# merge license36.x and license36.y;
license36 <- coalesce(df5$LICENSE36.x, df5$LICENSE36.y)
df6 <- df5 %>% 
  select(-LICENSE36.x, -LICENSE36.y)
df6$LICENSE36 <- license36
df6 <- df6 %>% 
  select(RID.x, PACE_ID, ABTSRBIID, USERSITE.x, LICENSE36, EMPLOYEDATFOLLOWUP.x, EMPLOYEDATFOLLOWUP.y, everything())

# similarly merge employedatfolloup.x and employedatfollowup.y
employedatfollowup <- coalesce(df6$EMPLOYEDATFOLLOWUP.x, df6$EMPLOYEDATFOLLOWUP.y)
df7 <- df6 %>% 
  select(-EMPLOYEDATFOLLOWUP.x, -EMPLOYEDATFOLLOWUP.y)
df7$EMPLOYEDATFOLLOWUP <- employedatfollowup
df7 <- df7 %>% 
  select(RID.x, PACE_ID, ABTSRBIID, USERSITE.x, LICENSE36, EMPLOYEDATFOLLOWUP, everything())

# You probably can merge pace_id and ABTSRBIID; but check carefully
abtsrbiid <- coalesce(df7$PACE_ID, df7$ABTSRBIID)
df8 <- df7 %>% 
  select(-ABTSRBIID, -PACE_ID)
df8$ABTSRBIID <- abtsrbiid
df8 <- df8 %>% 
  select(RID.x, ABTSRBIID, USERSITE.x, LICENSE36, EMPLOYEDATFOLLOWUP, everything())

# merging hpog ds17
df9 <- left_join(df8, hpog_ds17, by = c("ABTSRBIID"))

# Remove RID.y
df10 <- df9 %>% 
  select(-RID.y)

# merge hpog ds21
df11 <- left_join(df10, hpog_ds21, by = c("ABTSRBIID"))

# Final HPOG 1.0 DataFrame
hpog_df <- df11

# ------- Write extracted info. to CSV ------- 
folder_path <- file.path(root, "read-HPOG1.0-output")

# Create the folder if it doesn't exist
if(!dir.exists(folder_path)) { 
  dir.create(folder_path)
}
write.csv(hpog_df, file.path(folder_path,"hpog1.0-joined-df.csv"), row.names = FALSE)

# Remove all data frames
rm(list = ls())
gc()