root <- find_root(is_rstudio_project)
source("utils.R")

data <- read.csv('Analysis-DataFrame.csv')

study_hpog_equals_1 <- data %>% 
  filter(study_hpog == 1)

study_pace_equals_1 <- data %>% 
  filter(study_pace == 1)

study_year_up_equals_1 <- data %>%
  filter(program_id == 9)


# Fetch summary for Code Book
summary_stats <- sapply(study_year_up_equals_1, calculate_stats)
print(summary_stats)
