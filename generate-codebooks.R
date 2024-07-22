library(readr)
library(dplyr)
library(openxlsx)
library(rprojroot)

# Find the root of the current project
root <- find_root(is_rstudio_project)

source("utils.R")

# Generate Markdown Code for HPOG CodeBook
lines <- readLines('YEAR_UP_36.txt') # Input summary statistics file path to generate markdown
output <- ""
var_name <- ""
for (line in lines) {
  if(!grepl("Non-missing", line)) {
    var_name <- trimws(line)
  } else {
    line <- trimws(line)
    if(grepl("Mean:", line)) {
      output <- paste(output, parse_numeric_variables(line, var_name), sep = "")
    } else {
      if(str_equal(var_name, "participant_id")) {
        output <- paste(output, parse_numeric_variables(line, var_name), sep = "")  
      } else
        output <- paste(output, parse_binary_variables(line, var_name), sep = "")
    }
  }
}

sink("YEAR_UP_36_month_survey_respondents.txt")
cat(output)
sink()
