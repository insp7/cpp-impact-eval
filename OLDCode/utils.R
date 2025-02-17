library(readr)
library(dplyr)
library(openxlsx)
library(rprojroot)
library(stringr)

# Checks whether x is binary
is_binary <- function(x) all(x %in% c(0, 1) | is.na(x))

# Calculates mean, min, max, standard deviation, n(Missing) & n(Non-missing) for x
calculate_stats <- function(x) {
  missing <- sum(is.na(x))
  non_missing <- sum(!is.na(x))
  total <- missing + non_missing
  result <- NA

  if(class(x) == "character") {
    # special case: for handling participant_id
    result <- paste("Missing: ", missing,
                    "Non-missing: ", non_missing)
  } else if(is_binary(x)) {
    # calculate stats for all binary variables
    frequency <- table(x)
    freq_string <- paste(names(frequency), frequency, sep = ": ", collapse = ", ")
    str <- paste("Missing: ", missing,
                 "Non-missing: ", non_missing)
    result <- paste(freq_string, str)
  } else {
    # calculate stats for all numeric variables
    min = min(x, na.rm = T)
    mean = mean(x, na.rm = T)
    max = max(x, na.rm = T)
    sd = sd(x, na.rm = T)
    stats_str <- paste("Min: ", min,
                       "Mean: ", mean,
                       "Max: ", max,
                       "SD: ", sd)
    missing <- sum(is.na(x))
    non_missing <- sum(!is.na(x))
    str <- paste("Missing: ", missing,
                 "Non-missing: ", non_missing)
    result <- paste(stats_str, str)
  }

  return (result)
}

# Parses numeric variables and returns statistics in markdown format
parse_numeric_variables <- function(str, var_name) {
  result <- NA
  missing <- str_extract(str, "(?<=Missing:\\s\\s)\\d+\\.?\\d*")
  non_missing <- str_extract(str, "(?<=Non-missing:\\s\\s)\\d+\\.?\\d*")
  total <- as.numeric(missing) + as.numeric(non_missing)

  if(str_equal(var_name, "participant_id")) {
    result <- paste0("### ", var_name, "\n\n",
                  "- N (Missing): ", missing, "\n",
                  "- N (Non-missing): ", non_missing, "\n",
                  "- N (Total): ", total, "\n\n")
  } else {
    min <- str_extract(str, "(?<=Min:\\s\\s)\\d+\\.?\\d*")
    mean <- str_extract(str, "(?<=Mean:\\s\\s)\\d+\\.?\\d*")
    max <- str_extract(str, "(?<=Max:\\s\\s)\\d+\\.?\\d*")
    sd <- str_extract(str, "(?<=SD:\\s\\s)\\d+\\.?\\d*")
    
    result <- paste0("### ", var_name, "\n\n",
                  "- N (Minimum): ", min, "\n",
                  "- N (Maximum): ", max, "\n",
                  "- N (Missing): ", missing, "\n",
                  "- N (Non-missing): ", non_missing, "\n",
                  "- N (Total): ", total, "\n",
                  "- Mean: ", mean, "\n",
                  "- Standard Deviation: ", sd, "\n\n")
  }

  return (result)
}

# Parses binary variables and returns statistics in markdown format
parse_binary_variables <- function(str, name) {
  freq_0 <- as.numeric(str_extract(str, "(?<=0:\\s)\\d+"))
  freq_1 <- as.numeric(str_extract(str, "(?<=1:\\s)\\d+"))
  missing <- as.numeric(str_extract(str, "(?<=Missing:\\s\\s)\\d+"))

  if(is.na(freq_0))
    freq_0 <- 0
  
  if(is.na(freq_1))
    freq_1 <- 0
  
  if(is.na(missing))
    missing <- 0
  
  total <- freq_0 + freq_1 + missing
  percent_0 <- (freq_0 / total) * 100
  percent_1 <- (freq_1 / total) * 100
  percent_missing <- (missing / total) * 100
  
  return(paste0("### ", name, "\n\n",
                "| Value | Label | Frequency | Percentage |\n",
                "|:-----:|:-----:|:---------:|:----------:|\n",
                "|   0   |  No   |   ", freq_0, "    |    ", sprintf("%.2f", percent_0), "%   |\n",
                "|   1   | Yes   |   ", freq_1, "    |    ", sprintf("%.2f", percent_1), "%   |\n",
                "|  NA   | Missing |   ", missing, "    |    ", sprintf("%.2f", percent_missing), "%   |\n",
                "N (Total): ", total, "\n\n"))
}
