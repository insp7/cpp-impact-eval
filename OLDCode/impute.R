library(readr)
library(rprojroot)
library(tidyverse)
library(mice)

# Find the root of the current project
root <- find_root(is_rstudio_project)

source("utils.R")

all_hpog_imp_set <- read.csv("AF2--ALL-HPOG.csv")
pace_only_imp_set <- read.csv("AF2--PACE-ONLY-DataFrame.csv")
year_up_imp_set <- read.csv("AF2--YEAR-UP-DataFrame.csv")


# HPOG Imputation:
all_hpog_imp <- mice(all_hpog_imp_set, method = 'pmm', m = 1, maxit = 40, seed = 123)
imputed_hpog_df <- complete(all_hpog_imp)

# PACE Imputation:
imp <- mice(pace_only_imp_set, method = 'pmm', m = 1, maxit = 40, seed = 123)
imputed_pace_only_df <- complete(imp)

# YearUp Imputation:
year_up_imp <- mice(year_up_imp_set, method = 'pmm', m = 1, maxit = 40, seed = 123)
imputed_year_up_df <- complete(year_up_imp)
