setwd(dirname(rstudioapi::getActiveDocumentContext()[['path']]))

## Load necessary packages
library(tidyverse)

## Reading the datasets
election_results <- read_csv("counties-election-results.csv")
gender_data <- read_csv("counties-gender.csv")





