setwd(dirname(rstudioapi::getActiveDocumentContext()[['path']]))

## Load necessary packages
library(tidyverse)

## Reading the datasets
election_results <- read_csv("counties-election-results.csv")
gender_data <- read_csv("counties-gender.csv")


total_votes <- election_results %>% 
  group_by(statePostal, fipsCode, reportingUnitName) %>% 
  summarise(total_county_votes = sum(voteCount)) %>% 
  arrange(desc(total_votes))


total_pop <- gender_data %>%
  mutate(total_county_pop = Male_Population + Female_Population) %>% 
  select(-Male_Population, -Female_Population) %>% 
  rename(fipsCode = GEOID)


merged <- left_join(total_votes, total_pop)

share <- merged %>% 
  mutate(share = (total_county_votes / total_county_pop) * 100)
  
