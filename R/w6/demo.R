setwd(dirname(rstudioapi::getActiveDocumentContext()[['path']]))
options(scipen=999)

# Load necessary packages
library(tidyverse)

# Reading the datasets
election_results <- read_csv("counties-election-results.csv")
gender_data <- read_csv("counties-gender.csv")

# Calculate the total number of votes per county
county_election_results <- election_results %>% 
  ## Because some states report in sub-counties, we must first group by FIPS...
  group_by(statePostal, fipsCode) %>% 
  ## And add them all up to get the county votes
  summarise(total_county_votes = sum(voteCount)) %>% 
  arrange(desc(total_county_votes))

# Calculate the total number of votes for each candidate per county
county_election_results <- election_results %>% 
  ## Because some states report in sub-counties, we must first group by FIPS...
  group_by(statePostal, fipsCode, last) %>% 
  ## And add them all up to get the county votes
  summarise(voteCount = sum(voteCount)) %>% 
  ungroup()

# But maybe we care less about the number of raw votes and would rather have the
# vote percentages. Since we summarized the subcounty votes, we need to recalculate
# the candidate shares. We can do that by grouping by the fips code and mutating
# a group level sum.
county_election_results <- county_election_results %>% 
  group_by(statePostal, fipsCode) %>% 
  mutate(votePct = voteCount / sum(voteCount)) %>% 
  ungroup()

# Now that we have the count and vote, let's add a column to determine the winner.
# There are a few ways to do this, but here we are once again grouping by fipsCode
# and then using array indexing and which.max to select the `last` value of the row
# with the max `voteCount` value.
county_election_results <- county_election_results %>% 
  group_by(statePostal, fipsCode) %>% 
  mutate(winner = last[which.max(voteCount)]) %>%
  ungroup()



# Let's try combining our election and gender data to determine the voter turnout
# of each county. 


# First, we can merge our election data and our voter data together by using left_join
merged <- left_join(gender_data, county_election_results, by=c("GEOID" = "fipsCode"))

# Note: When merging data, you always want to check to see if there are unjoined
# rows. By running the command below, we can see there are counties for which we
# have census data but no election data. 
merged %>%
  filter(if_any(everything(), is.na))

# In this particular case, it is due to 1) the census data including results for
# P.R. that aren't in our election results and 2) a mismatch on Alaska counties
# and D.C. Normally you'd want to resolve this, but for now we can just filter
# those records out by removing any rows that have NA values
merged <- filter(merged, !if_any(everything(), is.na))


# Next, we can calculate the total population per county
# by adding the male and female counts together.
merged <- mutate(merged, total_population = Male_Population + Female_Population)


# Then we can calculate the total number of votes per county using group_by()
# and sum()
merged <- merged %>% 
  group_by(GEOID) %>% 
  mutate(total_vote_count = sum(voteCount))

# Now we're ready to make a new column for turnout, dividing county vote by county population
merged <- merged %>% 
  mutate(turnout = total_vote_count / total_population)


# Sometimes we need to modify the structure of our data by making it "wider" or "taller".
# Currently, we have a row for every candidate's county results. If we wanted to just
# look at Trump and Harris votes and have a single row for each county, we could "spread"
# the data with pivot_wider().

widened <- merged %>% 
  filter(last %in% c("Harris", "Trump")) %>% 
  pivot_wider(
    # ID variables that are uniform across county rows
    id_cols = c(statePostal, GEOID, County, winner, turnout, total_population),
    # Get column names from...
    names_from = last,
    # Get values from...
    values_from = c(voteCount, votePct))

# Using ggplot2, we can visualize our data. Let's see if we can compare voter
# turnout vs how the county voted
widened %>% 
  # Calculate the vote margin by subtracting the D share from the R share so that
  # negative values indicates a D lean
  mutate(voteMargin = votePct_Trump - votePct_Harris ) %>% 
  ggplot(
    aes(x = voteMargin,
        y=turnout,
    )
  ) +
  geom_point()


# This gives a scatter plot, but it's hard to tell what is going on. We can use
# our other columns to assign additional properties like color and size to pack
# more information into our plot
widened %>% 
  mutate(voteMargin = votePct_Trump - votePct_Harris) %>% 
  ggplot(
    aes(
      x = voteMargin,
      y = turnout,
      color = winner,
      size = total_population
    )
  ) +
  geom_point(alpha = 0.25) +
  scale_color_manual(
    values = c("Trump" = "red", "Harris" = "blue"),
    name = "Winner"
  ) +
  labs(
    x = "Vote Margin (Trump - Harris)",
    y = "Turnout",
    title = "Vote Margin vs Turnout",
    size = "Total Population"
  ) +
  theme_minimal()




