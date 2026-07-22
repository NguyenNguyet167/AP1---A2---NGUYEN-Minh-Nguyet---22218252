#Read data
library(tidyverse)
matches_raw <- read_csv("Matches.csv")
stadiums <- read_csv("Stadiums.csv")
teams <- read_csv("Teams.csv")
tournaments <- read_csv("Tournaments.csv")

#Merge dataset
matches_joined <- matches_raw %>%
  left_join(stadiums, by = "StadiumID") %>%
  left_join(tournaments, by = "TournamentID")

#test
glimpse(matches_joined)


#QUESTION 1
matches_clean <- matches_joined %>%
  # Replace "?" with NA in all character columns
  mutate(across(where(is.character), ~ na_if(.x, "?"))) %>%
  # Convert specific columns to factors
  mutate(
    Result = as.factor(Result),
    Stage = as.factor(Stage),
    Country = as.factor(Country),
    ExtraTime = as.factor(ExtraTime)
  ) %>%
  # Convert penalties to numeric and replace NAs with 0
  mutate(
    HomePenalty = as.numeric(HomePenalty),
    AwayPenalty = as.numeric(AwayPenalty),
    HomePenalty = replace_na(HomePenalty, 0),
    AwayPenalty = replace_na(AwayPenalty, 0)
  )
  #Test
summary(matches_clean %>% select(Result, Stage, Country, ExtraTime, HomePenalty))

  # Visualization for Q1
penalty_matches <- matches_clean %>%
  # Filter matches decided by penalty
  filter(PenaltyShootout == "TRUE" | PenaltyShootout == TRUE)

ggplot(penalty_matches, aes(x = as.factor(HomePenalty))) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Home Team Penalty Goals",
    x = "Number of Penalty Goals (Home Team)",
    y = "Frequency"
  ) +
  theme_minimal()


