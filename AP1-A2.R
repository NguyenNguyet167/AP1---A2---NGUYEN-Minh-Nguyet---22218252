#Read data
library(tidyverse)
library(scales)
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
    Result <- as.factor(Result),
    Stage <- as.factor(Stage),
    Country <- as.factor(Country),
    ExtraTime <- as.factor(ExtraTime)
  ) %>%
  # Convert penalties to numeric and replace NAs with 0
  mutate(
    HomePenalty <- as.numeric(HomePenalty),
    AwayPenalty <- as.numeric(AwayPenalty),
    HomePenalty <- replace_na(HomePenalty, 0),
    AwayPenalty <- replace_na(AwayPenalty, 0)
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

#QUESTION 2
# Histogram 1: By Stage
ggplot(matches_clean, aes(x = HomeTeamScore)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", color = "black") +
  facet_wrap(~Stage, scales = "free_y") +
  labs(title = "Home Team Score Distribution by Stage", x = "Home Team Score", y = "Count") +
  theme_minimal()

# Histogram 2: By Tournament Name
ggplot(matches_clean, aes(x = HomeTeamScore)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black") +
  facet_wrap(~TournamentName, scales = "free_y") +
  labs(title = "Home Team Score Distribution by Tournament", x = "Home Team Score", y = "Count") +
  theme_minimal()

# Grouping AwayTeamScore
matches_grouped <- matches_clean %>%
  mutate(AwayScoreGroup = case_when(
    AwayTeamScore <= 1 ~ "0-1 goals",
    AwayTeamScore >= 2 & AwayTeamScore <= 3 ~ "2-3 goals",
    AwayTeamScore >= 4 ~ "4 or more goals",
    TRUE ~ "Unknown"
  )) %>%
  # Lock the logical order of factors for plotting
  mutate(AwayScoreGroup = factor(AwayScoreGroup, levels = c("0-1 goals", "2-3 goals", "4 or more goals")))

# Test if the grouping logic worked correctly before plotting
table(matches_grouped$AwayScoreGroup)

# Histogram 3: By AwayTeamScore Group
ggplot(matches_grouped, aes(x = HomeTeamScore)) +
  geom_histogram(binwidth = 1, fill = "coral", color = "black") +
  facet_wrap(~AwayScoreGroup) +
  labs(title = "Home Team Score Distribution by Away Team Score Group", x = "Home Team Score", y = "Count") +
  theme_minimal()


#QUESTION 3
# Top 5 Home Teams
top_home_penalties <- penalty_matches %>%
  count(HomeTeamID, name = "NumberOfMatches") %>%
  arrange(desc(NumberOfMatches)) %>%
  slice_head(n = 5) %>% 
  left_join(teams, by = c("HomeTeamID" = "TeamID")) %>%
  select(TeamName, TeamCode, NumberOfMatches)

# Top 5 Away Teams
top_away_penalties <- penalty_matches %>%
  count(AwayTeamID, name = "NumberOfMatches") %>%
  arrange(desc(NumberOfMatches)) %>%
  slice_head(n = 5) %>%
  left_join(teams, by = c("AwayTeamID" = "TeamID")) %>%
  select(TeamName, TeamCode, NumberOfMatches)

# Print tables
print("--- Top 5 Home Teams ---")
print(top_home_penalties)
print("--- Top 5 Away Teams ---")
print(top_away_penalties)


#QUESTION 4
# Filter out matches where Result is NA for accurate modeling/visualization
analysis_data <- matches_clean %>%
  filter(!is.na(Result))

# Verify that NAs are removed and the structure is ready for plotting 
str(analysis_data %>% select(ExtraTime, Stage, Result))


# Visualizing Factor 1: ExtraTime vs Result
ggplot(analysis_data, aes(x = ExtraTime, fill = Result)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Match Outcome Proportion by Extra Time",
    x = "Did the match go to Extra Time?",
    y = "Proportion of Match Outcomes"
  ) +
  theme_minimal()

# Visualizing Factor 2: Stage vs Result
ggplot(analysis_data, aes(x = Stage, fill = Result)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() + # Flip x and y axis for better readability of Stage names
  labs(
    title = "Match Outcome Proportion by Competition Stage",
    x = "Competition Stage",
    y = "Proportion of Match Outcomes"
  ) +
  theme_minimal()

