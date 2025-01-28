library(dplyr)
library(ggplot2)
library(summarytools)
library(tidyr)


# Load the dataset
file_path <- "C:\\Users\\aalhm\\Downloads\\OneDrive_1_1-25-2025\\combined_data.csv"
data <- read.csv(file_path, stringsAsFactors = FALSE)


# Combine Team1 and Team2 into a unified format
unified_data <- data %>%
  mutate(
    Team_A = pmin(Team1, Team2),
    Team_B = pmax(Team1, Team2),
    Score_A = ifelse(Team1 == Team_A, Score1, Score2),
    Score_B = ifelse(Team1 == Team_A, Score2, Score1)
  ) %>%
  select(Team_A, Team_B, Score_A, Score_B, Date, Season)

summary(unified_data)



# Calculate the number of matches per season
matches_per_season <- unified_data %>%
  group_by(Season) %>%
  summarise(Number_of_Matches = n())

# Find the maximum and minimum seasons
max_season <- matches_per_season %>% filter(Number_of_Matches == max(Number_of_Matches))
min_season <- matches_per_season %>% filter(Number_of_Matches == min(Number_of_Matches))

# Display results
print("Matches per Season:")
print(matches_per_season)

print("Average Number of Matches per Season:")
print(mean(matches_per_season$Number_of_Matches))

print("Season with Maximum Matches:")
print(max_season)

print("Season with Minimum Matches:")
print(min_season)

# 1. Which team won the most against others
wins_data <- unified_data %>%
  mutate(
    Winner = case_when(
      Score_A > Score_B ~ Team_A,
      Score_B > Score_A ~ Team_B,
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Winner)) %>%
  group_by(Winner, Team_A, Team_B) %>%
  summarise(Wins = n(), .groups = "drop") %>%
  arrange(desc(Wins))

most_wins <- wins_data %>%
  group_by(Winner) %>%
  summarise(Total_Wins = sum(Wins)) %>%
  arrange(desc(Total_Wins))

cat("\n--- Teams with the Most Wins Against Others ---\n")
print(most_wins)

# 2. Which team scored the most goals against another specific team
goals_scored_against <- unified_data %>%
  group_by(Team_A, Team_B) %>%
  summarise(
    Total_Score_A = sum(Score_A, na.rm = TRUE),
    Total_Score_B = sum(Score_B, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Most_Scored_Against = ifelse(Total_Score_A > Total_Score_B, Team_A, Team_B)
  ) %>%
  arrange(desc(Total_Score_A), desc(Total_Score_B))

cat("\n--- Teams Scoring the Most Goals Against Specific Teams ---\n")
print(goals_scored_against)

# 3. Visualization of Most Wins
ggplot(most_wins, aes(x = reorder(Winner, -Total_Wins), y = Total_Wins, fill = Winner)) +
  geom_bar(stat = "identity") +
  labs(title = "Teams with the Most Wins", x = "Team", y = "Total Wins") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Visualization of Most Goals Scored Against Specific Teams
goals_scored_against_top <- goals_scored_against %>%
  top_n(10, wt = Total_Score_A + Total_Score_B)

ggplot(goals_scored_against_top, aes(x = reorder(paste(Team_A, "vs", Team_B), -(Total_Score_A + Total_Score_B)), y = Total_Score_A + Total_Score_B, fill = Team_A)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Goal-Scoring Matchups", x = "Matchup", y = "Total Goals Scored") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Focus on which team scored the most goals against another
goals_scored_against_top <- goals_scored_against %>%
  arrange(desc(Total_Score_A)) %>%
  top_n(10, wt = Total_Score_A)

ggplot(goals_scored_against_top, aes(x = reorder(paste(Team_A, "vs", Team_B), -Total_Score_A), y = Total_Score_A, fill = Team_A)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Teams Scoring the Most Goals Against Others", x = "Matchup", y = "Total Goals Scored by Team_A") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("Analysis complete. Results saved as 'most_wins.csv' and 'goals_scored_against.csv'.\n")

# Negative Draws (matches ending 0-0)
negative_draws <- unified_data %>%
  filter(Score_A == 0 & Score_B == 0) %>%
  group_by(Team_A, Team_B) %>%
  summarise(Negative_Draws = n(), .groups = "drop") %>%
  arrange(desc(Negative_Draws))

cat("\n--- Teams with the Most Negative Draws (0-0 Matches) ---\n")
print(negative_draws)

# Positive Draws (matches ending in 1-1 or higher)
positive_draws <- unified_data %>%
  filter(Score_A == Score_B & Score_A > 0) %>%
  group_by(Team_A, Team_B) %>%
  summarise(Positive_Draws = n(), .groups = "drop") %>%
  arrange(desc(Positive_Draws))

cat("\n--- Teams with the Most Positive Draws (1-1 or Higher) ---\n")
print(positive_draws)

# 5. Average Goals per Match per Team
avg_goals_per_match <- unified_data %>%
  gather(key = "Role", value = "Team", Team_A, Team_B) %>%
  gather(key = "Score_Role", value = "Score", Score_A, Score_B) %>%
  filter((Role == "Team_A" & Score_Role == "Score_A") | (Role == "Team_B" & Score_Role == "Score_B")) %>%
  group_by(Team) %>%
  summarise(Average_Goals = mean(Score, na.rm = TRUE), Matches_Played = n(), .groups = "drop") %>%
  arrange(desc(Average_Goals))

cat("\n--- Average Goals per Match per Team ---\n")
print(avg_goals_per_match)

