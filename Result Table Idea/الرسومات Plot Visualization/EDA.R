# Load required libraries
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the dataset
data <- fromJSON("C:\\Users\\aalhm\\OneDrive\\Desktop\\get_data\\combined_data.json")

# View the structure of the data
str(data)

# Check for missing values
colSums(is.na(data))

data <- data %>%
  mutate(
    games_played = as.numeric(games_played),
    wins = as.numeric(wins),
    draws = as.numeric(draws),
    losses = as.numeric(losses),
    goals_for = as.numeric(goals_for),
    goals_against = as.numeric(goals_against),
    goal_difference = as.numeric(goal_difference),
    points = as.numeric(points),
    starting_year = as.numeric(substr(season, 1, 4))
  )

# Summary statistics
summary(data)


# Count championships for each team
championships <- data %>%
  filter(rank == 1) %>%
  count(team, sort = TRUE)

# Display top teams
print(championships)


# Calculate average points per team
average_points <- data %>%
  group_by(team) %>%
  summarise(avg_points = mean(points, na.rm = TRUE)) %>%
  arrange(desc(avg_points))

# Display top teams
print(head(average_points, 10))




# Get top 5 teams by average points
top_teams <- average_points %>% top_n(5, avg_points) %>% pull(team)

# Filter data for these teams
top_teams_data <- data %>% filter(team %in% top_teams)


# Select relevant columns for correlation
cor_data <- data %>%
  select(points, goals_for, goals_against, goal_difference)

# Compute the correlation matrix
cor_matrix <- cor(cor_data, use = "complete.obs")
print(cor_matrix)


# Total goals scored by each team
total_goals <- data %>%
  group_by(team) %>%
  summarise(goals_scored = sum(goals_for, na.rm = TRUE)) %>%
  arrange(desc(goals_scored))

print(head(total_goals, 10))

# Total goals scored on each team
total_goals <- data %>%
  group_by(team) %>%
  summarise(goals_against = sum(goals_against, na.rm = TRUE)) %>%
  arrange(desc(goals_against))

print(head(total_goals, 10))





# Aggregate goals scored by season
goals_scored_by_season <- data %>%
  group_by(starting_year) %>%
  summarise(
    total_goals_scored = sum(goals_for, na.rm = TRUE)
  )

# Plot total goals scored over seasons
ggplot(goals_scored_by_season, aes(x = starting_year, y = total_goals_scored)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Total Goals Scored per Season",
    x = "Starting Year",
    y = "Total Goals Scored"
  ) +
  theme_minimal()


# Scatterplot of goals scored vs. points
ggplot(data, aes(x = goals_for, y = points)) +
  geom_point(color = "darkorange", alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  labs(
    title = "Scatterplot of Goals Scored vs. Points",
    x = "Goals Scored",
    y = "Points"
  ) +
  theme_minimal()


# Aggregate total wins, losses, and draws
totals <- data %>%
  summarise(
    total_wins = sum(wins, na.rm = TRUE),
    total_losses = sum(losses, na.rm = TRUE),
    total_draws = sum(draws, na.rm = TRUE)
  )

# Transform the data into a format suitable for a pie chart
totals_long <- totals %>%
  pivot_longer(cols = everything(), names_to = "result", values_to = "count")

# View the data
print(totals_long)

# Calculate percentages
totals_long <- totals_long %>%
  mutate(percentage = round((count / sum(count)) * 100, 1))  # Add percentages column

# View the updated data
print(totals_long)

# Pie chart with percentages and custom colors
ggplot(totals_long, aes(x = "", y = count, fill = result)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5), size = 5, color = "white") +  # Add percentage labels
  labs(
    title = "Proportion of Wins, Losses, and Draws",
    x = NULL,
    y = NULL,
    fill = "Result"
  ) +
  theme_void() +  # Clean theme
  scale_fill_manual(
    values = c(
      "total_wins" = "#1f77b4",  # Blue for wins
      "total_losses" = "#ff7f0e",  # Orange for losses
      "total_draws" = "#2ca02c"   # Green for draws
    )
  )

