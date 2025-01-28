# Load necessary libraries
library(dplyr)
library(ggplot2)
library(jsonlite)

# Load the data
league_data <- fromJSON("C:\\Users\\aalhm\\OneDrive\\Desktop\\get_data\\combined_data.json", simplifyVector = TRUE)

# Prepare the data for regression
league_data <- league_data %>%
  mutate(
    goals_for = as.numeric(goals_for),
    goals_against = as.numeric(goals_against)
  ) %>%
  group_by(team) %>%
  arrange(season) %>%
  mutate(
    goal_difference = goals_for - goals_against
  )

# Fit the linear regression model
linear_model <- lm(points ~ goals_for + goals_against, data = league_data)
# Generate the linear regression model
linear_model <- lm(points ~ goals_for, data = league_data)

# Predict values for a range of goals_for
prediction_data <- data.frame(goals_for = seq(min(league_data$goals_for, na.rm = TRUE), 
                                              max(league_data$goals_for, na.rm = TRUE), 
                                              length.out = 100))
prediction_data$predicted_points <- predict(linear_model, newdata = prediction_data)

# Plot with a continuous regression line
library(ggplot2)

ggplot(league_data, aes(x = goals_for, y = points)) +
  geom_point(color = "blue", alpha = 0.6) + # Actual data points
  geom_line(data = prediction_data, aes(x = goals_for, y = predicted_points), color = "red", size = 1) + # Regression line
  labs(
    title = "Linear Regression: Goals For vs. Points",
    x = "Goals For",
    y = "Points"
  ) +
  theme_minimal()