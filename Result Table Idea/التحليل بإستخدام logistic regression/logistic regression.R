# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load the dataset
file_path <- "C:\\Users\\aalhm\\Downloads\\OneDrive_1_1-25-2025\\combined_data.csv"
df <- read_csv(file_path)

# Check structure of the data
head(df)

# Handle missing values
df <- df %>%
  filter(!is.na(Score1) & !is.na(Score2))

# Define Match Outcome:
# 1 = Win for Team1, 0 = No Win (Draw or Loss)
df <- df %>%
  mutate(Win = ifelse(Score1 > Score2, 1, 0))

# Logistic Regression Model (Win ~ Goals Scored + Goals Conceded)
model <- glm(Win ~ Score1 + Score2, data = df, family = binomial)

# Summary of the Model
summary(model)

# Extract key coefficients
coef <- summary(model)$coefficients
print(coef)

# Visualizing predicted probabilities
df$predicted <- predict(model, type = "response")

# Enhanced Visualization
ggplot(df, aes(x = Score1, y = predicted)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "red", size = 1.2) +  # Adjust line thickness
  scale_x_continuous(breaks = seq(0, max(df$Score1), by = 1)) +  # Add better axis breaks
  labs(title = "Logistic Regression: Win Probability vs. Goals Scored",
       x = "Goals Scored by Team",
       y = "Predicted Win Probability") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))
