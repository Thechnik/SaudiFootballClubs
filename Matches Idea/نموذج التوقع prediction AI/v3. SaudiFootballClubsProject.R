# ------------------------------------------------------------
# 1) Load required libraries
# ------------------------------------------------------------
library(dplyr)        # For data manipulation
library(tidyr)        # For data cleaning
library(randomForest) # For Random Forest model
library(nnet)         # For multinomial logistic regression
library(caret)        # For model evaluation
library(lubridate)    # For flexible date handling

# ------------------------------------------------------------
# 2) Specify the folder path containing CSV files
# ------------------------------------------------------------
folder_path <- "C:/Users/Thech/OneDrive/الماجستير/S2/Data Analytics نال 6114 تحليل البيانات/Assignments and Projects/Project/Code"

# ------------------------------------------------------------
# 3) List all CSV files
# ------------------------------------------------------------
files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# ------------------------------------------------------------
# 4) Define a helper function to check required columns
# ------------------------------------------------------------
required_cols_data <- c("Team1", "Team2", "Score1", "Score2", "Date")

check_required_columns <- function(df, required_cols) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    warning(paste("Missing columns:", paste(missing_cols, collapse = ", "), "- Filling with NA"))
    df[missing_cols] <- NA
  }
  return(df)
}

# ------------------------------------------------------------
# 5) Read and clean each CSV file
# ------------------------------------------------------------
data_list <- lapply(files, function(file) {
  # Read the CSV file
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # Check for required columns
  df <- check_required_columns(df, required_cols_data)
  if (is.null(df)) {
    return(NULL)
  }
  
  # Remove the 'Round' column if it exists
  if ("Round" %in% names(df)) {
    df <- df %>% select(-Round)
  }
  
  # Trim spaces in the Date column
  df$Date <- trimws(df$Date)
  
  # Convert date from multiple formats to R Date
  df$Date <- parse_date_time(df$Date, orders = c("dmy", "mdy", "ymd"))
  
  return(df)
})

# ------------------------------------------------------------
# 6) Remove NULL elements (invalid CSV files)
# ------------------------------------------------------------
data_list <- Filter(Negate(is.null), data_list)

# If no valid files remain, stop
if (length(data_list) == 0) {
  stop("No valid CSV file found (missing required columns)!")
}

# ------------------------------------------------------------
# 7) Combine all data frames into one
# ------------------------------------------------------------
data <- bind_rows(data_list)

# ------------------------------------------------------------
# 8) Filter out rows with missing dates and sort by date
# ------------------------------------------------------------
data <- data %>%
  filter(!is.na(Date)) %>%
  arrange(Date)

# If all rows are removed, stop
if (nrow(data) == 0) {
  stop("After cleaning the Date column, no valid rows remain. Check date formats or file contents.")
}

# ------------------------------------------------------------
# 9) Add derived features and target variable
# ------------------------------------------------------------
data <- data %>%
  mutate(
    goal_diff = abs(Score1 - Score2), # Absolute difference in scores
    result = case_when(
      Score1 > Score2 ~ "Win",
      Score1 < Score2 ~ "Loss",
      TRUE ~ "Draw"
    )
  )

# Convert result to factor for classification
data$result <- as.factor(data$result)

# ------------------------------------------------------------
# 10) Split data into training (80%) and testing (20%) sets (time-based split)
# ------------------------------------------------------------
train_data <- data[1:(0.8 * nrow(data)), ]
test_data <- data[(0.8 * nrow(data) + 1):nrow(data), ]

# ------------------------------------------------------------
# 11) Remove rows with missing values from training data
# ------------------------------------------------------------
train_data <- train_data %>%
  filter(
    !is.na(Team1), !is.na(Team2),
    !is.na(Score1), !is.na(Score2),
    !is.na(Date)
  )

if (nrow(train_data) == 0) {
  stop("Training data is empty after removing rows with missing values!")
}

# ------------------------------------------------------------
# 12) Train Random Forest model (classification)
# ------------------------------------------------------------
set.seed(123)
rf_model <- randomForest(
  result ~ goal_diff,
  data = train_data,
  ntree = 500 # Increase the number of trees
)

# Predict on the test set
rf_predictions <- predict(rf_model, test_data)
conf_matrix_rf <- confusionMatrix(rf_predictions, test_data$result)
print(conf_matrix_rf)

# ------------------------------------------------------------
# 13) Train Multinomial Logistic Regression model
# ------------------------------------------------------------
log_reg_model <- multinom(
  result ~ goal_diff,
  data = train_data
)

# Predict on the test set
log_reg_predictions <- predict(log_reg_model, test_data)
conf_matrix_log_reg <- confusionMatrix(log_reg_predictions, test_data$result)
print(conf_matrix_log_reg)

# ------------------------------------------------------------
# 14) Define a function to predict the match outcome between two teams
# ------------------------------------------------------------
predict_match <- function(team1, team2) {
  # Check if teams exist in the dataset
  if (!(team1 %in% unique(data$Team1)) || !(team2 %in% unique(data$Team2))) {
    stop("One or both team names are not valid.")
  }
  
  # Prepare input data for prediction
  input_data <- data.frame(
    goal_diff = abs(team1_stats$avg_score - team2_stats$avg_score)
  )
  
  # Predict using Random Forest
  rf_result <- predict(rf_model, input_data)
  # Predict using Multinomial Logistic Regression
  log_reg_result <- predict(log_reg_model, input_data)
  
  return(list(Random_Forest = rf_result, Logistic_Regression = log_reg_result))
}
