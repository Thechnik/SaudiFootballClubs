library(dplyr)

# Define the folder containing the CSV files
folder_path <- "./"

# List all CSV files in the folder
file_list <- list.files(path = folder_path, pattern = "SPL-\\d{4}-\\d{4}-FS\\.csv", full.names = TRUE)

# Function to extract the season from the file name
extract_season <- function(file_name) {
  matches <- regmatches(file_name, regexpr("\\d{4}-\\d{4}", file_name))
  return(matches)
}

# Initialize an empty list to store the data frames
all_data <- list()

# Loop through each file
for (file in file_list) {
  # Read the CSV file
  data <- read.csv(file, stringsAsFactors = FALSE)

  # Select only the required columns
  data <- data %>% select(matchNo, Date, Time, Team1, Team2, Score1, Score2)

  # Extract the season from the file name
  season <- extract_season(basename(file))

  # Add the season as a new column
  data$Season <- season

  # Append the data frame to the list
  all_data[[length(all_data) + 1]] <- data
}

# Combine all data frames into one
combined_data <- bind_rows(all_data)

# Save the combined data to a new CSV file
write.csv(combined_data, file = "combined_data.csv", row.names = FALSE)

cat("Combined data has been saved to 'combined_data.csv'\n")
