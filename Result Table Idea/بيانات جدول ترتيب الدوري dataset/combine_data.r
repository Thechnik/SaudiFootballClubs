library(jsonlite)

# Set the directory containing the JSON files
json_dir <- "./"  # Replace with the actual path

# Generate the list of JSON file names by season
years <- 1974:2024  # Adjust the range as needed
json_files <- sprintf("%s/kooora_%d_%d.json", json_dir, years, years + 1)

# Initialize an empty list to store combined data
combined_data <- list()

# Loop through each generated file name and process it
for (file in json_files) {
  cat("Reading file:", file, "\n")
  
  # Check if the file exists before reading
  if (file.exists(file)) {
    # Read the JSON file
    json_data <- tryCatch({
      fromJSON(file)
    }, error = function(e) {
      cat("Error reading file:", file, " - ", e$message, "\n")
      return(NULL)
    })
    
    # Only proceed if json_data is valid
    if (!is.null(json_data)) {
      # Extract the season year from the file name
      season <- sub("kooora_|\\.json", "", basename(file))
      
      # Add the season year to each record in the "teams" list
      if ("teams" %in% names(json_data)) {
        teams <- json_data$teams
        if (is.data.frame(teams)) {
          teams$season <- season
          combined_data <- rbind(combined_data, teams)
        } else {
          cat("Warning: 'teams' is not a data.frame in file:", file, "\n")
        }
      } else {
        cat("Warning: No 'teams' key in file:", file, "\n")
      }
    }
  } else {
    cat("File does not exist:", file, "\n")
  }
}

# Convert the combined data to JSON
output_json <- toJSON(combined_data, pretty = TRUE, auto_unbox = TRUE)

# Write the combined JSON to a file
output_file <- "combined_saudi_league.json"
write(output_json, output_file)

# Print the length of the combined data list
cat("Number of records in the combined list:", length(combined_data), "\n")

cat("Combined JSON file has been saved as:", output_file, "\n")
