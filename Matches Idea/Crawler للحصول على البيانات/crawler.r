library(rvest)
library(dplyr)
library(jsonlite)

# Function to scrape match data for a single season
scrape_season_data <- function(season_url) {
  # Read the HTML of the page
  page <- read_html(season_url)

  # Extract data using the CSS selectors provided
  hometeams_scores <- page %>% 
    html_nodes(".match_score .hometeam") %>% 
    html_text(trim = TRUE)

  awayteams_scores <- page %>% 
    html_nodes(".match_score .awayteam") %>% 
    html_text(trim = TRUE)

  hometeams <- page %>% 
    html_nodes(".team.hometeam .the_team") %>% 
    html_text(trim = TRUE)

  awayteams <- page %>% 
    html_nodes(".team.awayteam .the_team") %>% 
    html_text(trim = TRUE)

  times <- page %>% 
    html_nodes(".match_time .the_otime") %>% 
    html_text(trim = TRUE)

  # Combine data into a data frame
  match_data <- data.frame(
    home_team_score = hometeams_scores,
    away_team_score = awayteams_scores,
    home_team_name = hometeams,
    away_team_name = awayteams,
    time = times,
    stringsAsFactors = FALSE
  )

  return(match_data)
}

# Base URL and seasons to scrape
base_url <- "https://jdwel.com/"
seasons <- c("2018-2019", "2019-2020", "2020-2021", "2021-2022", "2022-2023", "2023-2024")

# Initialize an empty list to store data for all seasons
all_seasons_data <- list()

# Loop through each season and scrape data
for (season in seasons) {
  season_url <- paste0(base_url, season, "-saudi-league-fixtures/")
  cat("Scraping data for season:", season, "from URL:", season_url, "\n")
  
  season_data <- tryCatch({
    scrape_season_data(season_url)
  }, error = function(e) {
    cat("Error scraping", season, ":", e$message, "\n")
    NULL
  })

  if (!is.null(season_data)) {
    all_seasons_data[[season]] <- season_data
  }
}

# Combine all seasons into a single data frame
final_data <- bind_rows(all_seasons_data, .id = "season")

# Save the data to a CSV file
write.csv(final_data, "saudi_league_matches_2018_2024.csv", row.names = FALSE)

# Optionally save as JSON
write_json(final_data, "saudi_league_matches_2018_2024.json", pretty = TRUE)

cat("Scraping complete. Data saved to CSV and JSON.")