library(rvest)
library(jsonlite)
library(dplyr)

# Function to fetch season pages and save HTML locally
fetch_season_html <- function(base_url, output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  # Launch browser session with RSelenium or similar
  browser <- RSelenium::remoteDriver(browserName = "chrome")
  browser$open()

  # Navigate to the base URL
  browser$navigate(base_url)

  # Get all season values from the dropdown
  seasons <- browser$executeScript(
    "return Array.from(document.querySelector('select.seasonpicker').options).map(option => ({ value: option.value, text: option.textContent.trim() }));"
  )

  for (season in seasons) {
    cat("Fetching HTML for season:", season$text, "\n")

    # Build the URL for the season
    season_url <- paste0("https://www.kooora.com/?c=", season$value)
    browser$navigate(season_url)

    # Wait for the page to load
    Sys.sleep(3) # Adjust if necessary

    # Get the page source
    page_source <- browser$getPageSource()[[1]]

    # Save the HTML to a file
    html_file_path <- file.path(output_dir, paste0(gsub("/", "_", season$text), ".html"))
    write(page_source, file = html_file_path)

    cat("Saved HTML for", season$text, "to", html_file_path, "\n")
  }

  browser$close()
}

# Function to extract league table data
extract_league_table <- function(html_file_path) {
  # Read the HTML file
  page <- read_html(html_file_path)

  # Extract table columns using CSS selectors
  rank <- page %>% html_nodes(".s_t:nth-child(1)") %>% html_text(trim = TRUE)
  team <- page %>% html_nodes(".s_t:nth-child(2) a.team") %>% html_text(trim = TRUE)
  games_played <- page %>% html_nodes(".s_t:nth-child(4)") %>% html_text(trim = TRUE)
  wins <- page %>% html_nodes(".s_t:nth-child(5)") %>% html_text(trim = TRUE)
  draws <- page %>% html_nodes(".s_t:nth-child(6)") %>% html_text(trim = TRUE)
  losses <- page %>% html_nodes(".s_t:nth-child(7)") %>% html_text(trim = TRUE)
  goals_for <- page %>% html_nodes(".s_t:nth-child(8)") %>% html_text(trim = TRUE)
  goals_against <- page %>% html_nodes(".s_t:nth-child(9)") %>% html_text(trim = TRUE)
  goal_difference <- page %>% html_nodes(".s_t:nth-child(10)") %>% html_text(trim = TRUE)
  points <- page %>% html_nodes(".s_t:nth-child(11)") %>% html_text(trim = TRUE)

  # Combine the extracted data into a data frame
  league_table <- data.frame(
    rank = rank,
    team = team,
    games_played = games_played,
    wins = wins,
    draws = draws,
    losses = losses,
    goals_for = goals_for,
    goals_against = goals_against,
    goal_difference = goal_difference,
    points = points,
    stringsAsFactors = FALSE
  )

  return(league_table)
}

# Function to save league table as JSON
save_as_json <- function(data, file_path) {
  write_json(data, file_path, pretty = TRUE)
}

base_url <- "https://www.kooora.com/?c=25961"
html_output_dir <- "./html_files"
json_output_dir <- "./json_files"

fetch_season_html(base_url, html_output_dir)

html_files <- list.files(path = html_output_dir, pattern = "*.html", full.names = TRUE)

if (!dir.exists(json_output_dir)) {
  dir.create(json_output_dir)
}

for (html_file in html_files) {
  cat("Processing file:", html_file, "\n")
  league_table <- extract_league_table(html_file)

  # Extract season name from file name
  season_name <- tools::file_path_sans_ext(basename(html_file))
  json_file_path <- file.path(json_output_dir, paste0(season_name, ".json"))

  save_as_json(league_table, json_file_path)
  cat("Saved JSON for", season_name, "to", json_file_path, "\n")
}

cat("All files processed and saved as JSON.")
