# Load necessary libraries
library(tidyverse)
library(factoextra)

# Load data
data <- jsonlite::fromJSON("C:\\Users\\Thech\\OneDrive\\الماجستير\\S2\\Data Analytics نال 6114 تحليل البيانات\\Assignments and Projects\\Project\\Result Table Idea\\combined_data.json")

# Convert necessary columns to numeric
data <- data %>%
  mutate(across(c(games_played, wins, draws, losses, goals_for, goals_against, points), as.numeric))

# Select features for clustering
clustering_data <- data %>%
  select(games_played, wins, draws, losses, goals_for, goals_against, points)

# Scale the data for K-Means
scaled_data <- scale(clustering_data)

# Determine optimal k using Elbow Method
set.seed(42)
wss <- sapply(1:10, function(k) {
  kmeans(scaled_data, centers = k, nstart = 10)$tot.withinss
})

# Plot the Elbow Curve
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal k")

# Apply K-Means clustering with k = 3 (or based on the elbow result)
set.seed(42)
kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)

# Add cluster labels to the original dataset
data$cluster <- kmeans_result$cluster

# View the clustering results
print(data %>% select(team, games_played, wins, draws, losses, goals_for, goals_against, points, cluster))

# Visualize the clusters
fviz_cluster(kmeans_result, data = scaled_data, 
             geom = "point", 
             ellipse.type = "convex",
             main = "K-Means Clustering Results")


# Split the data into three clusters based on the cluster labels
cluster_1 <- data %>% filter(cluster == 1)
cluster_2 <- data %>% filter(cluster == 2)
cluster_3 <- data %>% filter(cluster == 3)

# View each cluster
print("Cluster 1:")
print(cluster_1 %>% select(team, games_played, wins, draws, losses, goals_for, goals_against, points))

print("Cluster 2:")
print(cluster_2 %>% select(team, games_played, wins, draws, losses, goals_for, goals_against, points))

print("Cluster 3:")
print(cluster_3 %>% select(team, games_played, wins, draws, losses, goals_for, goals_against, points))

# Optionally save the clusters as separate CSV files for further analysis
write.csv(cluster_1, "cluster_1.csv", row.names = FALSE)
write.csv(cluster_2, "cluster_2.csv", row.names = FALSE)
write.csv(cluster_3, "cluster_3.csv", row.names = FALSE)

# Summarize each cluster
cluster_summary <- data %>%
  group_by(cluster) %>%
  summarise(
    avg_wins = mean(wins),
    avg_draws = mean(draws),
    avg_losses = mean(losses),
    avg_goals_for = mean(goals_for),
    avg_goals_against = mean(goals_against),
    avg_points = mean(points),
    count = n()
  )

print(cluster_summary)

