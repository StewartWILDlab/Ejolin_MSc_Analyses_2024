#script to count cameras/cluster, and distance between cameras/clusters

cameras_locations<-read.csv("Raw_Data/TDN_camera_site_locations.csv")

# Load necessary library
library(dplyr)

################################################################################
# Average number of cameras per cluster

# Group by location_site and count the number of locations per location_site
location_counts <- cameras_locations %>%
  group_by(location_site) %>%
  summarise(count = n())

# Calculate the average, SD, and range of the number of locations per location_site
avg_locations <- mean(location_counts$count)
sd_locations <- sd(location_counts$count)
range_locations <- range(location_counts$count)

# Print the results
cat("Average number of locations per location_site:", avg_locations, "\n")
cat("Standard Deviation of locations per location_site:", sd_locations, "\n")
cat("Range of locations per location_site: from", range_locations[1], "to", range_locations[2], "\n")
#This code will provide you with the average number of locations per location_site, the standard

################################################################################
#Distance between clusters

# Install and load necessary package
if (!require(geosphere)) {
  install.packages("geosphere")
}
library(geosphere)

# Extract the unique location_site coordinates using dplyr::select
unique_sites <- cameras_locations %>%
  dplyr::select(location_site, latitude_site, longitude_site) %>%
  distinct()

# Calculate the pairwise distances using geosphere::distm
dist_matrix <- geosphere::distm(unique_sites[, c("longitude_site", "latitude_site")])

# Convert the distance matrix to a vector of distances (in meters)
distances <- dist_matrix[upper.tri(dist_matrix, diag = FALSE)]

# Calculate the average, SD, and range of the distances
avg_distance <- mean(distances)
sd_distance <- sd(distances)
range_distance <- range(distances)

# Print the results
cat("Average distance between location_site values:", avg_distance, "meters\n")
cat("Standard Deviation of distances between location_site values:", sd_distance, "meters\n")
cat("Range of distances between location_site values: from", range_distance[1], "to", range_distance[2], "meters\n")

################################################################################
#Average distance between cameras across clusters

# Function to calculate pairwise distances within a group
calculate_distances <- function(df) {
  # Compute pairwise distances
  dist_matrix <- geosphere::distm(df[, c("longitude", "latitude")])
  # Extract the upper triangle of the distance matrix
  distances <- dist_matrix[upper.tri(dist_matrix, diag = FALSE)]
  return(distances)
}

# Group by location_site and calculate distances, min, and max distances
distance_stats <- cameras_locations %>%
  group_by(location_site) %>%
  summarise(
    distances = list(calculate_distances(cur_data())),
    min_distance = min(unlist(distances), na.rm = TRUE),
    max_distance = max(unlist(distances), na.rm = TRUE)
  ) %>%
  ungroup()

# Extract all distances into a single vector
all_distances <- unlist(distance_stats$distances)

# Calculate the overall average, SD, and range of all distances
avg_distance <- mean(all_distances, na.rm = TRUE)
sd_distance <- sd(all_distances, na.rm = TRUE)
range_distance <- range(all_distances, na.rm = TRUE)

# Calculate the average, SD, and range of minimum distances across all clusters
avg_min_distance <- mean(distance_stats$min_distance, na.rm = TRUE)
sd_min_distance <- sd(distance_stats$min_distance, na.rm = TRUE)
range_min_distance <- range(distance_stats$min_distance, na.rm = TRUE)

# Calculate the average, SD, and range of maximum distances across all clusters
avg_max_distance <- mean(distance_stats$max_distance, na.rm = TRUE)
sd_max_distance <- sd(distance_stats$max_distance, na.rm = TRUE)
range_max_distance <- range(distance_stats$max_distance, na.rm = TRUE)

# Print the results
cat("Average distance between cameras within clusters:", avg_distance, "meters\n")
cat("Standard Deviation of distances between cameras within clusters:", sd_distance, "meters\n")
cat("Range of distances between cameras within clusters: from", range_distance[1], "to", range_distance[2], "meters\n")
cat("Average minimum distance between cameras within clusters:", avg_min_distance, "meters\n")
cat("Standard Deviation of minimum distances between cameras within clusters:", sd_min_distance, "meters\n")
cat("Range of minimum distances between cameras within clusters: from", range_min_distance[1], "to", range_min_distance[2], "meters\n")
cat("Average maximum distance between cameras within clusters:", avg_max_distance, "meters\n")
cat("Standard Deviation of maximum distances between cameras within clusters:", sd_max_distance, "meters\n")
cat("Range of maximum distances between cameras within clusters: from", range_max_distance[1], "to", range_max_distance[2], "meters\n")
