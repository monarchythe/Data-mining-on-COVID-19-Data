# Load necessary libraries
library(tidyverse)       # For data manipulation and visualization
library(cluster)         # For Gower distance and clustering
library(dendextend)      # For hierarchical clustering visualization
library(pheatmap)        # For heatmaps
library(GGally)          # For visualizing pairwise relationships
library(factoextra)      # For clustering visualization
library(ggplot2)         # For data visualization
library(reshape2)        # For reshaping data
library(clValid)         # For Dunn Index calculation
library(NbClust)         # For determining optimal number of clusters


# Load dataset and convert to tibble
covid_data <- read_csv("/Users/mokashi/Namrata/R/COVID-19_cases_plus_census.csv") %>% as_tibble()

# Data Cleaning: Check for duplicates in County FIPS code and remove if necessary
covid_data <- covid_data %>% distinct(county_fips_code, .keep_all = TRUE)

# Convert character columns to factors for nominal data handling
covid_data <- covid_data %>% mutate_if(is.character, as.factor)

# Define the states and counties for clustering
selected_states <- c("NY", "NJ", "OR", "CA")
selected_counties <- c("New York County", "Essex County", "Malheur County", "Kings County")

# Filter dataset based on these locations
covid_data_filtered <- covid_data %>% 
  filter(state %in% selected_states & county_name %in% selected_counties)

# Check and calculate feature vectors if they do not exist
covid_data_filtered <- covid_data_filtered %>% 
  mutate(
    hispanic_pop_percentage = (hispanic_pop / total_pop) * 100,
    black_pop_percentage = (black_pop / total_pop) * 100,
    age_45_54_black = black_male_45_54 / total_pop * 100,
    age_55_64_black = black_male_55_64 / total_pop * 100,
    age_45_54_hispanic = hispanic_male_45_54 / total_pop * 100,
    age_55_64_hispanic = hispanic_male_55_64 / total_pop * 100
  )

# Define the feature subset for clustering
features <- c("hispanic_pop_percentage", "black_pop_percentage",  
              "age_45_54_black", "age_55_64_black",  
              "age_45_54_hispanic", "age_55_64_hispanic")

# Remove rows with missing data in selected features
covid_data_filtered <- covid_data_filtered %>% drop_na(all_of(features))

# Scale the data if needed and ensure it remains a data frame
covid_data_scaled <- covid_data_filtered %>% 
  mutate(across(all_of(features), ~ as.numeric(scale(.)))) %>% 
  as.data.frame()  # Ensure it remains a data frame

# Calculate Gower distance for mixed data
distance_matrix <- daisy(covid_data_scaled %>% select(all_of(features)), metric = "gower")

# Hierarchical clustering with Ward's method
hclust_result <- hclust(distance_matrix, method = "ward.D2")

# Convert county and state names to row labels
covid_data_filtered <- covid_data_filtered %>% 
  mutate(label = paste(county_name, state, sep = ", "))

# Dendrogram visualization
dend <- as.dendrogram(hclust_result)
labels(dend) <- covid_data_filtered$label

# Customize the appearance of the dendrogram
dend <- dend %>% 
  set("branches_k_color", k = 4, value = c("red", "green", "blue", "purple")) %>% 
  set("branches_lwd", 2.5) %>% 
  set("labels_colors", k = 4, value = c("red", "green", "blue", "purple")) %>% 
  set("labels_cex", 0.8)

# Plot the dendrogram
plot(dend, main = "Hierarchical Clustering Dendrogram\n(Age and Ethnicity-Based Clustering)", 
     xlab = "Counties", ylab = "Dissimilarity", sub = "Clusters based on age and ethnicity", yaxt = "n")

# Add custom y-axis with labeled intervals
axis(side = 2, at = seq(0, 0.8, by = 0.2), labels = seq(0, 0.8, by = 0.2), las = 1)

# Annotate clusters with descriptions
text(x = 5, y = 0.75, label = "Cluster 1: High Hispanic, Ages 45-64\nCounties: Kings County, CA", col = "red", pos = 4, cex = 0.7)
text(x = 5, y = 0.7, label = "Cluster 2: High Black, Ages 45-64\nCounties: Essex County, NJ", col = "green", pos = 4, cex = 0.7)
text(x = 5, y = 0.65, label = "Cluster 3: Balanced Demographics\nCounties: Essex County, NY", col = "blue", pos = 4, cex = 0.7)
text(x = 5, y = 0.6, label = "Cluster 4: Mixed Age Groups\nCounties: New York County, NY", col = "purple", pos = 4, cex = 0.7)

# Cut the dendrogram to create clusters
clusters <- cutree(hclust_result, k = 4)
covid_data_filtered$cluster <- clusters

# Create a unique identifier for the heatmap
row.names(covid_data_filtered) <- covid_data_filtered$label

# Visualize clusters with a heatmap
data_matrix <- as.matrix(covid_data_scaled %>% select(all_of(features)))
rownames(data_matrix) <- covid_data_filtered$label

# Create an annotation dataframe for the heatmap
annotation_data <- data.frame(Cluster = factor(clusters))
rownames(annotation_data) <- covid_data_filtered$label

# Generate the heatmap
pheatmap(
  data_matrix,
  cluster_rows = as.hclust(hclust_result),
  main = "Heatmap of Age and Ethnicity-Based Clusters",
  annotation_row = annotation_data,
  color = colorRampPalette(c("blue", "white", "red"))(100)
)

# Summarize data by cluster
cluster_summary <- covid_data_filtered %>% 
  group_by(cluster) %>% 
  summarize(across(all_of(features), mean, na.rm = TRUE))

print(cluster_summary)

# Reshape the cluster summary data for plotting
cluster_summary_long <- melt(cluster_summary, id.vars = "cluster")

# Prepare county names for each cluster
clustered_counties <- covid_data_filtered %>% 
  select(label, cluster) %>% 
  group_by(cluster) %>% 
  summarise(counties = paste(label, collapse = ", "))

# Generate the bar plot with annotations
ggplot(cluster_summary_long, aes(x = variable, y = value, fill = factor(cluster))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Feature Means by Cluster (Hierarchical Clustering Results)", 
       x = "Feature", 
       y = "Mean Value", 
       fill = "Cluster (Hierarchical)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_text(aes(label = round(value, 1)), position = position_dodge(width = 0.9), vjust = -0.25, size = 3) + 
  annotate("text", x = 5, y = 50, label = "Cluster 1: High Hispanic Population\nCounties: Kings County, CA", color = "darkred", hjust = 0, size = 2, fontface = "italic") +
  annotate("text", x = 5, y = 47, label = "Cluster 2: High Black Population\nCounties: Essex County, NJ", color = "darkgreen", hjust = 0, size = 2, fontface = "italic") +
  annotate("text", x = 5, y = 43, label = "Cluster 3: Low Hispanic & Black Populations\nCounties: Essex County, NY", color = "blue", hjust = 0, size = 2, fontface = "italic") +
  annotate("text", x = 5, y = 38, label = "Cluster 4: Mixed Demographics\nCounties: New York County, NY", color = "purple", hjust = 0, size = 2, fontface = "italic")

# Clustering evaluation methods
# Elbow Method
calculate_wcss_hierarchical <- function(hclust_obj, data, max_clusters = 6) {
  wcss <- numeric(max_clusters)
  for (k in 1:max_clusters) {
    clusters <- cutree(hclust_obj, k = k)
    total_wcss <- 0
    for (cluster in unique(clusters)) {
      cluster_points <- data[clusters == cluster, , drop = FALSE]
      cluster_center <- if (nrow(cluster_points) > 1) colMeans(cluster_points) else as.numeric(cluster_points)
      total_wcss <- total_wcss + sum(rowSums((cluster_points - cluster_center) ^ 2))
    }
    wcss[k] <- total_wcss
  }
  return(wcss)
}

# Calculate WCSS for a range of cluster numbers
wcss_values <- calculate_wcss_hierarchical(hclust_result, as.matrix(distance_matrix), max_clusters = 6)
wcss_data <- data.frame(k = 1:6, wcss = wcss_values)

# Plot the Elbow Method
ggplot(wcss_data, aes(x = k, y = wcss)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method: Within-Cluster Sum of Squares (WCSS)", 
       x = "Number of Clusters (k)", 
       y = "Total WCSS") +
  geom_vline(xintercept = which.min(diff(diff(wcss_values))) + 2, color = "red", linetype = "dashed") +
  theme_minimal()

# Silhouette Method
calculate_silhouette_width <- function(hclust_obj, diss_matrix, max_clusters = 6) {
  sil_widths <- numeric(max_clusters)
  for (k in 2:max_clusters) {
    clusters <- cutree(hclust_obj, k = k)
    sil <- tryCatch({
      silhouette(clusters, diss_matrix)
    }, error = function(e) {
      return(NULL)
    })
    if (!is.null(sil) && is.matrix(sil) && ncol(sil) >= 3) {
      sil_widths[k] <- mean(sil[, 3], na.rm = TRUE)
    } else {
      sil_widths[k] <- NA
    }
  }
  return(sil_widths)
}

sil_widths <- calculate_silhouette_width(hclust_result, as.dist(distance_matrix), max_clusters = 6)
silhouette_data <- data.frame(k = 2:6, silhouette_width = sil_widths[2:6])

# Plot Silhouette Method
ggplot(silhouette_data, aes(x = k, y = silhouette_width)) +
  geom_line() +
  geom_point() +
  labs(title = "Silhouette Method for Optimal Number of Clusters", 
       x = "Number of Clusters (k)", 
       y = "Average Silhouette Width") +
  geom_vline(xintercept = which.max(sil_widths[2:6]) + 1, color = "red", linetype = "dashed") +
  theme_minimal()

# Dunn Index Method
calculate_dunn_index <- function(hclust_obj, diss_matrix, max_clusters = 6) {
  dunn_indices <- numeric(max_clusters)
  for (k in 2:max_clusters) {
    clusters <- cutree(hclust_obj, k = k)
    dunn_indices[k] <- dunn(diss_matrix, clusters)
  }
  return(dunn_indices)
}

dunn_indices <- calculate_dunn_index(hclust_result, as.dist(distance_matrix), max_clusters = 6)
dunn_data <- data.frame(k = 2:6, dunn_index = dunn_indices[2:6])

# Plot Dunn Index
ggplot(dunn_data, aes(x = k, y = dunn_index)) +
  geom_line() +
  geom_point() +
  labs(title = "Dunn Index for Optimal Number of Clusters", 
       x = "Number of Clusters (k)", 
       y = "Dunn Index") +
  geom_vline(xintercept = which.max(dunn_indices[2:6]) + 1, color = "red", linetype = "dashed") +
  theme_minimal()


#---------------------------------------------------------------------------------------------------------------------
# Cluster TWO: Economic Factors
#---------------------------------------------------------------------------------------------------------------------


# Define the economic feature subset for clustering
economic_features <- c("poverty_percentage", "gini_index", "death_percentage")

# Inspect and handle zero or missing values in required columns
covid_data_filtered <- covid_data_filtered %>% 
  filter(total_pop > 0) %>%  # Remove rows with zero population to avoid division by zero
  drop_na(gini_index, poverty, deaths)  # Drop rows with missing values in key columns

# Calculate necessary feature columns if they don't already exist
if (!"poverty_percentage" %in% colnames(covid_data_filtered)) {
  covid_data_filtered <- covid_data_filtered %>% 
    mutate(poverty_percentage = poverty / total_pop * 100)
}

if (!"death_percentage" %in% colnames(covid_data_filtered)) {
  covid_data_filtered <- covid_data_filtered %>% 
    mutate(death_percentage = (deaths / total_pop) * 100)
}

# Ensure that all rows have non-missing values for the selected features
covid_data_filtered <- covid_data_filtered %>% drop_na(all_of(economic_features))

# Scale the data for clustering
covid_data_scaled_economic <- covid_data_filtered %>% 
  mutate(across(all_of(economic_features), ~ as.numeric(scale(.)))) %>% 
  as.data.frame()  # Ensure it remains a data frame

# Remove any rows with NaN or Inf values after scaling to avoid errors
covid_data_scaled_economic <- covid_data_scaled_economic %>% 
  filter_all(all_vars(!is.nan(.) & !is.infinite(.)))

# Check for any remaining NaN or Inf values after cleaning
rows_with_issues <- covid_data_scaled_economic %>% 
  filter_all(any_vars(is.nan(.) | is.infinite(.)))

if (nrow(rows_with_issues) > 0) {
  print("Rows with NaN or Inf values after all cleaning steps:")
  print(rows_with_issues)
  stop("Data still contains NaN or Inf values even after cleaning.")
}

# Calculate Gower distance for mixed data
distance_matrix_economic <- daisy(covid_data_scaled_economic %>% select(all_of(economic_features)), metric = "gower")

# Perform hierarchical clustering with Ward's method
hclust_result_economic <- hclust(distance_matrix_economic, method = "ward.D2")

# Convert county and state names to row labels
covid_data_filtered <- covid_data_filtered %>% 
  mutate(label = paste(county_name, state, sep = ", "))

# Customize and plot the dendrogram for economic factors
dend_economic <- as.dendrogram(hclust_result_economic)
labels(dend_economic) <- covid_data_filtered$label

# Customize the appearance of the dendrogram
dend_economic <- dend_economic %>% 
  set("branches_k_color", k = 4, value = c("red", "green", "blue", "purple")) %>% 
  set("branches_lwd", 2.5) %>% 
  set("labels_colors", k = 4, value = c("red", "green", "blue", "purple")) %>% 
  set("labels_cex", 0.8)

# Plot the dendrogram with enhanced title
plot(dend_economic, main = "Hierarchical Clustering Dendrogram\n(Economic Factors-Based Clustering)", 
     xlab = "Counties", ylab = "Dissimilarity", sub = "Clusters based on poverty, income inequality, and death percentage", yaxt = "n")

# Add custom y-axis
axis(side = 2, at = seq(0, 0.8, by = 0.2), labels = seq(0, 0.8, by = 0.2), las = 1)

# Annotate clusters with descriptions
text(x = 5, y = 0.75, label = "Cluster 1: High Poverty and Death Rates\nCounties: Example County, CA", col = "red", pos = 4, cex = 0.7)
text(x = 5, y = 0.7, label = "Cluster 2: High Income Disparity\nCounties: Example County, NJ", col = "green", pos = 4, cex = 0.7)
text(x = 5, y = 0.65, label = "Cluster 3: Moderate Inequality\nCounties: Example County, NY", col = "blue", pos = 4, cex = 0.7)
text(x = 5, y = 0.6, label = "Cluster 4: Mixed Economic Profiles\nCounties: Example County, NY", col = "purple", pos = 4, cex = 0.7)

# Cut the dendrogram to create clusters
clusters_economic <- cutree(hclust_result_economic, k = 4)
covid_data_filtered$cluster_economic <- clusters_economic

# Create a unique identifier for the heatmap
row.names(covid_data_filtered) <- covid_data_filtered$label

# Visualize clusters with a heatmap
data_matrix_economic <- as.matrix(covid_data_scaled_economic %>% select(all_of(economic_features)))
rownames(data_matrix_economic) <- covid_data_filtered$label

# Create an annotation dataframe for the heatmap
annotation_data_economic <- data.frame(Cluster = factor(clusters_economic))
rownames(annotation_data_economic) <- covid_data_filtered$label

# Generate the heatmap for economic factors
pheatmap(
  data_matrix_economic,
  cluster_rows = as.hclust(hclust_result_economic),
  main = "Heatmap of Economic Factor Clusters",
  annotation_row = annotation_data_economic,
  color = colorRampPalette(c("blue", "white", "red"))(100)
)

# Summarize data by cluster
cluster_summary_economic <- covid_data_filtered %>% 
  group_by(cluster_economic) %>% 
  summarize(across(all_of(economic_features), mean, na.rm = TRUE))

# Print the cluster summary
print(cluster_summary_economic)

# Reshape the cluster summary data for plotting
cluster_summary_long_economic <- melt(cluster_summary_economic, id.vars = "cluster_economic")

# Generate the bar plot with annotations for clusters
ggplot(cluster_summary_long_economic, aes(x = variable, y = value, fill = factor(cluster_economic))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Feature Means by Cluster (Economic Factors Clustering)", 
       x = "Feature", 
       y = "Mean Value", 
       fill = "Cluster (Hierarchical)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  # Adding numeric labels for each bar
  geom_text(aes(label = round(value, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.25, 
            size = 3) + 
  # Adding annotations for each cluster with line breaks
  annotate("text", x = 3, y = 50, label = "Cluster 1: High Poverty and Death Rates\nCounties: Example County, CA", color = "darkred", hjust = 0, size = 2, fontface = "italic") +
  annotate("text", x = 3, y = 47, label = "Cluster 2: High Income Disparity\nCounties: Example County, NJ", color = "darkgreen", hjust = 0, size = 2, fontface = "italic") +
  annotate("text", x = 3, y = 43, label = "Cluster 3: Moderate Inequality\nCounties: Example County, NY", color = "blue", hjust = 0, size = 2, fontface = "italic") +
  annotate("text", x = 3, y = 38, label = "Cluster 4: Mixed Economic Profiles\nCounties: Example County, NY", color = "purple", hjust = 0, size = 2, fontface = "italic")

# Ensure `covid_data_filtered` contains no missing data in the Gini Index column
covid_data_filtered <- covid_data_filtered %>% drop_na(gini_index)

# Convert Gini Index to matrix format for heatmap
data_matrix_gini <- as.matrix(covid_data_filtered$gini_index)
rownames(data_matrix_gini) <- covid_data_filtered$label  # Use county names as row labels
colnames(data_matrix_gini) <- "Gini Index"  # Label the column

# Generate clustered heatmap for Gini Index
pheatmap(
  data_matrix_gini,
  cluster_rows = TRUE,      # Enable clustering for rows
  cluster_cols = FALSE,     # Only one column, so no need to cluster columns
  main = "Clustered Heatmap of Gini Index by County",
  color = colorRampPalette(c("blue", "white", "red"))(100), # Blue for low, red for high inequality
  show_rownames = TRUE,     # Display row names for identification
  labels_col = "Gini Index"
)

  # Define the range of clusters to evaluate
  k_values <- 1:6  # Start from 1 to be consistent with WCSS calculation


# Elbow Method
calculate_wcss_hierarchical <- function(hclust_obj, data, max_clusters = 6) {
  wcss <- numeric(max_clusters)
  for (k in 1:max_clusters) {
    clusters <- cutree(hclust_obj, k = k)
    total_wcss <- 0
    for (cluster in unique(clusters)) {
      cluster_points <- data[clusters == cluster, , drop = FALSE]
      cluster_center <- if (nrow(cluster_points) > 1) colMeans(cluster_points) else as.numeric(cluster_points)
      total_wcss <- total_wcss + sum(rowSums((cluster_points - cluster_center) ^ 2))
    }
    wcss[k] <- total_wcss
  }
  return(wcss)
}

# Calculate WCSS for the Elbow Method
wcss_values <- calculate_wcss_hierarchical(hclust_result_economic, as.matrix(distance_matrix_economic), max_clusters = max(k_values))
wcss_data <- data.frame(k = k_values, wcss = wcss_values)

# Plot the Elbow Method
ggplot(wcss_data, aes(x = k, y = wcss)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method: Within-Cluster Sum of Squares (WCSS)", 
       x = "Number of Clusters (k)", 
       y = "Total WCSS") +
  geom_vline(xintercept = which.min(diff(diff(wcss_values))) + 2, color = "red", linetype = "dashed") +
  theme_minimal()


# Silhouette Method
calculate_silhouette_width <- function(hclust_obj, diss_matrix, max_clusters = 6) {
  sil_widths <- numeric(max_clusters)
  for (k in 2:max_clusters) {
    clusters <- cutree(hclust_obj, k = k)
    sil <- tryCatch({
      silhouette(clusters, diss_matrix)
    }, error = function(e) {
      return(NULL)
    })
    if (!is.null(sil) && is.matrix(sil) && ncol(sil) >= 3) {
      sil_widths[k] <- mean(sil[, 3], na.rm = TRUE)
    } else {
      sil_widths[k] <- NA
    }
  }
  return(sil_widths)
}

# Calculate Silhouette Width for each cluster size
sil_widths <- calculate_silhouette_width(hclust_result_economic, as.dist(distance_matrix_economic), max_clusters = max(k_values))
silhouette_data <- data.frame(k = k_values, silhouette_width = sil_widths[k_values])

# Plot Silhouette Method
ggplot(silhouette_data, aes(x = k, y = silhouette_width)) +
  geom_line() +
  geom_point() +
  labs(title = "Silhouette Method for Optimal Number of Clusters",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Width") +
  geom_vline(xintercept = which.max(sil_widths[k_values]), color = "red", linetype = "dashed") +
  theme_minimal()

# Dunn Index Method
calculate_dunn_index <- function(hclust_obj, diss_matrix, max_clusters = 6) {
  dunn_indices <- numeric(max_clusters)
  for (k in 2:max_clusters) {
    clusters <- cutree(hclust_obj, k = k)
    dunn_indices[k] <- dunn(diss_matrix, clusters)
  }
  return(dunn_indices)
}

# Calculate Dunn Index for each cluster size
dunn_indices <- calculate_dunn_index(hclust_result_economic, as.dist(distance_matrix_economic), max_clusters = max(k_values))
dunn_data <- data.frame(k = k_values, dunn_index = dunn_indices[k_values])

# Plot Dunn Index
ggplot(dunn_data, aes(x = k, y = dunn_index)) +
  geom_line() +
  geom_point() +
  labs(title = "Dunn Index for Optimal Number of Clusters",
       x = "Number of Clusters (k)",
       y = "Dunn Index") +
  geom_vline(xintercept = which.max(dunn_indices[k_values]), color = "red", linetype = "dashed") +
  theme_minimal()



#---------------------------------------------------------------------------------------------------------------------
# Cluster THREE: Economic Vulnerability
#---------------------------------------------------------------------------------------------------------------------

# Load necessary libraries
library(tidyverse)
library(cluster)        # For Gower distance and clustering
library(dendextend)     # For hierarchical clustering visualization
library(pheatmap)       # For heatmaps

# Define the economic vulnerability feature subset for Cluster THREE
economic_vulnerability_features <- c("unemployment_rate", 
                                     "income_less_10000", 
                                     "income_10000_14999", 
                                     "death_percentage", 
                                     "median_income")

# Calculate `unemployment_rate` if it does not exist
if (!"unemployment_rate" %in% colnames(covid_data_filtered)) {
  if ("unemployed_pop" %in% colnames(covid_data_filtered) && "pop_in_labor_force" %in% colnames(covid_data_filtered)) {
    covid_data_filtered <- covid_data_filtered %>%
      mutate(unemployment_rate = (unemployed_pop / pop_in_labor_force) * 100)
  } else {
    stop("Cannot calculate unemployment rate; `unemployed_pop` and/or `pop_in_labor_force` columns are missing.")
  }
}

# Ensure that all rows have non-missing values for the selected features
covid_data_filtered <- covid_data_filtered %>% 
  drop_na(all_of(economic_vulnerability_features))

# Scale the data for clustering
covid_data_scaled_vulnerability <- covid_data_filtered %>% 
  mutate(across(all_of(economic_vulnerability_features), ~ as.numeric(scale(.)))) %>% 
  as.data.frame()  # Ensure it remains a data frame

# Check for and remove any rows with NaN or Inf values after scaling
covid_data_scaled_vulnerability <- covid_data_scaled_vulnerability %>% 
  filter_all(all_vars(!is.nan(.) & !is.infinite(.)))

# Calculate Gower distance for mixed data
distance_matrix_vulnerability <- daisy(covid_data_scaled_vulnerability %>% select(all_of(economic_vulnerability_features)), metric = "gower")

# Perform hierarchical clustering with Ward's method
hclust_result_vulnerability <- hclust(distance_matrix_vulnerability, method = "ward.D2")

# Convert county and state names to row labels for clarity in plots
covid_data_filtered <- covid_data_filtered %>% 
  mutate(label = paste(county_name, state, sep = ", "))

# Create and customize the dendrogram
dend_vulnerability <- as.dendrogram(hclust_result_vulnerability)
labels(dend_vulnerability) <- covid_data_filtered$label

# Customize the appearance of the dendrogram
dend_vulnerability <- dend_vulnerability %>% 
  set("branches_k_color", k = 4, value = c("red", "green", "blue", "purple")) %>% 
  set("branches_lwd", 2.5) %>% 
  set("labels_colors", k = 4, value = c("red", "green", "blue", "purple")) %>% 
  set("labels_cex", 0.8)

# Plot the dendrogram with enhanced title
plot(dend_vulnerability, main = "Hierarchical Clustering Dendrogram\n(Economic Vulnerability-Based Clustering)", 
     xlab = "Counties", ylab = "Dissimilarity", sub = "Clusters based on unemployment and income brackets", yaxt = "n")

# Add custom y-axis labels
axis(side = 2, at = seq(0, 0.8, by = 0.2), labels = seq(0, 0.8, by = 0.2), las = 1)

# Annotate clusters with descriptions
text(x = 5, y = 0.75, label = "Cluster 1: High Unemployment, Low Income\nCounties: Example County, CA", col = "red", pos = 4, cex = 0.7)
text(x = 5, y = 0.7, label = "Cluster 2: Moderate Economic Vulnerability\nCounties: Example County, NJ", col = "green", pos = 4, cex = 0.7)
text(x = 5, y = 0.65, label = "Cluster 3: Mixed Income Levels\nCounties: Example County, NY", col = "blue", pos = 4, cex = 0.7)
text(x = 5, y = 0.6, label = "Cluster 4: Low Economic Vulnerability\nCounties: Example County, NY", col = "purple", pos = 4, cex = 0.7)

# Cut the dendrogram to create clusters
clusters_vulnerability <- cutree(hclust_result_vulnerability, k = 4)
covid_data_filtered$cluster_vulnerability <- clusters_vulnerability

# Visualize clusters with a heatmap
data_matrix_vulnerability <- as.matrix(covid_data_scaled_vulnerability %>% select(all_of(economic_vulnerability_features)))
rownames(data_matrix_vulnerability) <- covid_data_filtered$label

# Create an annotation dataframe for the heatmap
annotation_data_vulnerability <- data.frame(Cluster = factor(clusters_vulnerability))
rownames(annotation_data_vulnerability) <- covid_data_filtered$label

# Generate the heatmap for economic vulnerability clusters
pheatmap(
  data_matrix_vulnerability,
  cluster_rows = as.hclust(hclust_result_vulnerability),
  main = "Heatmap of Economic Vulnerability Clusters",
  annotation_row = annotation_data_vulnerability,
  color = colorRampPalette(c("blue", "white", "red"))(100)
)

# Summarize data by cluster
cluster_summary_vulnerability <- covid_data_filtered %>% 
  group_by(cluster_vulnerability) %>% 
  summarize(across(all_of(economic_vulnerability_features), mean, na.rm = TRUE))

# Print the cluster summary for reference
print(cluster_summary_vulnerability)

# Reshape the cluster summary data for plotting
cluster_summary_long_vulnerability <- melt(cluster_summary_vulnerability, id.vars = "cluster_vulnerability")

# Generate a bar plot for cluster feature means
ggplot(cluster_summary_long_vulnerability, aes(x = variable, y = value, fill = factor(cluster_vulnerability))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Feature Means by Cluster (Economic Vulnerability Clustering)", 
       x = "Feature", 
       y = "Mean Value", 
       fill = "Cluster") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  # Adding numeric labels for each bar
  geom_text(aes(label = round(value, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.25, 
            size = 3)
#---------------------------------------------------------------------------------------------------
#Number of clustring 
#1 Elbow Method 
#---------------------------------------------------------------------------------------------------
# Load necessary libraries
library(cluster)  # For daisy and clustering
library(ggplot2)  # For plotting

# Step 1: Calculate the Gower distance matrix
distance_matrix_vulnerability <- daisy(covid_data_scaled_vulnerability %>% select(all_of(economic_vulnerability_features)), metric = "gower")

# Step 2: Perform hierarchical clustering
hclust_result_vulnerability <- hclust(distance_matrix_vulnerability, method = "ward.D2")

# Step 3: Define a function to calculate WCSS for each number of clusters (k)
calculate_wcss_hierarchical <- function(hclust_obj, data, max_clusters = 10) {
  wcss <- numeric(max_clusters)
  
  for (k in 1:max_clusters) {
    # Cut the dendrogram to create `k` clusters
    clusters <- cutree(hclust_obj, k = k)
    
    # Calculate WCSS for each cluster
    total_wcss <- 0
    for (cluster in unique(clusters)) {
      cluster_points <- data[clusters == cluster, ]
      
      # Calculate the center only if there is more than one point in the cluster
      if (is.matrix(cluster_points) && nrow(cluster_points) > 1) {
        cluster_center <- colMeans(cluster_points)
      } else {
        cluster_center <- as.numeric(cluster_points)
      }
      
      # Sum of squared distances to the cluster center
      total_wcss <- total_wcss + sum(rowSums((as.matrix(cluster_points) - cluster_center) ^ 2))
    }
    wcss[k] <- total_wcss
  }
  
  return(wcss)
}

# Step 4: Calculate WCSS for a range of cluster numbers
wcss_values <- calculate_wcss_hierarchical(hclust_result_vulnerability, as.matrix(distance_matrix_vulnerability), max_clusters = 6)

# Step 5: Create a data frame for plotting
wcss_data <- data.frame(k = 1:6, wcss = wcss_values)

# Step 6: Plot the Elbow Method
ggplot(wcss_data, aes(x = k, y = wcss)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method: Within-Cluster Sum of Squares (WCSS) for Economic Vulnerability",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares (WCSS)") +
  geom_vline(xintercept = which.min(diff(diff(wcss_values))), color = "red", linetype = "dashed") +
  theme_minimal()
#---------------------------------------------------------------------------------------------------
#Number of clustering three
#2 Silhouette Method 
#---------------------------------------------------------------------------------------------------
# Load necessary libraries
library(cluster)  # For silhouette calculations
library(ggplot2)  # For plotting

# Step 1: Calculate the Gower distance matrix for mixed data types
distance_matrix_vulnerability <- daisy(covid_data_scaled_vulnerability %>% select(all_of(economic_vulnerability_features)), metric = "gower")

# Step 2: Perform hierarchical clustering
hclust_result_vulnerability <- hclust(distance_matrix_vulnerability, method = "ward.D2")

# Step 3: Define a function to calculate the average silhouette width for each k
calculate_silhouette_width <- function(hclust_obj, diss_matrix, max_clusters = 6) {  # Adjusted max_clusters to 6
  sil_widths <- numeric(max_clusters)
  
  for (k in 2:max_clusters) {  # Start from k=2 because silhouette doesn't apply to k=1
    # Cut the dendrogram to create k clusters
    clusters <- cutree(hclust_obj, k = k)
    
    # Calculate the silhouette width for the current clustering
    sil <- tryCatch({
      silhouette(clusters, diss_matrix)
    }, error = function(e) {
      return(NULL)
    })
    
    # Check that sil is not NULL and has the correct dimensions
    if (!is.null(sil) && is.matrix(sil) && ncol(sil) >= 3) {
      sil_widths[k] <- mean(sil[, 3], na.rm = TRUE)  # Extract the silhouette width column and calculate the mean
    } else {
      sil_widths[k] <- NA  # Set to NA if silhouette calculation was not successful
    }
  }
  
  return(sil_widths)
}

# Step 4: Calculate silhouette widths for a range of cluster numbers (up to 6)
sil_widths <- calculate_silhouette_width(hclust_result_vulnerability, as.dist(distance_matrix_vulnerability), max_clusters = 6)

# Step 5: Create a data frame for plotting
silhouette_data <- data.frame(k = 2:6, silhouette_width = sil_widths[2:6])

# Step 6: Plot the Silhouette Method results
optimal_k <- which.max(sil_widths[2:6]) + 1  # Adding 1 because k starts from 2

ggplot(silhouette_data, aes(x = k, y = silhouette_width)) +
  geom_line() +
  geom_point() +
  labs(title = "Silhouette Method for Optimal Number of Clusters (Economic Vulnerability)",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Width") +
  geom_vline(xintercept = optimal_k, color = "red", linetype = "dashed") +
  theme_minimal() +
  annotate("text", x = optimal_k, 
           y = max(sil_widths[2:6], na.rm = TRUE), 
           label = paste("Optimal k =", optimal_k), 
           vjust = -1, color = "red")
#---------------------------------------------------------------------------------------------------
#Number of clustering three
#3 Dunn Method 
#---------------------------------------------------------------------------------------------------
# Load necessary libraries
library(cluster)    # For clustering calculations
library(clValid)    # For Dunn Index calculation

# Calculate the Gower distance matrix
distance_matrix_vulnerability <- daisy(covid_data_scaled_vulnerability %>% select(all_of(economic_vulnerability_features)), metric = "gower")

# Perform hierarchical clustering with Ward's method
hclust_result_vulnerability <- hclust(distance_matrix_vulnerability, method = "ward.D2")

# Define a function to calculate Dunn Index for different numbers of clusters
calculate_dunn_index <- function(hclust_obj, diss_matrix, max_clusters = 6) {
  dunn_indices <- numeric(max_clusters)
  
  for (k in 2:max_clusters) {
    # Cut the dendrogram to create k clusters
    clusters <- cutree(hclust_obj, k = k)
    
    # Calculate the Dunn Index
    dunn_indices[k] <- dunn(diss_matrix, clusters)
  }
  
  return(dunn_indices)
}

# Calculate Dunn Index for a range of cluster numbers
dunn_indices <- calculate_dunn_index(hclust_result_vulnerability, as.dist(distance_matrix_vulnerability), max_clusters = 6)

# Create a data frame for plotting
dunn_data <- data.frame(k = 2:6, dunn_index = dunn_indices[2:6])

# Plot the Dunn Index values
library(ggplot2)
ggplot(dunn_data, aes(x = k, y = dunn_index)) +
  geom_line() +
  geom_point() +
  labs(title = "Dunn Index for Optimal Number of Clusters (Economic Vulnerability)",
       x = "Number of Clusters (k)",
       y = "Dunn Index") +
  theme_minimal()

#---------------------------------------------------------------------------------------------------------------------
# Cluster FOUR: Comprehensive Feature Set for Clustering
#---------------------------------------------------------------------------------------------------------------------

# Define comprehensive feature set for clustering
comprehensive_features <- c(
  "hispanic_pop_percentage", "black_pop_percentage",
  "age_45_54_black", "age_55_64_black",
  "age_45_54_hispanic", "age_55_64_hispanic",
  "poverty_percentage", "gini_index", "death_percentage",
  "unemployment_rate", "income_less_10000", "income_10000_14999",
  "median_income"
)

# Ensure dataset contains only relevant features and handle missing values
covid_data_filtered <- covid_data_filtered %>%
  select(all_of(comprehensive_features)) %>%
  drop_na() %>%
  mutate(across(everything(), ~ as.numeric(scale(.))))

# Calculate Gower distance for mixed data
distance_matrix_comprehensive <- daisy(covid_data_filtered, metric = "gower")

# Perform hierarchical clustering with Ward's method
hclust_result_comprehensive <- hclust(distance_matrix_comprehensive, method = "ward.D2")

# Convert hierarchical clustering result to dendrogram and set colors for clusters
dend <- as.dendrogram(hclust_result_comprehensive) %>%
  set("branches_k_color", k = 4, value = c("red", "green", "blue", "purple")) %>%
  set("labels_cex", 0.8) %>%
  set("labels_colors", k = 4, value = c("red", "green", "blue", "purple"))

# Enhanced Dendrogram with Cluster Labels and Annotations
plot(dend, 
     main = "Hierarchical Clustering of Counties by Demographics and Economic Factors", 
     sub = "Clusters Highlighting Key Population and Economic Profiles", 
     xlab = "Counties", 
     ylab = "Dissimilarity")
rect.hclust(hclust_result_comprehensive, k = 4, border = c("red", "green", "blue", "purple"))

# Annotate clusters with descriptions to explain the grouping
text(x = 4.8, y = 0.75, label = "Cluster 1: High Hispanic, High Poverty", col = "red", pos = 4, cex = 0.8)
text(x = 4.8, y = 0.7, label = "Cluster 2: High Black, High Unemployment", col = "green", pos = 4, cex = 0.8)
text(x = 4.8, y = 0.65, label = "Cluster 3: High Median Income", col = "blue", pos = 4, cex = 0.8)
text(x = 4.8, y = 0.6, label = "Cluster 4: Mixed Demographics", col = "purple", pos = 4, cex = 0.8)

# Enhanced Parallel Coordinates Plot with Cluster Descriptions
ggparcoord(
  data = covid_data_filtered %>% mutate(cluster = as.factor(clusters_comprehensive)),
  columns = 1:(ncol(covid_data_filtered) - 1),  # Exclude the cluster column from features
  groupColumn = "cluster",                      # Color by cluster
  scale = "uniminmax"                           # Scale to [0, 1] for better comparison
) + 
  labs(
    title = "Parallel Coordinates Plot for Clustered Data by Demographics and Economic Factors",
    subtitle = "Key Features: Poverty, Ethnicity, Income Levels, and Unemployment Rate, Age",
    x = "Features",
    y = "Scaled Values"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10)
  ) +
  scale_color_manual(
    values = c("red", "green", "blue", "purple"),
    labels = c(
      "1: High Hispanic, High Poverty",
      "2: High Black, High Unemployment",
      "3: High Median Income",
      "4: Mixed Demographics"
    )
  ) +
  guides(color = guide_legend(title = "Cluster Descriptions"))
# Load necessary libraries
library(tidyverse)
library(cluster)        # For clustering calculations and Gower distance
library(ggplot2)        # For plotting
library(clValid)        # For Dunn Index calculation if needed

# Define comprehensive feature set for clustering
comprehensive_features <- c(
  "hispanic_pop_percentage", "black_pop_percentage",
  "age_45_54_black", "age_55_64_black",
  "age_45_54_hispanic", "age_55_64_hispanic",
  "poverty_percentage", "gini_index", "death_percentage",
  "unemployment_rate", "income_less_10000", "income_10000_14999",
  "median_income"
)

# Ensure dataset contains only relevant features and handle missing values
covid_data_filtered <- covid_data_filtered %>%
  select(all_of(comprehensive_features)) %>%
  drop_na() %>%
  mutate(across(everything(), ~ as.numeric(scale(.))))

# Calculate Gower distance for mixed data
distance_matrix_comprehensive <- daisy(covid_data_filtered, metric = "gower")

# Perform hierarchical clustering with Ward's method
hclust_result_comprehensive <- hclust(distance_matrix_comprehensive, method = "ward.D2")

# ---------------------------------------------------------------------------------------------------
# Method 1: Elbow Method (WCSS)
# ---------------------------------------------------------------------------------------------------

# Function to calculate WCSS for hierarchical clustering
calculate_wcss_hierarchical <- function(hclust_obj, data, max_clusters = 10) {
  wcss <- numeric(max_clusters)
  
  for (k in 1:max_clusters) {
    clusters <- cutree(hclust_obj, k = k)
    total_wcss <- 0
    for (cluster in unique(clusters)) {
      cluster_points <- data[clusters == cluster, ]
      if (is.matrix(cluster_points) && nrow(cluster_points) > 1) {
        cluster_center <- colMeans(cluster_points)
      } else {
        cluster_center <- as.numeric(cluster_points)
      }
      total_wcss <- total_wcss + sum(rowSums((as.matrix(cluster_points) - cluster_center) ^ 2))
    }
    wcss[k] <- total_wcss
  }
  
  return(wcss)
}

# Calculate WCSS and handle NA values
wcss_values <- calculate_wcss_hierarchical(hclust_result_comprehensive, as.matrix(distance_matrix_comprehensive), max_clusters = 6)
wcss_values[is.na(wcss_values)] <- 0

# Prepare data for plotting
wcss_data <- data.frame(k = 1:6, wcss = wcss_values)

# Plot the Elbow Method
ggplot(wcss_data, aes(x = k, y = wcss)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method: WCSS for Comprehensive Feature Set",
       x = "Number of Clusters (k)",
       y = "Total WCSS") +
  geom_vline(xintercept = which.min(diff(diff(wcss_values))), color = "red", linetype = "dashed") +
  theme_minimal()

# ---------------------------------------------------------------------------------------------------
# Method 2: Silhouette Method
# ---------------------------------------------------------------------------------------------------

# Function to calculate silhouette widths
calculate_silhouette_width <- function(hclust_obj, diss_matrix, max_clusters = 6) {
  sil_widths <- numeric(max_clusters)
  
  for (k in 2:max_clusters) {
    clusters <- cutree(hclust_obj, k = k)
    sil <- tryCatch({
      silhouette(clusters, diss_matrix)
    }, error = function(e) {
      return(NULL)
    })
    if (!is.null(sil) && is.matrix(sil) && ncol(sil) >= 3) {
      sil_widths[k] <- mean(sil[, 3], na.rm = TRUE)
    } else {
      sil_widths[k] <- NA
    }
  }
  
  return(sil_widths)
}

# Calculate Silhouette Widths and handle NA values
sil_widths <- calculate_silhouette_width(hclust_result_comprehensive, as.dist(distance_matrix_comprehensive), max_clusters = 6)
sil_widths[is.na(sil_widths)] <- 0

# Prepare data for plotting
silhouette_data <- data.frame(k = 2:6, silhouette_width = sil_widths[2:6])

# Plot the Silhouette Method
optimal_k <- which.max(sil_widths[2:6]) + 1
ggplot(silhouette_data, aes(x = k, y = silhouette_width)) +
  geom_line() +
  geom_point() +
  labs(title = "Silhouette Method for Comprehensive Feature Set",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Width") +
  geom_vline(xintercept = optimal_k, color = "red", linetype = "dashed") +
  theme_minimal() +
  annotate("text", x = optimal_k, 
           y = max(sil_widths[2:6], na.rm = TRUE), 
           label = paste("Optimal k =", optimal_k), 
           vjust = -1, color = "red")

# ---------------------------------------------------------------------------------------------------
# Method 3: Dunn Index
# ---------------------------------------------------------------------------------------------------

# Function to calculate Dunn Index for different clusters
calculate_dunn_index <- function(hclust_obj, diss_matrix, max_clusters = 6) {
  dunn_indices <- numeric(max_clusters)
  
  for (k in 2:max_clusters) {
    clusters <- cutree(hclust_obj, k = k)
    dunn_indices[k] <- dunn(diss_matrix, clusters)
  }
  
  return(dunn_indices)
}

# Calculate Dunn Index and handle NA values
dunn_indices <- calculate_dunn_index(hclust_result_comprehensive, as.dist(distance_matrix_comprehensive), max_clusters = 6)
dunn_indices[is.na(dunn_indices)] <- 0

# Prepare data for plotting
dunn_data <- data.frame(k = 2:6, dunn_index = dunn_indices[2:6])

# Plot the Dunn Index
ggplot(dunn_data, aes(x = k, y = dunn_index)) +
  geom_line() +
  geom_point() +
  labs(title = "Dunn Index for Comprehensive Feature Set",
       x = "Number of Clusters (k)",
       y = "Dunn Index") +
  theme_minimal()


