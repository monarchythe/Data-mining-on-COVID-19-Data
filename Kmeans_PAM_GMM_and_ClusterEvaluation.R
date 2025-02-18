#Libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(maps)
library(ggcorrplot)
library(DT)
library(ggrepel)
library(htmlwidgets)
library(factoextra)
library(pheatmap)
library(seriation)
library(fpc)
library(cluster)
library(plotly)


# Reading the data set
covid_cases_census <- read_csv("/Users/monarchnigam/Covid data /COVID-19/COVID-19_cases_plus_census.csv",)


#Converting the data set to tibble 
covid_cases_census <- covid_cases_census %>% mutate_if(is.character, factor)
covid_census_tibble <- as_tibble(covid_cases_census)

#Preparing data fOR CLUSTERING:
#Handling missing data, Scaling the data, and Handling outliers
#-Starting With NY-----------------------
cases_NY <- covid_cases_census %>% filter(state == "NY")
dim(cases_NY)
cases_NY
#Cleaning the data
# Checking for missing values in the entire dataset
cases_NY %>%
  summarise_all(~ sum(is.na(.))) #So there are no missing values 
# Checking the duplicate rows in County fips code
cases_NY %>%
  filter(duplicated(.))

cases_NY %>%
  summarise(
    missing_fips = sum(is.na(county_fips_code)),
    duplicated_fips = sum(duplicated(county_fips_code))
  )
#summary(cases_NY)
# Calculate the total population of New York State
total_population_NY <- sum(cases_NY$total_pop, na.rm = TRUE)

cases_NY_select <- cases_NY %>% #filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, 
         median_income,hispanic_pop, black_pop, poverty,
         gini_index,income_less_10000,income_10000_14999,
         unemployed_pop,pop_in_labor_force,hispanic_male_45_54,
         hispanic_male_55_64, black_male_45_54, black_male_55_64)#From others report we got an idea to use gini index as well.

cases_NY_select <- cases_NY_select %>% mutate(
  
  cases_percent = (confirmed_cases/total_pop)*100, 
  deaths_percent = (deaths/total_pop)*100, 
  death_per_case = deaths/confirmed_cases,
  
  # New calculations for Hispanic and Black population percentages
  hispanic_pop_percentage = (hispanic_pop / total_pop) * 100,
  black_pop_percentage = (black_pop / total_pop) * 100,
  
  # Sum of Hispanic and Black population percentages
  hispanic_black_pop_percentage = ((hispanic_pop + black_pop) / total_pop) * 100,
  
  # Poverty percentage
  poverty_percentage = (poverty / total_pop) * 100,
  
  # Population percentage per county
  population_percentage = (total_pop / total_population_NY) * 100,
  
  #Poverty Percentage
  poverty_percentage = (poverty / total_pop) * 100,
  
  # Population percentage per county
  population_percentage = (total_pop / total_population_NY) * 100,
  
  # Unemployed population percentage
  unemployed_pop_percentage = (unemployed_pop / total_pop) * 100,
  
  # Population in labor force percentage
  pop_in_labor_force_percentage = (pop_in_labor_force / total_pop) * 100,
  
  # Income less than $10,000 percentage
  income_less_10000_percentage = (income_less_10000 / total_pop) * 100,
  
  # Income between $10,000 and $14,999 percentage
  income_10000_14999_percentage = (income_10000_14999 / total_pop) * 100,
  
  # Unemployment rate
  unemplyment_rate = unemployed_pop / total_pop,
  
  #Age
  hispanic_male_45_54_percentage = (hispanic_male_45_54/hispanic_pop) * 100,
  hispanic_male_55_64_percentage = (hispanic_male_55_64/hispanic_pop) * 100,
  black_male_45_54_percentage = (black_male_45_54/black_pop) * 100,
  black_male_55_64_percentage = (black_male_55_64/black_pop) * 100

)

summary(cases_NY_select) # looks like we dont have any missing values and the rows matches to the exact county numbers in NY state. 
cases_NY_select$gini_index
# We decide the number of feature vectors to compare------------------------------------------------------
ggplot(data = cases_NY_select, aes(x = hispanic_black_pop_percentage, y = deaths_percent)) + 
  geom_point() +
  geom_text_repel(aes(label = county_name), max.overlaps = 6) +  # Add county names
  labs(
    title = "Deaths Percentage vs. Hispanic and Black Population Percentage",
    x = "Hispanic and Black Population Percentage",
    y = "Deaths Percentage"
  )


#Scaling the data ----------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# Selecting only the relevant columns to scale (excluding county_name)
features_to_scale <- cases_NY_select %>%
  select(hispanic_black_pop_percentage, deaths_percent)

# Scaling the selected columns
scaled_features <- scale(features_to_scale)
summary(scaled_features)
# K means clustering the data 
km<- kmeans(scaled_features, centers = 3, nstart = 12)
km
str(km)
plot(scaled_features, col= km$cluster)
cluster<- factor(km$cluster)

# Create a dataframe with scaled features and cluster assignments
clustered_data <- data.frame(
  scaled_features,
  cluster = factor(km$cluster),
  county_name = cases_NY_select$county_name
)

# Plot the clusters with county names as labels
fviz_cluster(km, data = scaled_features, geom = "point", ellipse.type = "norm") +
  geom_text_repel(data = clustered_data, aes(x = hispanic_black_pop_percentage, y = deaths_percent, label = county_name),
                  max.overlaps = 6, # Controls the maximum number of overlaps
                  box.padding = 0.3, # Adds padding around text boxes
                  point.padding = 0.5, # Adds padding between text and points
                  size = 3) + # Text size
  labs(
    title = "Cluster Plot with County Names",
    x = "Hispanic and Black Population Percentage (Scaled)",
    y = "Deaths Percentage (Scaled)"
  ) +
  theme_minimal()
##--------------------------------OUTLIER---------------------------------------------------
#Removing the outlier and -----------------------------------------------------------------------------
# Exclude specific counties from the dataset
cases_NY_select_without_outlier <- cases_NY_select %>%
  filter(!county_name %in% c("Bronx County", "Queens County", "Richmond County", "Kings County"))

# Check the remaining counties
#summary(cases_NY_select_without_outlier)

#------------------------------------------------------
ggplot(data = cases_NY_select_without_outlier, aes(x = hispanic_black_pop_percentage, y = deaths_percent)) + 
  geom_point() +
  geom_text_repel(aes(label = county_name), max.overlaps = 4) +  # Add county names
  labs(
    title = "Deaths Percentage vs. Hispanic and Black Population Percentage",
    x = "Hispanic and Black Population Percentage",
    y = "Deaths Percentage"
  )


# Selecting only the relevant columns to scale (excluding county_name)
features_to_scale <- cases_NY_select_without_outlier %>%
  select(hispanic_black_pop_percentage, deaths_percent)

# Scaling the selected columns
scaled_features <- scale(features_to_scale)


summary(scaled_features)
# K means clustering the data 
km<- kmeans(scaled_features, centers = 4, nstart = 20)
km
str(km)
plot(scaled_features, col= km$cluster)
cluster<- factor(km$cluster)

# Create a dataframe with scaled features and cluster assignments
clustered_data <- data.frame(
  scaled_features,
  cluster = factor(km$cluster),
  county_name = cases_NY_select_without_outlier$county_name #without outlier counties
)

# Plot the clusters with county names as labels
fviz_cluster(km, data = scaled_features, geom = "point", ellipse.type = "norm") +
  geom_text_repel(data = clustered_data, aes(x = hispanic_black_pop_percentage, y = deaths_percent, label = county_name),
                  max.overlaps = 6, # Controls the maximum number of overlaps
                  box.padding = 0.3, # Adds padding around text boxes
                  point.padding = 0.5, # Adds padding between text and points
                  size = 3) + # Text size
  labs(
    title = "Cluster Plot with County Names",
    x = "Hispanic and Black Population Percentage (Scaled)",
    y = "Deaths Percentage (Scaled)"
  ) +
  theme_minimal()
#Number of cluster for CLUSTER ONE -
k_values <- 1:10  # Adjust this range as needed

# Initialize a vector to store the WCSS for each k
wcss <- numeric(length(k_values))

# Calculate WCSS for each k
for (k in k_values) {
  set.seed(123)  # For reproducibility
  km <- kmeans(scaled_features, centers = k, nstart = 25)  # 25 random starts for better accuracy
  wcss[k] <- km$tot.withinss  # Total within-cluster sum of squares
}

# Create a data frame to store the results
wcss_data <- data.frame(k = k_values, wcss = wcss)

# Plot the Elbow Method
ggplot(wcss_data, aes(x = k, y = wcss)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 4, color = "red", linetype = "dashed") + 
  labs(title = "Elbow Method: Within-Cluster Sum of Squares",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares (WCSS)") +
  theme_minimal() 

# Install and load necessary libraries
if (!require(cluster)) install.packages("cluster", dependencies=TRUE)
if (!require(clValid)) install.packages("clValid", dependencies=TRUE)
library(cluster)
library(clValid)

# Define the range of clusters to test
k_values <- 2:10  # Start from 2 since Dunn Index is not defined for k=1

# Initialize a vector to store the Dunn Index for each k
dunn_index_values <- numeric(length(k_values))

# Loop through each k value to calculate Dunn Index
for (i in seq_along(k_values)) {
  k <- k_values[i]
  set.seed(123)  # For reproducibility
  
  # Perform k-means clustering
  km <- kmeans(scaled_features, centers = k, nstart = 2)
  
  # Calculate Dunn Index using 'clValid'
  dunn_index_values[i] <- dunn(clusters = km$cluster, Data = scaled_features)
}

# Create a data frame to store the results
dunn_data <- data.frame(k = k_values, Dunn_Index = dunn_index_values)

# Find the optimal k based on the maximum Dunn Index
optimal_k <- dunn_data$k[which.max(dunn_data$Dunn_Index)]
cat("Optimal number of clusters based on Dunn Index:", optimal_k, "\n")

# Plot the Dunn Index values for different k
ggplot(dunn_data, aes(x = k, y = Dunn_Index)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = optimal_k, color = "red", linetype = "dashed") +  # Highlight optimal k
  labs(title = "Dunn Index for Different Numbers of Clusters",
       x = "Number of Clusters (k)",
       y = "Dunn Index") +
  theme_minimal()

#Clustering tendency
get_clust_tendency(scaled_features, n = 10)
# Define the k values we want to compare
k_values_to_test <- c(4, 6)
cluster_percentages <- list()  # Store cluster percentages for each k

for (k in k_values_to_test) {
  set.seed(123)  # For reproducibility
  km <- kmeans(scaled_features, centers = k, nstart = 25)
  
  # Calculate the percentage of points in each cluster
  cluster_counts <- table(km$cluster)
  total_points <- sum(cluster_counts)
  cluster_percentage <- round((cluster_counts / total_points) * 100, 2)
  
  # Store the result in the list
  cluster_percentages[[paste0("k=", k)]] <- cluster_percentage
}

# Display cluster percentages for k=4 and k=6
cluster_percentages

#Supervised Evaluation---------------------------------
# Install and load the necessary package
if (!require(mclust)) install.packages("mclust", dependencies = TRUE)
library(mclust)

# Transform the death percentage into categorical labels as ground truth
# Example: We could bin the death percentage into categories (e.g., low, medium, high)
# Adjust the breaks based on your data to create meaningful categories
cases_NY_select_without_outlier$death_label <- cut(cases_NY_select_without_outlier$deaths_percent,
                                                   breaks = 3,  # Adjust the number of bins as appropriate
                                                   labels = c("Low", "Medium", "High"))

# K-means clustering: Assuming you already have multiple clustering results
# Below is the clustering done on scaled features (as in your example)
km_clusters <- km$cluster  # Cluster labels for your first clustering

# Rand Index calculation for each clustering result
# Ensure `death_label` is a factor for comparison
ground_truth <- as.numeric(as.factor(cases_NY_select_without_outlier$death_label))

# Rand Index for the first clustering result (you can repeat this for others)
rand_index_1 <- adjustedRandIndex(ground_truth, km_clusters)
print(paste("Rand Index for first clustering:", rand_index_1))

# Repeat this process for the other clustering results by changing `km_clusters`
# For example, if you have other clusterings saved as `km_clusters_2`, `km_clusters_3`, etc.
# rand_index_2 <- adjustedRandIndex(ground_truth, km_clusters_2)
# rand_index_3 <- adjustedRandIndex(ground_truth, km_clusters_3)
# ...

# Output Rand Index values to compare each clustering result
print(paste("Rand Index for clustering 1:", rand_index_1))
# Repeat for the others as needed
# print(paste("Rand Index for clustering 2:", rand_index_2))
# print(paste("Rand Index for clustering 3:", rand_index_3))
# ...

#---------------------------------------------------------------------------
##-----------------------plotting for more variables----------------------------------------------------
##---------------------------------------------------------------------------
##---------------------------------Income Factors Only------------------------------------------
##---------------------------------------------------------------------------
# We decide the number of feature vectors to compare------------------------------------------------------
# 1. UNEMPLOYED VS DEATHS
ggplot(data = cases_NY_select, aes(x = gini_index, y = deaths_percent)) + #Instead of cases_NY_select, put cases_NY_select_without_outlier 
  geom_point() +
  geom_text_repel(aes(label = county_name), max.overlaps = 4) +  # Add county names
  labs(
    title = "Deaths Percentage vs. Gini Index",
    x = "Gini Index",
    y = "Deaths Percentage"
  )
#Removing the outliers
unique(cases_NY_select$county_name)
cases_NY_select_without_outlier <- cases_NY_select %>%
  filter(!county_name %in% c("Bronx County", "Queens County", "New York County", "Kings County"))
unique(cases_NY_select_without_outlier$county_name)
# Selecting only the relevant columns to scale (excluding county_name)
features_to_scale <- cases_NY_select_without_outlier %>% #Instead of cases_NY_select, put cases_NY_select_without_outlier
  select(gini_index, poverty_percentage,
         deaths_percent, hispanic_male_45_54_percentage, 
         hispanic_male_55_64_percentage,black_male_45_54_percentage, 
         black_male_55_64_percentage,unemplyment_rate,
         income_less_10000_percentage, income_10000_14999_percentage)
         #hispanic_black_pop_percentage,poverty_percentage,gini_index,unemplyment_rate, income_less_10000_percentage, income_10000_14999_percentage ) #, hispanic_male_45_54_percentage, hispanic_male_55_64_percentage,black_male_45_54_percentage, black_male_55_64_percentage
#hispanic_black_pop_percentage,poverty_percentage,gini_index,unemplyment_rate, income_less_10000_percentage, income_10000_14999_percentage
# Scaling the selected columns
scaled_features <- scale(features_to_scale)
summary(scaled_features)
# K means clustering the data 
km<- kmeans(scaled_features, centers = 4, nstart = 20)
km
str(km)
plot(scaled_features, col= km$cluster)
cluster<- factor(km$cluster)
# Create a new data frame with the scaled features and cluster assignments
clustered_data <- data.frame(
  unemplyment_rate = scaled_features[, "unemplyment_rate"],
  income_less_10000_percentage = scaled_features[, "income_less_10000_percentage"],
  income_10000_14999_percentage = scaled_features[, "income_10000_14999_percentage"],
  deaths_percent = scaled_features[, "deaths_percent"],
  county_name = cases_NY_select_without_outlier$county_name,  # Adding county names
  cluster = factor(km$cluster)  # Adding cluster assignments as a factor
)

# Generate the cluster plot with fviz_cluster and retrieve the coordinates
fviz_cluster_plot <- fviz_cluster(
  km,
  data = clustered_data[, c("unemplyment_rate", "income_less_10000_percentage", "income_10000_14999_percentage", "deaths_percent")],
  geom = "point",
  ellipse.type = "norm",
  main = "Cluster plot with unemplyment rate, Income less than 10000 and Income between 10000 and 14999, and Death per Case",
  xlab = "Dimension 1",
  ylab = "Dimension 2",
  show.clust.cent = TRUE
)

# Extract data points and cluster assignments from fviz_cluster
fviz_cluster_data <- fviz_cluster_plot$data

# Add the county names to the extracted data for labeling
fviz_cluster_data$county_name <- clustered_data$county_name

# Plot with county names using ggrepel
fviz_cluster_plot + 
  geom_text_repel(
    data = fviz_cluster_data,
    aes(x = x, y = y, label = county_name),
    max.overlaps = 5,  # Limits overlapping labels
    size = 3
  )
#Looking at cluster profile
ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"), 
                    cols = colnames(km$centers)), 
       aes(y = name, x = value, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(cols = vars(cluster)) +
  labs(y = "feature", x = "z-scores", title = "Cluster Profiles") + 
  guides(fill="none")


# Reorder data by cluster and create a heatmap
ordered_data <- scaled_features[order(km$cluster), ]
row.names(ordered_data) <- cases_NY_select_without_outlier$county_name[order(km$cluster)] #Instead of cases_NY_select, put cases_NY_select_without_outlier
# Generate the heatmap with labeled counties by ORDERING THE ROWS OF THE PLOT AS professor Suggested---------------------------
# Calculate the row mean to use as a basis for ordering
row_order <- order(rowMeans(ordered_data), decreasing = TRUE)
# Reorder the rows of ordered_data
ordered_data_sorted <- ordered_data[row_order, ]

# Generate the heatmap with labeled counties and sorted rows
pheatmap(ordered_data_sorted, cluster_rows = FALSE, cluster_cols = FALSE, 
         color = colorRampPalette(c("blue", "white", "red"))(100), 
         main = "Heatmap of Scaled Features by Cluster (Ordered)",
         labels_row = row.names(ordered_data_sorted),
         fontsize_row = 6) 

#Deciding the number of cluster:----------------------------------------
str(scaled_features)
#Number of cluster for CLUSTER 2-4 -
k_values <- 1:10  # Adjust this range as needed

# Initialize a vector to store the WCSS for each k
wcss <- numeric(length(k_values))

# Calculate WCSS for each k
for (k in k_values) {
  set.seed(123)  # For reproducibility
  km <- kmeans(scaled_features, centers = k, nstart = 25)  # 25 random starts for better accuracy
  wcss[k] <- km$tot.withinss  # Total within-cluster sum of squares
}

# Create a data frame to store the results
wcss_data <- data.frame(k = k_values, wcss = wcss)

# Plot the Elbow Method
ggplot(wcss_data, aes(x = k, y = wcss)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 3, color = "red", linetype = "dashed") + 
  labs(title = "Elbow Method: Within-Cluster Sum of Squares",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares (WCSS)") +
  theme_minimal()
#-2nd METHOD TO FIND Number of cluster-----------------------------------------------------------
#-------------------------------------------------------------

# Define the range of clusters to test
k_values <- 2:10  # Start from 2 since Dunn Index is not defined for k=1

# Initialize a vector to store the Dunn Index for each k
dunn_index_values <- numeric(length(k_values))

# Loop through each k value to calculate Dunn Index
for (i in seq_along(k_values)) {
  k <- k_values[i]
  set.seed(123)  # For reproducibility
  
  # Perform k-means clustering
  kms <- kmeans(scaled_features, centers = k, nstart = 20)
  
  # Calculate Dunn Index using 'clValid'
  dunn_index_values[i] <- dunn(clusters = kms$cluster, Data = scaled_features)
}

# Create a data frame to store the results
dunn_data <- data.frame(k = k_values, Dunn_Index = dunn_index_values)

# Find the optimal k based on the maximum Dunn Index
optimal_k <- dunn_data$k[which.max(dunn_data$Dunn_Index)]
cat("Optimal number of clusters based on Dunn Index:", optimal_k, "\n")

# Plot the Dunn Index values for different k
ggplot(dunn_data, aes(x = k, y = Dunn_Index)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = optimal_k, color = "red", linetype = "dashed") +  # Highlight optimal k
  labs(title = "Dunn Index for Different Numbers of Clusters",
       x = "Number of Clusters (k)",
       y = "Dunn Index") +
  theme_minimal()
#3rd method for Deciding number of cluster-------------------------------------
k <- clusGap(scaled_features, 
             FUN = kmeans,  
             nstart = 10,
             K.max = 10)
k
plot(k)
set.seed(123)  # For reproducibility
gap_stat <- clusGap(scaled_features, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# Plot the gap statistic
fviz_gap_stat(gap_stat) +
  ggtitle("Gap Statistic for Optimal Clusters") +
  theme_minimal()
#------------------------------------------------------------------------
#Clustering tendency
get_clust_tendency(scaled_features, n = 10)
#Clustering tendency
get_clust_tendency(scaled_features, n = 10)
# Define the k values we want to compare
k_values_to_test <- c(5, 2, 3, 4, 6)
cluster_percentages <- list()  # Store cluster percentages for each k

for (k in k_values_to_test) {
  set.seed(123)  # For reproducibility
  km <- kmeans(scaled_features, centers = k, nstart = 25)
  
  # Calculate the percentage of points in each cluster
  cluster_counts <- table(km$cluster)
  total_points <- sum(cluster_counts)
  cluster_percentage <- round((cluster_counts / total_points) * 100, 2)
  
  # Store the result in the list
  cluster_percentages[[paste0("k=", k)]] <- cluster_percentage
}

# Display cluster percentages for k=4 and k=6
cluster_percentages
#So we are getting K = 3 from this method, hence we will go with 3 cluster and do the reporting 

#-----------------------K-Means Clustering ends---------------------------------------------------------------------------
# Unsupervised cluster evaluation

d <- dist(scaled_features)
library(cluster)
plot(silhouette(km$cluster, d))
fviz_silhouette(silhouette(km$cluster, d))

library(seriation)
ggpimage(d)
ggpimage(d, order=order(km$cluster))
ggdissplot(d, labels = km$cluster, 
           options = list(main = "k-means with k=5"))
fviz_dist(d)

#Supervised --------------
cases_NY_select_without_outlier$death_label <- cut(cases_NY_select_without_outlier$deaths_percent,
                                                   breaks = 3,  # Adjust the number of bins as appropriate
                                                   labels = c("Low", "Medium", "High"))
# Plot the distribution of death rate categories
ggplot(cases_NY_select_without_outlier, aes(x = death_label, fill = death_label)) +
  geom_bar(stat = "count") +  # Use stat = "count" to count the number of counties in each category
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +  # Display the count above each bar
  labs(title = "Distribution of Counties by Death Rate Categories",
       x = "Death Rate Category",
       y = "Number of Counties") +
  scale_fill_manual(values = c("Low" = "skyblue", "Medium" = "orange", "High" = "red")) +
  theme_minimal() +
  theme(legend.position = "none")

# Rand Index calculation for each clustering result
# Ensure `death_label` is a factor for comparison
ground_truth <- as.numeric(as.factor(cases_NY_select_without_outlier$death_label))

# Rand Index for the first clustering result (you can repeat this for others)
rand_index_2 <- adjustedRandIndex(ground_truth, km_clusters)
print(paste("Rand Index for first clustering:", rand_index_1))

# Output Rand Index values to compare each clustering result
print(paste("Rand Index for clustering 2:", rand_index_2))
#------------------------------------------------------------------------------------
#Exceptional Work - 
d <- dist(scaled_features)
str(d)
p <- pam(d, k = 4)
p
# Create a new data frame from scaled_features and add the cluster column
scaled_feature_clustered <- as.data.frame(scaled_features) |> 
  add_column(cluster = factor(p$cluster))

# Extract medoids as a data frame
medoids <- as_tibble(scaled_features[p$medoids, ], rownames = "cluster")
medoids
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Perform PCA on scaled features
pca_result <- prcomp(scaled_features, center = TRUE, scale. = TRUE)

# Create a data frame with the first two principal components and the cluster assignments
pca_data <- as.data.frame(pca_result$x[, 1:2]) %>% 
  add_column(cluster = factor(p$cluster))

# Plot the clusters based on the first two principal components
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) + 
  geom_point(size = 2) +
  labs(title = "PAM Clustering Visualization using PCA",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggforce)  # For drawing ellipses

# Perform PCA on scaled features
pca_result <- prcomp(scaled_features, center = TRUE, scale. = TRUE)

# Create a data frame with the first two principal components and the cluster assignments
pca_data <- as.data.frame(pca_result$x[, 1:2]) %>% 
  add_column(cluster = factor(p$cluster))

# Extract medoid coordinates in PCA-transformed space
medoids_pca <- predict(pca_result, newdata = as.data.frame(scaled_features[p$medoids, ]))

# Convert medoids to data frame with cluster labels
medoids_pca_df <- as.data.frame(medoids_pca) %>%
  add_column(cluster = factor(1:4))

# Plot the clusters with enhancements
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) + 
  geom_point(size = 3, alpha = 0.7) +  # Slight transparency for better visibility
  geom_point(data = medoids_pca_df, aes(x = PC1, y = PC2, color = cluster), 
             shape = 3, size = 6, stroke = 2) +  # Larger, cross-shaped medoids
  geom_mark_ellipse(aes(fill = cluster), alpha = 0.1, show.legend = FALSE) +  # Cluster ellipses
  labs(title = "Enhanced PAM Clustering Visualization using PCA",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_brewer(palette = "Set1")  # Optional: Set color palette for distinction

#Determining Number of Clusters for PAM -----------------------------------------------
# Load necessary libraries
library(cluster)   # For clustering
library(clValid)   # For Dunn Index calculation

# Define the range for the number of clusters
num_clusters <- 2:10

# Initialize a vector to store Dunn Index for each k
dunn_values <- numeric(length(num_clusters))

# Calculate the Dunn Index for each number of clusters
for (k in num_clusters) {
  pam_result <- pam(scaled_features, k = k)
  cluster_labels <- pam_result$cluster
  
  # Calculate the Dunn Index
  dunn_values[k - 1] <- dunn(clusters = cluster_labels, Data = scaled_features)
}

# Plot the Dunn Index values to identify the optimal number of clusters
plot(num_clusters, dunn_values, type = "b", pch = 19, col = "blue",
     xlab = "Number of Clusters", ylab = "Dunn Index",
     main = "Dunn Index for Different Numbers of Clusters")

# Calculate the Gap Statistic
set.seed(123)  # Set seed for reproducibility
gap_stat <- clusGap(scaled_features, FUN = pam, K.max = 10, B = 100)

# Print the Gap Statistic result
print(gap_stat, method = "firstmax")

# Plot the Gap Statistic values
fviz_gap_stat(gap_stat)

#Unsupervide evaluation-----------------------------------------------
# Load necessary libraries
library(cluster)
library(factoextra)
library(seriation)

# Distance matrix calculation
d <- dist(scaled_features)

# Silhouette Plot for PAM Clustering
pam_silhouette <- silhouette(p$cluster, d)
plot(pam_silhouette, main = "Silhouette Plot for PAM Clustering")

# Visualize Silhouette using fviz_silhouette
fviz_silhouette(pam_silhouette)

# Visualize Dissimilarity Matrix for PAM Clustering
ggpimage(d)
ggpimage(d, order = order(p$cluster))
ggdissplot(d, labels = p$cluster, 
           options = list(main = "PAM with selected number of clusters"))
fviz_dist(d)

# Supervised Evaluation - PAM Clustering----------------------------

# Add death rate categories to the dataset
cases_NY_select_without_outlier$death_label <- cut(cases_NY_select_without_outlier$deaths_percent,
                                                   breaks = 3,  # Adjust the number of bins as appropriate
                                                   labels = c("Low", "Medium", "High"))



# Calculate Rand Index for PAM Clustering
library(mclust)

# Ensure `death_label` is a factor for comparison
ground_truth <- as.numeric(as.factor(cases_NY_select_without_outlier$death_label))
pam_clusters <- p$cluster

# Calculate Adjusted Rand Index for PAM Clustering
rand_index_pam <- adjustedRandIndex(ground_truth, pam_clusters)
print(paste("Rand Index for PAM clustering:", rand_index_pam))

#EXCEPTIONAL WORK 4th Algorithm Gaussian mixture models 
library(mclust)
m <- Mclust(scaled_features)
plot(m, what = "BIC")  # This plots BIC values for different models and cluster counts

# Fit the Gaussian Mixture Model
m <- Mclust(scaled_features)

# Perform PCA for dimensionality reduction
pca_result <- prcomp(scaled_features, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x[, 1:2])  # Take the first 2 principal components
pca_data$cluster <- as.factor(m$classification)  # Add the cluster assignments

# Plot the clusters on the first two principal components
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point() +
  labs(title = "Gaussian Mixture Model Clustering with PCA",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

library(plotly)

# Take the first 3 principal components
pca_data_3d <- as.data.frame(pca_result$x[, 1:3])
pca_data_3d$cluster <- as.factor(m$classification)

# 3D plot
plot_ly(pca_data_3d, x = ~PC1, y = ~PC2, z = ~PC3, color = ~cluster, colors = 'Set1') %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')),
         title = "3D Visualization of Clusters using Gaussian Mixture Model")
#Unsupervide for GMM ----------------------------------------------
# Fit GMM to data and summarize results
gmm <- Mclust(scaled_features)
summary(gmm)

# Extract the cluster assignments from the GMM model
gmm_clusters <- gmm$classification

# Calculate silhouette scores based on GMM clustering
d <- dist(scaled_features)
gmm_silhouette <- silhouette(gmm_clusters, d)

# Silhouette Plot for GMM Clustering
plot(gmm_silhouette, main = "Silhouette Plot for GMM Clustering")
fviz_silhouette(gmm_silhouette)

# Evaluate with BIC (already calculated in Mclust)
print(gmm$bic)


# Reorder distance matrix based on GMM clusters
library(seriation)
ggpimage(d, order = order(gmm_clusters))
ggdissplot(d, labels = gmm_clusters, 
           options = list(main = "Dissimilarity Matrix for GMM Clustering"))
#Supervised for GMM----------------------------------
gmm_clusters <- m$classification

# Ensure `death_label` is a factor for comparison
ground_truth <- as.numeric(as.factor(cases_NY_select_without_outlier$death_label))

# Calculate Adjusted Rand Index for GMM Clustering
rand_index_gmm <- adjustedRandIndex(ground_truth, gmm_clusters)
print(paste("Rand Index for GMM clustering:", rand_index_gmm))

#Reference code taken from : https://mhahsler.github.io/Introduction_to_Data_Mining_R_Examples/book/cluster-analysis.html