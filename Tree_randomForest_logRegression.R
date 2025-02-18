#Libraries
#install.packages("DMwR")
#nstall.packages("rpart")
#install.packages("gridExtra")
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
library(reshape2)
library(FSelector)
library(caret)
library(gridExtra)
library(rpart)
library(rpart.plot)
library(pROC)

# Reading the data set
covid_cases_census <- read_csv("/Users/mokashi/Namrata/R/COVID-19_cases_plus_census.csv")
covid_cases_census <- covid_cases_census %>% mutate_if(is.character, factor)
left_fips_code <- sprintf("%05d", as.numeric(covid_cases_census$county_fips_code))

mobility_data <- read_csv("/Users/mokashi/Namrata/R/Global_Mobility_Report.csv")
mobility_data <- mobility_data %>% mutate_if(is.character, factor)

right_fips_code <-sprintf("%05d", as.numeric(mobility_data$census_fips_code))

check_fips <- setdiff(left_fips_code , right_fips_code)
print(check_fips)

usa_mobility_data <- mobility_data %>%
  filter(country_region == "United States") #, !is.na(sub_region_1), !is.na(sub_region_2))

head(usa_mobility_data)

usa_mobility_data <- mobility_data %>%
  filter(country_region == "United States", !is.na(sub_region_1), !is.na(sub_region_2))
head(usa_mobility_data)

county_mobility_summary <- usa_mobility_data %>%
  group_by(census_fips_code) %>%
  summarize(
    retail_and_recreation = mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE),
    grocery_and_pharmacy = mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE),
    parks = mean(parks_percent_change_from_baseline, na.rm = TRUE),
    transit_stations = mean(transit_stations_percent_change_from_baseline, na.rm = TRUE),
    workplaces = mean(workplaces_percent_change_from_baseline, na.rm = TRUE),
    residential = mean(residential_percent_change_from_baseline, na.rm = TRUE),
    .groups = "drop"
  )
county_mobility_summary
nrow(county_mobility_summary)
nrow(covid_cases_census)
county_mobility_summary
summary(mobility_data)
str(mobility_data)

check_fips <- setdiff(covid_cases_census$county_fips_code , mobility_data$census_fips_code)
print(check_fips)
summary(covid_cases_census$county_fips_code)
summary(mobility_data$census_fips_code)
#Combining the file
combined_data <- covid_cases_census %>%
  left_join(county_mobility_summary, by = c("county_fips_code" = "census_fips_code")) 
nrow(combined_data)

combined_data <- combined_data %>% mutate_if(is.character, factor)
summary(combined_data)

#Adding class variables
combined_data <- combined_data %>% mutate(
  cases_percent = (confirmed_cases/total_pop)*100, 
  deaths_percent = (deaths/total_pop)*100, 
  death_per_case = deaths/confirmed_cases)

combined_cases_select <- combined_data %>% #filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, state, cases_percent, deaths_percent,death_per_case,
         total_pop,median_income,gini_index,
         hispanic_pop, black_pop, poverty,
         income_less_10000,income_10000_14999,
         unemployed_pop,hispanic_male_45_54,
         hispanic_male_55_64, #black_male_45_54, 
         black_male_55_64,
         commuters_by_public_transportation,employed_pop,unemployed_pop,
         percent_income_spent_on_rent,median_year_structure_built,in_school,
         in_undergrad_college, four_more_cars, 
         employed_manufacturing,sales_office_employed,employed_public_administration,
         employed_transportation_warehousing_utilities,employed_wholesale_trade,
         pop_in_labor_force,employed_construction,employed_public_administration,
         retail_and_recreation, grocery_and_pharmacy,
         workplaces)

combined_cases_select <- combined_cases_select %>% mutate(
  
  #Hispanic and Black population
  hispanic_pop = (hispanic_pop / total_pop), 
  black_pop = (black_pop / total_pop),
  
  # Poverty 
  poverty = (poverty / total_pop),
  
  # Unemployed 
  unemployed_pop = (unemployed_pop / total_pop),
  
  # Population in labor force percentage
  pop_in_labor_force = (pop_in_labor_force / total_pop) ,
  
  # Income less than $10,000 percentage
  income_less_10000 = income_less_10000 / total_pop ,
  
  # Income between $10,000 and $14,999 percentage
  income_10000_14999 = (income_10000_14999 / total_pop),
  
  #Age
  hispanic_male_45_54 = (hispanic_male_45_54/hispanic_pop),
  hispanic_male_55_64 = (hispanic_male_55_64/hispanic_pop),
  #black_male_45_54 = (black_male_45_54/black_pop),
  black_male_55_64 = (black_male_55_64/black_pop),
  
  #Commuters by public trans
  commuters_by_public_transportation = commuters_by_public_transportation/ total_pop, 
  
  #Education and employed and rich
  employed_pop = employed_pop / total_pop,
  in_school = in_school / total_pop, 
  in_undergrad_college = in_undergrad_college / total_pop,
  four_more_cars = four_more_cars/ total_pop,
  
  #Employed in different sectors from Offices to Labors
  employed_manufacturing = employed_manufacturing/ total_pop,
  
  sales_office_employed = sales_office_employed/ total_pop,
  
  employed_public_administration = employed_public_administration/ total_pop,
  
  employed_transportation_warehousing_utilities = employed_transportation_warehousing_utilities/ total_pop,
  
  employed_wholesale_trade = employed_wholesale_trade/total_pop,
  
  pop_in_labor_force = pop_in_labor_force/ total_pop,
  
  employed_construction = employed_construction/ total_pop,
  
  employed_public_administration = employed_public_administration/ total_pop
)
#CLass Variable average 
mean(combined_cases_select$deaths_percent)
#check that class variable is a factor! Otherwise, many models will perform regression.
str(combined_cases_select)
table(complete.cases(combined_cases_select))
dim(combined_cases_select)

# Compute the correlation matrix
cor_matrix <- cor(combined_cases_select %>% select(where(is.numeric)),use = "complete.obs" ) #use = "pairwise.complete.obs" use = "complete.obs"

#Using ggplot2 for heatmap
cor_data <- melt(cor_matrix)  # Reshape for ggplot
ggplot(cor_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap", x = "", y = "")

#-----------------------------------------------------------------------------
#Modeling rpart--------------------------------------------------
#-----------------------------------------------------------------------------

#Setting class variable
combined_cases_select <- combined_cases_select %>% mutate(bad = as.factor(deaths_percent > 0.11))
#imbalance checking
combined_cases_select %>% pull(bad) %>% table()

state_summary <- combined_cases_select %>% group_by(state) %>% 
  summarize(bad_pct = sum(bad == TRUE)/n()) %>%
  arrange(desc(bad_pct)) 
# Display top and bottom 5 rows
top_and_bottom <- bind_rows(
  state_summary %>% head(20),   # Top 5 rows
  state_summary %>% tail(20)    # Bottom 5 rows
)
# Print the result
print(top_and_bottom, n = 20)
specific_states <- state_summary %>% filter(state %in% c("MD", "CA", "MS", "DE", "TX", "NY", "IA", "KS", "NV", "ND")) #. "TX", "CA", "FL", "NY", "NJ"
print(specific_states)
#Split into training and testing Data
cases_train <- combined_cases_select %>% filter(state %in% c("MD", "CA", "MS", "DE", "TX", "NY", "IA", "KS", "NV", "ND"))
dim(cases_train)
cases_train %>% pull(bad) %>% table()

cases_test <- combined_cases_select %>% filter(!(state %in% c("MD", "CA", "MS", "DE", "TX", "NY", "IA", "KS", "NV", "ND")))
dim(cases_test)
cases_test %>% pull(bad) %>% table()
#Mapping the data 
counties <- as_tibble(map_data("county"))
counties <- counties %>% 
  rename(c(county = subregion, state = region)) %>%
  mutate(state = state.abb[match(state, tolower(state.name))]) %>%
  select(state, county, long, lat, group)
counties  

#Mapping the Training Data 
counties_all <- counties %>% left_join(cases_train %>% 
                                         mutate(county = county_name %>% str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', '')))
ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'yellow'))

#Mapping the testing data 
counties_all <- counties %>% left_join(cases_test %>% 
                                         mutate(county = county_name %>% str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', '')))
ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'yellow'))
#Checking variable Importance 
library(FSelector)
cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head()

#Removing the variables used to create class variable
cases_train <- cases_train %>% select(-deaths_percent, -death_per_case, -cases_percent)
cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head(n=12)

cases_test <- cases_test %>% select(-deaths_percent, -death_per_case, -cases_percent)

library(FSelector)
cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head(25)

cases_train <- cases_train %>% drop_na()
cases_test <- cases_test %>% drop_na()

#Bulding a model Random Forest
library(caret)
fit <- cases_train %>%
  train(bad ~ . - county_name - state, # Removing county name and State as it will make tree very complicated
        data = . ,
        #method = "rpart",
        method = "rf", #Random forest 
        #method = "nb",
        trControl = trainControl(method = "cv", number = 10)
  )
fit


varImp(fit)

cases_test$bad_predicted <- predict(fit, cases_test)

counties_test <- counties %>% left_join(cases_test %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'yellow'))
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'yellow'))

rf_conf_matrix <- confusionMatrix(data = cases_test$bad_predicted, ref = cases_test$bad)

cm_table <- as.data.frame(rf_conf_matrix$table)

# Rename columns for clarity
colnames(cm_table) <- c("Prediction", "Actual", "Count")

# Create the confusion matrix heatmap
heatmap <- ggplot(cm_table, aes(x = Actual, y = Prediction, fill = Count)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Count), size = 6, color = "white") +
  scale_fill_gradient(low = "orange", high = "blue") +
  labs(title = "Confusion Matrix for Random Forest Method", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# Create a summary table for metrics
metrics <- data.frame(
  Metric = c("Sensitivity", "Specificity", "Precision", "Recall", "Accuracy", "Kappa"),
  Value = c(
    round(rf_conf_matrix$byClass["Sensitivity"], 3),
    round(rf_conf_matrix$byClass["Specificity"], 3),
    round(rf_conf_matrix$byClass["Precision"], 3),
    round(rf_conf_matrix$byClass["Recall"], 3),
    round(rf_conf_matrix$overall["Accuracy"], 3),
    round(rf_conf_matrix$overall["Kappa"], 3)
  )
)

# Create the summary table visualization
metrics_table <- tableGrob(metrics, rows = NULL)

# Combine heatmap and metrics into a single plot
grid.arrange(heatmap, metrics_table, nrow = 2, heights = c(2, 1))

# Train a single decision tree
tree_fit <- train(
  bad ~ . - county_name - state,
  data = cases_train,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10)
)

# Feature importance
varImp(tree_fit)

# Selecting the most important variables based on varImp
important_vars <- c("pop_in_labor_force", "median_income",
                    "commuters_by_public_transportation", "black_male_55_64",
                    "unemployed_pop", "hispanic_male_55_64", 
                    "percent_income_spent_on_rent", "workplaces")

# Filter the dataset to include only the important variables
cases_train_subset <- cases_train %>% select(all_of(important_vars), bad)

# Adjust the tree complexity parameter to simplify
simple_tree_clean <- rpart(
  bad ~ ., 
  data = cases_train_subset, 
  method = "class",
  control = rpart.control(cp = 0.02, minsplit = 20, maxdepth = 4)
)

# Visualizing the cleaner decision tree
rpart.plot(
  simple_tree_clean, 
  type = 2,                # Display the decision labels at the split
  extra = 104,             # Show counts and percentages
  under = FALSE,           # Do not show class labels under nodes
  fallen.leaves = FALSE,   # Avoid scattered leaves
  box.palette = "GnBu",    # Use a color palette for better clarity
  shadow.col = "gray",     # Add shadow effect
  cex = 1.2,               # Reduce text size to make lines visible
  tweak = 0.4,             # Adjust spacing of nodes
  main = "Decision Tree"  # Add a title
)

# Complexity Parameter (CP) vs Cross-Validation Error
cp_table <- simple_tree_clean$cptable
cp_values <- cp_table[, "CP"]
xerror_values <- cp_table[, "xerror"]

plot(cp_values, xerror_values, 
     type = "b", 
     col = "blue", 
     pch = 19, 
     main = "Complexity Parameter vs. Cross-Validation Error", 
     xlab = "Complexity Parameter (CP)", 
     ylab = "Cross-Validation Error (xerror)",
     lwd = 2)
text(cp_values, xerror_values, labels = round(cp_values, 3), pos = 4, cex = 0.8, col = "red")
min_cp <- cp_table[which.min(cp_table[, "xerror"]), "CP"]
abline(v = min_cp, col = "red", lty = 2)
legend("topright", legend = c("Cross-validation error", "Optimal CP"), 
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)

# Predict on test data
cases_test$bad_predicted <- predict(simple_tree_clean, cases_test, type = "class")

# Confusion Matrix
conf_matrix <- confusionMatrix(data = cases_test$bad_predicted, reference = cases_test$bad)

# Convert confusion matrix to a data frame
cm_table <- as.data.frame(conf_matrix$table)
colnames(cm_table) <- c("Predicted", "Actual", "Count")

# Add class labels
levels(cm_table$Predicted) <- c("Endanger", "Not Endanger")
levels(cm_table$Actual) <- c("Endanger", "Not Endanger")

# Confusion Matrix Heatmap
heatmap_plot <- ggplot(cm_table, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Count), size = 6, color = "white") +
  scale_fill_gradient(low = "orange", high = "blue") +
  labs(title = "Confusion Matrix for rpart Method", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Performance Metrics
metrics <- data.frame(
  Metric = c("Sensitivity", "Specificity", "Precision", "Recall", "F1", "Accuracy", "Kappa"),
  Value = c(
    round(conf_matrix$byClass["Sensitivity"], 3),
    round(conf_matrix$byClass["Specificity"], 3),
    round(conf_matrix$byClass["Precision"], 3),
    round(conf_matrix$byClass["Recall"], 3),
    round(2 * (conf_matrix$byClass["Precision"] * conf_matrix$byClass["Recall"]) /
            (conf_matrix$byClass["Precision"] + conf_matrix$byClass["Recall"]), 3),
    round(conf_matrix$overall["Accuracy"], 3),
    round(conf_matrix$overall["Kappa"], 3)
  )
)

# Metrics Table
metrics_table <- tableGrob(metrics, rows = NULL)

# Combine Heatmap and Metrics Table
combined_plot <- grid.arrange(heatmap_plot, metrics_table, nrow = 2, heights = c(3, 1))
combined_plot

# ROC Curve
cases_test$bad_probs <- predict(simple_tree_clean, cases_test, type = "prob")[, 2]
roc_curve <- roc(cases_test$bad, cases_test$bad_probs)
plot(roc_curve, main = "ROC Curve for Decision Tree", col = "blue", lwd = 2)
#auc_value <- auc(roc_curve)
#text(0.5, 0.5, paste("AUC =", round(auc_value, 3)), col = "red", cex = 1.5)

# Feature Importance Visualization
imp <- as.data.frame(varImp(tree_fit)$importance)
imp$Feature <- rownames(imp)
ggplot(imp, aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Feature Importance", x = "Features", y = "Importance") +
  theme_minimal()

#---------------------------------------------------------------------------
#Logistic Regression 
#---------------------------------------------------------------------------
#------------------------------------------------
# Load necessary libraries
library(caret)
library(ggplot2)
library(pROC)

# 1. Data Preparation
# Ensure the target variable and predictors are correctly formatted
cases_train <- cases_train %>% mutate(bad = as.factor(bad)) # Ensure `bad` is a factor
cases_test <- cases_test %>% mutate(bad = as.factor(bad))

# 2. Train Logistic Regression Model
logistic_fit <- train(
  bad ~ . - county_name - state, # Exclude irrelevant features
  data = cases_train,
  method = "glm",
  family = binomial(),
  trControl = trainControl(method = "cv", number = 10)
)

# Display model summary
summary(logistic_fit)

# 3. Model Evaluation
# Predict on test data
cases_test$bad_predicted <- predict(logistic_fit, cases_test)
cases_test$bad_probs <- predict(logistic_fit, cases_test, type = "prob")[, 2]

# Confusion Matrix
conf_matrix <- confusionMatrix(data = cases_test$bad_predicted, reference = cases_test$bad)

# Convert confusion matrix to a data frame
cm_table <- as.data.frame(conf_matrix$table)
colnames(cm_table) <- c("Predicted", "Actual", "Count")

# 4. Visualizations
# Confusion Matrix Heatmap
heatmap_plot <- ggplot(cm_table, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Count), size = 6, color = "white") +
  scale_fill_gradient(low = "orange", high = "blue") +
  labs(title = "Confusion Matrix for Logistic Regression", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Metrics Table
metrics <- data.frame(
  Metric = c("Sensitivity", "Specificity", "Precision", "Recall", "Accuracy", "Kappa"),
  Value = c(
    round(conf_matrix$byClass["Sensitivity"], 3),
    round(conf_matrix$byClass["Specificity"], 3),
    round(conf_matrix$byClass["Precision"], 3),
    round(conf_matrix$byClass["Recall"], 3),
    round(conf_matrix$overall["Accuracy"], 3),
    round(conf_matrix$overall["Kappa"], 3)
  )
)
metrics_table <- tableGrob(metrics, rows = NULL)

# Combine Heatmap and Metrics Table
grid.arrange(heatmap_plot, metrics_table, nrow = 2, heights = c(3, 1))

# ROC Curve
roc_curve <- roc(cases_test$bad, cases_test$bad_probs)
plot(roc_curve, main = "ROC Curve for Logistic Regression", col = "blue", lwd = 2)
auc_value <- auc(roc_curve)
text(0.5, 0.5, paste("AUC =", round(auc_value, 3)), col = "red", cex = 1.5)

# 5. Feature Importance Visualization
# Coefficients Interpretation
coefficients <- summary(logistic_fit$finalModel)$coefficients
coefficients_df <- as.data.frame(coefficients)
coefficients_df$Feature <- rownames(coefficients_df)
coefficients_df <- coefficients_df %>% filter(Feature != "(Intercept)")

ggplot(coefficients_df, aes(x = reorder(Feature, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Feature Importance (Logistic Regression Coefficients)", 
       x = "Features", y = "Coefficient Estimate") +
  theme_minimal()

# 6. Model Interpretation

# Print the confusion matrix
print(conf_matrix)

# Print the confusion matrix
print(conf_matrix)

# Convert confusion matrix table for easy readability
cm_table <- as.data.frame(conf_matrix$table)
colnames(cm_table) <- c("Predicted", "Actual", "Count")

# Display as a formatted table for a cleaner look
library(knitr)
kable(cm_table, caption = "Confusion Matrix: Predicted vs Actual", align = "c")


# Plot predicted classification on the map
# Load the map data for U.S. counties
library(maps)
library(dplyr)

# Prepare the map data
counties <- as_tibble(map_data("county"))
counties <- counties %>%
  rename(c(county = subregion, state = region)) %>%
  mutate(state = state.abb[match(state, tolower(state.name))]) %>%
  select(state, county, long, lat, group)

# Ensure county names match for mapping
cases_test <- cases_test %>%
  mutate(county = county_name %>% str_to_lower() %>%
           str_replace('\\s+county\\s*$', ''))

# Merge logistic regression predictions with map data
mapped_data <- counties %>%
  left_join(cases_test, by = c("state", "county"))

# Plot predicted classification on the map
ggplot(mapped_data, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = bad_predicted), color = "black", size = 0.1) +
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'yellow'), name = "Predicted Class") +
  coord_quickmap() +
  labs(
    title = "Logistic Regression Predictions on U.S. Map",
    subtitle = "Classification of Counties (Bad vs Not Bad)",
    x = "", y = ""
  ) +
  theme_minimal()

# Optional: Plot probabilities instead of predicted classes
ggplot(mapped_data, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = bad_probs), color = "black", size = 0.1) +
  scale_fill_gradient(low = "yellow", high = "red", name = "Predicted Probability") +
  coord_quickmap() +
  labs(
    title = "Logistic Regression Probabilities on U.S. Map",
    subtitle = "Predicted Probability of 'Bad' Classification",
    x = "", y = ""
  ) +
  theme_minimal()
