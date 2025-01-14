#Libraries
#install.packages("DMwR")
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

# Reading the data set
covid_cases_census <- read_csv("/Users/monarchnigam/Covid data /COVID-19/COVID-19_cases_plus_census.csv")
left_fips_code <- sprintf("%05d", as.numeric(covid_cases_census$county_fips_code))


mobility_data <- read_csv("/Users/monarchnigam/Covid data /COVID-19/Global_Mobility_Report.csv")
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

# # # Install the writexl package if not already installed
# if (!require("writexl")) {
#   install.packages("writexl")
# }
# 
# # Load the writexl library
# library(writexl)
# 
# # Write the combined_data to an Excel file
# write_xlsx(combined_data, "/Users/monarchnigam/Covid data /COVID-19/left_join_combined_data.xlsx")
# 
# # Print a confirmation message
# cat("The file 'combined_data.xlsx' has been created successfully at the specified path.")
# 
# summary(combined_data)

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
#Modeling Training and Testing split
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

table(complete.cases(combined_cases_select))

#K-NN Model classification------------------------------------------------------
#MISSING VALUES 
colSums(is.na(cases_train))
colSums(is.na(cases_test))
dim(case_test)

#Dropping the Missing values 
cases_train <- cases_train %>% drop_na()
cases_test <- cases_test %>% drop_na()
summary(cases_test)

knnFit <-  cases_train |> train(bad ~ .- county_name -state,
                             method = "knn",
                             data = _,
                             preProcess = "scale",
                             tuneGrid = data.frame(k = c(1, 3, 5, 7, 9)),
                             trControl = trainControl(method = "cv"))
knnFit

# Variable Importance 
knnFit$finalModel


#Testing on the test data
cases_test$bad_predicted <- predict(knnFit, cases_test)
counties_test <- counties %>% left_join(cases_test %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
#Ground Truth 
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'yellow'))
#Predictions 
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'yellow'))

#Hyper parameter tuning has been done 
# What is being tuned?
#   
#   The hyperparameter k (number of neighbors) is being tested for values 1, 3, 5, 7, 9.
# Cross-validation (cv) is being used to evaluate each value of k.

#Confusion Matrix for evaluation
knn_conf_matrix <- confusionMatrix(data = cases_test$bad_predicted, ref = cases_test$bad)#Find a way to plot the confusion matrix 
cm_table <- as.data.frame(knn_conf_matrix$table)

# Rename columns for clarity
colnames(cm_table) <- c("Prediction", "Actual", "Count")

# Create the confusion matrix heatmap
heatmap <- ggplot(cm_table, aes(x = Actual, y = Prediction, fill = Count)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Count), size = 6, color = "white") +
  scale_fill_gradient(low = "orange", high = "blue") +
  labs(title = "Confusion Matrix for K-NN Method", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# Create a summary table for metrics
metrics <- data.frame(
  Metric = c("Sensitivity", "Specificity", "Precision", "Recall", "Accuracy", "Kappa"),
  Value = c(
    round(knn_conf_matrix$byClass["Sensitivity"], 3),
    round(knn_conf_matrix$byClass["Specificity"], 3),
    round(knn_conf_matrix$byClass["Precision"], 3),
    round(knn_conf_matrix$byClass["Recall"], 3),
    round(knn_conf_matrix$overall["Accuracy"], 3),
    round(knn_conf_matrix$overall["Kappa"], 3)
  )
)

# Create the summary table visualization
metrics_table <- tableGrob(metrics, rows = NULL)

# Combine heatmap and metrics into a single plot
grid.arrange(heatmap, metrics_table, nrow = 2, heights = c(2, 1))

#New Model Naive Bayes-------------------------------------------------------------------------------------------------
#Dropping the Missing values 
cases_train <- cases_train %>% drop_na()
cases_test <- cases_test %>% drop_na()
str(cases_test)
str(cases_train)
#Check class imbalance
#cases_train_for_NB <- cases_train %>% select(-county_name, -state)
#cases_train_for_NB %>% count(bad)
#cases_test_for_NB <- cases_test %>% select(-county_name, -state)
str(cases_train_for_NB)
str(cases_test_for_NB)


colSums(is.na(cases_train_for_NB))
#Naive Bays
#install.packages("e1071")
library(e1071)
# Train the Naive Bayes model
NBfit <- naiveBayes(bad ~ ., data = cases_train)
NBfit
# Predict on the test data
cases_test$bad_predicted <- predict(NBfit,cases_test)

cases_test_for_NB <- counties %>% left_join(cases_test %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))

NB_conf_matrix <- confusionMatrix(data = cases_test$bad_predicted, reference = cases_test$bad)
#Ground Truth 
ggplot(cases_test_for_NB, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'yellow'))
#Predictions 
ggplot(cases_test_for_NB, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'yellow'))
print(NB_conf_matrix)
#Confusion matrix plot
# Extract confusion matrix table
library(gridExtra)
cm_table <- as.data.frame(NB_conf_matrix$table)

# Rename columns for clarity
colnames(cm_table) <- c("Prediction", "Actual", "Count")

# Create the confusion matrix heatmap
heatmap <- ggplot(cm_table, aes(x = Actual, y = Prediction, fill = Count)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Count), size = 6, color = "white") +
  scale_fill_gradient(low = "orange", high = "blue") +
  labs(title = "Confusion Matrix for NBfit Method", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# Create a summary table for metrics
metrics <- data.frame(
  Metric = c("Sensitivity", "Specificity", "Precision", "Recall", "Accuracy", "Kappa"),
  Value = c(
    round(NB_conf_matrix$byClass["Sensitivity"], 3),
    round(NB_conf_matrix$byClass["Specificity"], 3),
    round(NB_conf_matrix$byClass["Precision"], 3),
    round(NB_conf_matrix$byClass["Recall"], 3),
    round(NB_conf_matrix$overall["Accuracy"], 3),
    round(NB_conf_matrix$overall["Kappa"], 3)
  )
)

# Create the summary table visualization
metrics_table <- tableGrob(metrics, rows = NULL)

# Combine heatmap and metrics into a single plot
grid.arrange(heatmap, metrics_table, nrow = 2, heights = c(2, 1))





















