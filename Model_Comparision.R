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


# Reading the data set
covid_cases_census <- read_csv("/Users/monarchnigam/Covid data /COVID-19/COVID-19_cases_plus_census.csv")
covid_cases_census <- covid_cases_census %>% mutate_if(is.character, factor)
left_fips_code <- sprintf("%05d", as.numeric(covid_cases_census$county_fips_code))


mobility_data <- read_csv("/Users/monarchnigam/Covid data /COVID-19/Global_Mobility_Report.csv")
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
#TRAIN TEST DATA Split------------------------
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

#-----------------------------------------------------------------------

#Comparision of different models 


# Convert the 'bad' variable to have valid factor levels
cases_train$bad <- factor(cases_train$bad, levels = c("TRUE", "FALSE"), labels = c("Yes", "No"))
cases_test$bad <- factor(cases_test$bad, levels = c("TRUE", "FALSE"), labels = c("Yes", "No"))


ctrl <- trainControl(
  method = "cv",          # Use k-fold cross-validation
  number = 10,            # Number of folds
  savePredictions = TRUE  # Save predictions for resampling
)
knnFit <- train(bad ~ . - county_name - state, 
                data = cases_train, 
                method = "knn", 
                trControl = ctrl)

tree_fit <- train(bad ~ . - county_name - state, 
                  data = cases_train, 
                  method = "rpart", 
                  trControl = ctrl)

rf_fit <- train(bad ~ . - county_name - state, 
                data = cases_train, 
                method = "rf", 
                trControl = ctrl)


baselineFit <- resamples(list(
  KNN = knnFit,
  DecisionTree = tree_fit,
  RandomForest = rf_fit
))

baselineFit$metrics
summary(baselineFit)

# Boxplot for Accuracy
bwplot(baselineFit, metric = "Accuracy", 
       par.settings = list(box.umbrella = list(lwd = 2)), 
       main = "Accuracy Comparison")

# Boxplot for Kappa
bwplot(baselineFit, metric = "Kappa", 
       par.settings = list(box.umbrella = list(lwd = 2)), 
       main = "Kappa Comparison")

difs <- diff(baselineFit)
summary(difs)







