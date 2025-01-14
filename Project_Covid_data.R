#Importing the libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(maps)
library(ggcorrplot)
library(DT)
library(ggrepel)
library(htmlwidgets)

# Reading the data set
covid_cases_census <- read_csv("/Users/monarchnigam/Covid data /COVID-19/COVID-19_cases_plus_census.csv",)
#cases_texas <- read_csv("/Users/monarchnigam/Covid data /COVID-19/COVID-19_cases_TX.csv",)

str(covid_cases_census)
colnames(covid_cases_census)

#Converting the data set to tibble 
covid_cases_census <- covid_cases_census %>% mutate_if(is.character, factor)
covid_census_tibble <- as_tibble(covid_cases_census)

#Income VS Deaths per state
summary(covid_cases_census$median_income)
#Aggregating the cleaned data of median income and deaths per state 
income_vs_cases_per_state <- covid_census_tibble %>%
  group_by(state) %>%
  summarise(
    median_income_perState = mean(median_income, na.rm = TRUE),
    deaths_perState = sum(deaths, na.rm = TRUE),
    population_perState = sum(total_pop, na.rm = TRUE),
    deaths_perState = sum(deaths, na.rm = TRUE),
    cases_perState = sum(confirmed_cases, na.rm = TRUE),
    
    #Death and cases percentage 
    death_percentage = (deaths_perState/population_perState)*100,
    cases_percentage = (cases_perState/population_perState)*100,
  )

income_vs_cases_per_state
ggplot(income_vs_cases_per_state, aes(x = reorder(state, -median_income_perState), y = median_income_perState)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Median Income per State", x = "State", y = "Median Income") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(income_vs_cases_per_state, aes(x = reorder(state, -death_percentage), y = death_percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Death per State", x = "State", y = "Death Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(income_vs_cases_per_state, aes(x = reorder(state, -cases_percentage), y = cases_percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Cases per State", x = "State", y = "Cases Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Finding the correlation coefficient 
correlation_income_vs_deaths =  cor(income_vs_cases_per_state$median_income_perState, income_vs_cases_per_state$death_percentage)
correlation_income_vs_deaths

ggplot(income_vs_cases_per_state, aes(x = median_income_perState, y = death_percentage)) +
  geom_point(color = "red", size = 3) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  geom_text(aes(label = state), vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(title = "Correlation between Median Income and Deaths is 0.084", x = "Median Income", y = "Deaths")
#Observation in the report------------------------------------------------------------------------
#So Income we are not considering-----------------------------------------------------------------
#Next I want to look at poverty 
Total_population_USA = sum(covid_census_tibble$total_pop)
Total_population_USA
population_vs_cases_vs_deaths <- covid_census_tibble %>%
  group_by(state) %>%
  summarise(
    population_perState = sum(total_pop, na.rm = TRUE),
    poverty_perState = sum(poverty, na.rm = TRUE),
    deaths_perState = sum(deaths, na.rm = TRUE),
    cases_perState = sum(confirmed_cases, na.rm = TRUE),
    
    #Calculations
    popultion_percentage = (population_perState/Total_population_USA)*100,
    death_percentage = (deaths_perState/population_perState)*100,
    cases_percentage = (cases_perState/population_perState)*100,
    poverty_percentage = (poverty_perState/population_perState)*100,

  )
population_vs_cases_vs_deaths
#Plotting histogram of population and their poverty population

ggplot(population_vs_cases_vs_deaths, aes(x = reorder(state, -poverty_percentage), y = poverty_percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "poverty per State", x = "State", y = "Poverty Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(population_vs_cases_vs_deaths, aes(x = reorder(state, -popultion_percentage), y = popultion_percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "popupation per State", x = "State", y = "population Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#We already have death percentage histogram

#Correlation Poverty percentage vs deaths
correlation_poverty_vs_deathsPercentage =  cor(population_vs_cases_vs_deaths$poverty_percentage, population_vs_cases_vs_deaths$death_percentage)
correlation_poverty_vs_deathsPercentage
correlation_poverty_vs_casesPercentage =  cor(population_vs_cases_vs_deaths$poverty_percentage, population_vs_cases_vs_deaths$cases_percentage)
correlation_poverty_vs_casesPercentage
#ploting 
ggplot(population_vs_cases_vs_deaths, aes(x = poverty_percentage, y = cases_percentage)) +
  geom_point(color = "tan3", size = 3) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  geom_text(aes(label = state), vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(title = "Correlation between Poverty% per State and Cases%", x = "Poverty Percentage", y = "Cases Percentage")
ggplot(population_vs_cases_vs_deaths, aes(x = poverty_percentage, y = death_percentage)) +
  geom_point(color = "tan3", size = 3) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  geom_text(aes(label = state), vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(title = "Correlation between Poverty % and Deaths % per State",
       x = "Poverty Percentage", y = "Deaths Percentage")

#So the cor = 0.234-which means we can look into this and find some patterns. Later in this code I have selected some states to look at.
#I have Ploted the maps and from the maps i have selected specific counties to find any patters and plot them.(Later in to code)

#---next--We are going to look upon Racial vs deaths-----------------------------
Racial_factors_VS_Deaths <- covid_census_tibble %>%
  group_by(state) %>%
  summarise(
    population_perState = sum(total_pop, na.rm = TRUE),
    poverty_perState = sum(poverty, na.rm = TRUE),
    deaths_perState = sum(deaths, na.rm = TRUE),
    cases_perState = sum(confirmed_cases, na.rm = TRUE),
    hispanic_pop_perState = sum(hispanic_pop, na.rm = TRUE),
    black_pop_perState = sum(black_pop, na.rm = TRUE),
    asian_pop_perState = sum(asian_pop, na.rm = TRUE),
    amerindian_pop_perState = sum(amerindian_pop, na.rm = TRUE),
    
    #Calculating the percentages 
    poverty_percentage = (poverty_perState/population_perState)*100,
    death_percentage = (deaths_perState/population_perState)*100,
    cases_percentage = (cases_perState/population_perState)*100,
    
    # Calculating total of Hispanic and Black populations combined per state
    total_hispanic_black_pop_perState = hispanic_pop_perState + black_pop_perState,
    
    # Calculating the Racial population percentage in each state
    hispanic_pop_percentage = (hispanic_pop_perState/population_perState)*100,
    balck_pop_percentage = (black_pop_perState/population_perState)*100,
    hispanic_n_black_pop_percentage = (total_hispanic_black_pop_perState/population_perState)*100
)
Racial_factors_VS_Deaths$black_pop_perState
Racial_factors_VS_Deaths$hispanic_pop_perState
Racial_factors_VS_Deaths$total_hispanic_black_pop_perState
Racial_factors_VS_Deaths$balck_pop_percentage
Racial_factors_VS_Deaths$hispanic_pop_percentage
Racial_factors_VS_Deaths$hispanic_n_black_pop_percentage



#plotting individual population state wise
ggplot(Racial_factors_VS_Deaths, aes(x = reorder(state, -hispanic_pop_percentage), y = hispanic_pop_percentage)) +
  geom_bar(stat = "identity", fill = "coral3") +
  theme_minimal() +
  labs(title = "hispanic_pop per State", x = "State", y = "hispanic population percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(Racial_factors_VS_Deaths, aes(x = reorder(state, -balck_pop_percentage), y = balck_pop_percentage)) +
  geom_bar(stat = "identity", fill = "coral3") +
  theme_minimal() +
  labs(title = "black_pop per State", x = "State", y = "black population percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(Racial_factors_VS_Deaths, aes(x = reorder(state, -hispanic_n_black_pop_percentage), y = hispanic_n_black_pop_percentage)) +
  geom_bar(stat = "identity", fill = "coral3") +
  theme_minimal() +
  labs(title = "total_hispanic and black_pop_perState", x = "State", y = "total_hispanic and black population percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Finding correlations between different racial population and deaths and cases 
hispanic_n_deaths = cor(Racial_factors_VS_Deaths$hispanic_pop_percentage, Racial_factors_VS_Deaths$death_percentage)
hispanic_n_deaths
black_n_deaths = cor(Racial_factors_VS_Deaths$balck_pop_percentage, Racial_factors_VS_Deaths$death_percentage)
black_n_deaths
Hispanic_n_Blacks = cor(Racial_factors_VS_Deaths$hispanic_n_black_pop_percentage, Racial_factors_VS_Deaths$death_percentage)
Hispanic_n_Blacks
#So here we can see the strongest correlation in the number of deaths and the black and hispanic population
#plotting
ggplot(Racial_factors_VS_Deaths, aes(x = hispanic_n_black_pop_percentage, y = death_percentage)) +
  geom_point(color = "tan3", size = 3) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  geom_text(aes(label = state), vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(title = "Correlation between Total Hispanic and Balack Population % and Deaths % per State", 
       x = "Total Hispanic and black Pop Percentage", y = "Deaths Percentage")

#------------------------------------------------------------------------------------------------------
# So now we will look at each state. based on the poverty vs death graph we are interseted in NY and OR counties
# also NJ and WA counties, because NY AND NJ performed poorly having same poverty % as OR and WA

#So now we have a list of states the did good, even after having high Black and hispanic population  -->
#    CA, VA but NJ AND NY again are the outliners 
# States which have lower racial population and lower poverty and lower death rates
#  WA, WY, HI, 
# The states which did poor on deaths -
#    TX,DC, LA, NJ, NY
#-----------------------------------------------------------------------------------------

#Filtering data:------------------------
States_we_interested_in = 
cases_NY <- covid_cases_census %>% filter(state == "NY")
dim(cases_NY)
cases_NY
#Cleaning the data
# Checking for missing values in the entire dataset
cases_NY %>%
  summarise_all(~ sum(is.na(.))) #So there are no missing values 
# Checking the duplicate rows 
cases_NY %>%
  filter(duplicated(.))

cases_NY %>%
  summarise(
    missing_fips = sum(is.na(county_fips_code)),
    duplicated_fips = sum(duplicated(county_fips_code))
  )
# Calculate the total population of New York State
total_population_NY <- sum(cases_NY$total_pop, na.rm = TRUE)

cases_NY_select <- cases_NY %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, 
         median_income,hispanic_pop, black_pop, poverty)

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
  population_percentage = (total_pop / total_population_NY) * 100
)
  

head(cases_NY_select)
datatable(cases_NY_select) %>% formatRound(6:7, 4) %>% formatPercentage(8, 2)

#Correlated
cor_NY <- cor(cases_NY_select[,-1])
ggcorrplot(cor_NY, p.mat = cor_pmat(cases_NY_select[,-1]), insig = "blank", hc.order = TRUE)



#Ploting couty wise map
counties <- as_tibble(map_data("county"))
counties_NY <- counties %>% dplyr::filter(region == "new york") %>% rename(c(county = subregion))

cases_NY <- cases_NY_select %>% mutate(county = county_name %>% str_to_lower() %>% 
                                         str_replace('\\s+county\\s*$', ''))

counties_NY <- counties_NY %>% left_join(cases_NY %>% 
                                           select(c(county,cases_percent,deaths_percent, death_per_case,
                                                    hispanic_pop_percentage,black_pop_percentage,
                                                    hispanic_black_pop_percentage,poverty_percentage,population_percentage)))

#Plotting with population % on the map
ggplot(counties_NY, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = population_percentage), color = "white") +
  
  # Use geom_text_repel to display population percentage on the map
  geom_text_repel(data = counties_NY %>% filter(complete.cases(population_percentage)) %>% 
                    group_by(county) %>%
                    summarize(long = mean(long), lat = mean(lat), 
                              population_percentage = mean(population_percentage)),
                  aes(label = round(population_percentage, 2)), 
                  size = 3, color = "black") +  # Adjust size and color as needed
  
  coord_quickmap() + 
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Population Percentage by County in New York", 
       subtitle = "Only counties reporting 100+ cases",
       fill = "Population %") +
  theme_minimal()

#california
cases_CA <- covid_cases_census %>% filter(state == "CA")
dim(cases_CA)
cases_CA
#Cleaning the data
# Checking for missing values in the entire dataset
cases_CA %>%
  summarise_all(~ sum(is.na(.))) #So there are no missing values 
# Checking the duplicate rows 
cases_CA %>%
  filter(duplicated(.))

cases_CA %>%
  summarise(
    missing_fips = sum(is.na(county_fips_code)),
    duplicated_fips = sum(duplicated(county_fips_code))
  )


cases_CA_select <- cases_CA %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, 
         median_income,hispanic_pop, black_pop, poverty)

cases_CA_select <- cases_CA_select %>% mutate(
  
  cases_percent = (confirmed_cases/total_pop)*100, 
  deaths_percent = (deaths/total_pop)*100, 
  death_per_case = deaths/confirmed_cases,
  
  # New calculations for Hispanic and Black population percentages
  hispanic_pop_percentage = (hispanic_pop / total_pop) * 100,
  black_pop_percentage = (black_pop / total_pop) * 100,
  
  # Sum of Hispanic and Black population percentages
  hispanic_black_pop_percentage = ((hispanic_pop + black_pop) / total_pop) * 100,
  
  # Poverty percentage
  poverty_percentage = (poverty / total_pop) * 100
)


head(cases_CA_select)
datatable(cases_CA_select) %>% formatRound(6:7, 4) %>% formatPercentage(8, 2)

#Correlated
cor_CA <- cor(cases_CA_select[,-1])
ggcorrplot(cor_CA, p.mat = cor_pmat(cases_CA_select[,-1]), insig = "blank", hc.order = TRUE)



#Ploting couty wise map
counties <- as_tibble(map_data("county"))
counties_CA <- counties %>% dplyr::filter(region == "california") %>% rename(c(county = subregion))

cases_CA <- cases_CA_select %>% mutate(county = county_name %>% str_to_lower() %>% 
                                         str_replace('\\s+county\\s*$', ''))

counties_CA <- counties_CA %>% left_join(cases_CA %>% 
                                           select(c(county,cases_percent,deaths_percent, death_per_case,
                                                    hispanic_pop_percentage,black_pop_percentage,
                                                    hispanic_black_pop_percentage,poverty_percentage)))

ggplot(counties_CA, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = poverty_percentage)) +
  # geom_text_repel(data = counties_NY %>% filter(complete.cases(.)) %>% group_by(county) %>% 
  #    summarize(long = mean(long), lat = mean(lat)) %>% mutate(county = str_to_title(county))) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Cases per 1000 People", 
       subtitle = "Only counties reporting 100+ cases",
       fill = "cases percent")


# FOR LOOPING Throgh each and every state we are interested in ----------------------------------------------------------

# List of target states
target_states <- c("new york", "new jersey", "oregon", "washington","california")
corr_plots <- list()

# Loop through each state
for (state in target_states) {
  
  # Convert state names to title case (to match R's built-in state.name)
  target_states_title_case <- stringr::str_to_title(state)
  
  # Create a vector of 2-letter abbreviations for the target states
  state_abbr <- state.abb[match(target_states_title_case, state.name)]
  
  # View the new array of abbreviations
  print(state_abbr)
  
  # Filtering data for the current state
  cases_state <- covid_cases_census %>% filter(state == state_abbr)
  
  
  # Cleaning the data
  # Checking for missing values in the entire dataset
  cases_state %>%
    summarise_all(~ sum(is.na(.)))
  
  # Checking the duplicate rows
  cases_state %>%
    filter(duplicated(.))
  
  cases_state %>%
    summarise(
      missing_fips = sum(is.na(county_fips_code)),
      duplicated_fips = sum(duplicated(county_fips_code))
    )
  
  # Calculate the total population of the state
  total_population_state <- sum(cases_state$total_pop, na.rm = TRUE)
  
  # Selecting and cleaning data
  cases_state_select <- cases_state %>% 
    filter(confirmed_cases > 100) %>% 
    arrange(desc(confirmed_cases)) %>%    
    select(county_name, confirmed_cases, deaths, total_pop, 
           median_income, hispanic_pop, black_pop, poverty) %>%
    mutate(
      cases_percent = (confirmed_cases / total_pop) * 100, 
      deaths_percent = (deaths / total_pop) * 100, 
      death_per_case = deaths / confirmed_cases,
      
      # New calculations for Hispanic and Black population percentages
      hispanic_pop_percentage = (hispanic_pop / total_pop) * 100,
      black_pop_percentage = (black_pop / total_pop) * 100,
      
      # Sum of Hispanic and Black population percentages
      hispanic_black_pop_percentage = ((hispanic_pop + black_pop) / total_pop) * 100,
      
      # Poverty percentage
      poverty_percentage = (poverty / total_pop) * 100,
      
      # Population percentage per county
      population_percentage = (total_pop / total_population_state) * 100
    )
  
  # # Display data (datatable)
  # state_table <- datatable(cases_state_select) %>% 
  #   formatRound(6:7, 4) %>% 
  #   formatPercentage(8, 2)
  # 
  # #Saving the datatable
  # saveWidget(state_table, paste0("datatable_", state, ".html"), selfcontained = TRUE)
  
  # cor_state <- cor(cases_state_select[,-1], use = "complete.obs")
  # p_mat <- cor_pmat(cases_state_select[,-1])
  # corr_plot <- ggcorrplot(cor_state, p.mat = p_mat, 
  #                         insig = "blank", hc.order = TRUE, lab = TRUE, lab_size = 1.5) +
  #   labs(title = paste("Correlation Plot for", str_to_title(state))) +
  #   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6),
  #         axis.text.y = element_text(size = 6))
  # 
  # 
  # # Store the correlation plot in the list
  # corr_plots[[state]] <- corr_plot
  # 
  # Plotting county-wise map
  counties <- as_tibble(map_data("county"))
  
  # Filter county map data for the current state
  counties_state <- counties %>% 
    filter(region == state) %>% 
    rename(county = subregion)
  
  cases_state <- cases_state_select %>% 
    mutate(county = county_name %>% 
             str_to_lower() %>% 
             str_replace('\\s+county\\s*$', ''))
  
  counties_state <- counties_state %>% 
    left_join(cases_state %>% 
                select(county, cases_percent, deaths_percent, death_per_case,
                       hispanic_pop_percentage, black_pop_percentage,
                       hispanic_black_pop_percentage, poverty_percentage, 
                       population_percentage))
  
  # Plot population % on the map
  ggplot(counties_state, aes(long, lat, label = county)) + 
    geom_polygon(aes(group = group, fill = hispanic_black_pop_percentage), color = "white") +
    
    # Use geom_text_repel to display population percentage on the map
    geom_text_repel(data = counties_state %>% 
                      filter(complete.cases(hispanic_black_pop_percentage)) %>% 
                      group_by(county) %>%
                      summarize(long = mean(long), lat = mean(lat), 
                                hispanic_black_pop_percentage = mean(hispanic_black_pop_percentage)),
                    aes(label = round(hispanic_black_pop_percentage, 2)), 
                    size = 6, color = "black", max.overlaps = 100) +
    
    coord_quickmap() + 
    scale_fill_gradient(low = "yellow", high = "red") +
    labs(title = paste("Hispanic and Black Population% in", str_to_title(state)), 
         subtitle = "Counties reporting 100+ cases",
         fill = "Hispanic and Black Population%") +
    theme_minimal()  +
    theme(
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 10)
    )
  
  
  # Save each plot for each state (optional)
  ggsave(paste0("Hispanic and Black Population_percentage_map_", state, ".png"),
         width = 12, height = 9, units = "in",
         bg = "white")
}

corr_plots[1]


#--Downloading sates cases and deaths over time period data-----------------

cases_NY_overtime <- read_csv("/Users/monarchnigam/Covid data /COVID-19/NY_State_Overtime_Data.csv")
cases_NY_overtime <- cases_NY_overtime %>% mutate_if(is.character, factor)
summary(cases_NY_overtime)
head(cases_NY_overtime)
unique(cases_NY_overtime$county_name)
cases_nycity <- cases_NY_overtime %>% filter(county_name == "New York County" & state == "NY"& date<"2021-02-22")#& date<"2021-02-22"
cases_nycity <- cases_nycity %>% mutate(
  
  death_per_case = deaths/confirmed_cases
)

ggplot(cases_nycity, aes(x = date, y = death_per_case)) + 
  geom_line() + 
  geom_smooth() +
  labs(y = "deaths per case")


#mobility Data.

Mobility_data <- read_csv("/Users/monarchnigam/Covid data /COVID-19/Global_Mobility_Report.csv", col_types =  cols(sub_region_2 = col_character()))
str(Mobility_data)
Mobility_data <- Mobility_data %>% mutate_if(is.character, factor)
dim(Mobility_data)
head(Mobility_data)
summary(Mobility_data) # We can see loot if empty values
mobility_NewYork <- Mobility_data %>% filter(sub_region_1 == "New York")
dim(mobility_NewYork)
head(mobility_NewYork)
# Finding the SUB regions
unique(mobility_NewYork$sub_region_2)

NewYorkCity_mobility <- Mobility_data %>% filter(sub_region_1 == "New York" & sub_region_2=="New York County")
dim(NewYorkCity_mobility)
head(NewYorkCity_mobility)

ggplot(NewYorkCity_mobility, mapping = aes(x = date, y = retail_and_recreation_percent_change_from_baseline)) + 
  geom_line() + 
  geom_smooth() +
  labs(y = "retail and recreation change in percent")

ggplot(NewYorkCity_mobility, mapping = aes(x = date, y = grocery_and_pharmacy_percent_change_from_baseline)) + 
  geom_line() + 
  geom_smooth() +
  labs(y = "grocery and pharmacy change in percent")

ggplot(NewYorkCity_mobility, mapping = aes(x = date, y = parks_percent_change_from_baseline)) + 
  geom_line() + 
  geom_smooth() +
  labs(y = "Parks change in percent")
ggplot(NewYorkCity_mobility, mapping = aes(x = date, y = transit_stations_percent_change_from_baseline)) + 
  geom_line() + 
  geom_smooth() +
  labs(y = "Transit Stations change in percent")





