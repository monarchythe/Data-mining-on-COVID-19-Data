library(tidyverse)
library(ggplot2)
library(dplyr)
library(maps)
library(ggcorrplot)
library(DT)
library(ggrepel)
library(htmlwidgets)

cases_NY_overtime <- read_csv("/Users/monarchnigam/Covid data /COVID-19/CA_data.csv")
cases_NY_overtime <- cases_NY_overtime %>% mutate_if(is.character, factor)
summary(cases_NY_overtime)
head(cases_NY_overtime)
unique(cases_NY_overtime$county_name)
cases_nycity <- cases_NY_overtime %>% filter(county_name == "Kings County" & state == "CA"& date<"2021-02-22")#& date<"2021-02-22"
cases_nycity <- cases_nycity %>% mutate(
  
  death_per_case = deaths/confirmed_cases
)




ggplot(cases_nycity, aes(x = date, y = death_per_case)) + 
  geom_line() + 
  geom_smooth() +
  labs(y = "deaths per case")


#Now from the mobility data we need to find out the OR VS NY Mobility and lock down and CA and NY NJ mobility
# Then we can see what happened how different states behaved near the holiday times etc .

Mobility_data <- read_csv("/Users/monarchnigam/Covid data /COVID-19/Global_Mobility_Report.csv", col_types =  cols(sub_region_2 = col_character()))
str(Mobility_data)
Mobility_data <- Mobility_data %>% mutate_if(is.character, factor)
dim(Mobility_data)
head(Mobility_data)
summary(Mobility_data) # We can see loot if empty values
mobility_NewYork <- Mobility_data %>% filter(sub_region_1 == "California")
dim(mobility_NewYork)
head(mobility_NewYork)
# Finding the SUB regions
unique(mobility_NewYork$sub_region_2)

NewYorkCity_mobility <- Mobility_data %>% filter(sub_region_1 == "California" & sub_region_2=="Kings County")
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









