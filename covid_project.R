library(tidyverse)

url <- 'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv'
data <- as_tibble(read.csv(url))

colnames(data)

unique(data$location)
unique(data$continent)[-2]

data %>% # Current United States data
  select(iso_code, continent, location, date, total_cases, new_cases, total_deaths, new_deaths) %>%
  filter(grepl('United States', location)) %>% arrange(desc(date))

data %>% # Current total cases, deaths, median age all countries
  select(location, date, total_cases, total_cases_per_million, total_deaths, total_deaths_per_million, median_age) %>%
  filter(grepl((Sys.Date()-1),date)) %>% arrange(desc(total_deaths_per_million))

high_death <- data %>% # Current total cases, deaths, median age all countries - sorted most deaths
  select(location, date, total_cases, total_cases_per_million, total_deaths, total_deaths_per_million, median_age) %>%
  filter(grepl((Sys.Date()-1),date)) %>% arrange(desc(total_deaths_per_million))

high_age <- data %>% # Current total cases, deaths, median age all countries - sorted highest age
  select(location, date, total_cases, total_cases_per_million, total_deaths, total_deaths_per_million, median_age) %>%
  filter(grepl((Sys.Date()-1),date)) %>% arrange(desc(median_age))

# T-test comparing top-50 fatalities to top-50 median age
t.test(high_death$total_deaths_per_million[1:50], high_age$total_deaths_per_million[1:50])
