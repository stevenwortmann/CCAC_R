library(tidyverse)

url <- 'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv'
data <- as_tibble(read.csv(url))

colnames(data)
unique(data$location)

data %>% # Current United States data
  select(iso_code, continent, location, date, total_cases, new_cases, total_deaths, new_deaths) %>%
  filter(grepl('United States', location)) %>% arrange(desc(date))

countries <- data %>% # Key data for all countries
  select(location, date, total_cases, total_cases_per_million, total_deaths, total_deaths_per_million, population_density, gdp_per_capita, median_age, human_development_index) %>%
  filter(grepl((Sys.Date()-1),date)) %>% arrange(desc(total_deaths_per_million))

countries

colnames(countries) # Total cases and deaths per million, population density, GDP/capita, median age, human dev. index

# T-test: Comparing fatalities of lowest and highest median age countries...
t.test((countries%>%arrange((median_age)))$total_deaths_per_million[1:20],
       (countries%>%arrange(desc(median_age)))$total_deaths_per_million[1:20])

# T-test: Comparing fatalities of lowest and highest population density countries...
t.test((countries%>%arrange(population_density))$total_deaths_per_million[1:20],
       (countries%>%arrange(desc(population_density)))$total_deaths_per_million[1:20])

# T-test: Comparing fatalities of lowest and highest 20 HDI countries...
t.test((countries%>%arrange(human_development_index))$total_deaths_per_million[1:20],
       (countries%>%arrange(desc(human_development_index)))$total_deaths_per_million[1:20])

# T-test: Comparing fatalities of lowest and highest 20 GDP/capita countries...
t.test((countries%>%arrange(gdp_per_capita))$total_deaths_per_million[1:20],
       (countries%>%arrange(desc(gdp_per_capita)))$total_deaths_per_million[1:20])

