library(tidyverse)

data <- as_tibble(read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv'))

unique(data$location)

data %>% 
  select(iso_code, continent, location, date, total_cases, new_cases, total_deaths, new_deaths) %>%
  filter(grepl('United States', location)) %>% arrange(desc(date))

