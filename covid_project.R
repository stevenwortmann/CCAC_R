library(tidyverse)
library(plotly)

url <- 'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv'
data <- as_tibble(read.csv(url))

colnames(data)
unique(data$location)

usa <- data %>% # Current United States data
  select(iso_code, continent, location, date, total_cases, new_cases, total_deaths, total_deaths_per_million, new_deaths) %>%
  filter(grepl('United States', location)) #%>% arrange(desc(date))

countries <- data %>% # Key data for all countries
  select(location, date, total_cases, total_cases_per_million, total_deaths, total_deaths_per_million, population_density, gdp_per_capita, median_age, human_development_index, aged_65_older, aged_70_older) %>%
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

# T-test: Comparing fatalities of lowest and highest 20 aged-70+ countries...
t.test((countries%>%arrange(aged_70_older))$total_deaths_per_million[1:20],
       (countries%>%arrange(desc(aged_70_older)))$total_deaths_per_million[1:20])

# T-test: Comparing fatalities of 20 lowest GDP/capital and 20 oldest median age countries...
t.test((countries%>%arrange(gdp_per_capita))$total_deaths_per_million[1:20],
       (countries%>%arrange(desc(aged_70_older)))$total_deaths_per_million[1:20])

ggplot(usa, aes(x=date, y=total_deaths, color="#990239")) + geom_point() +
  xlab('Time') + ylab('Total Deaths') + theme(legend.position = "none") +
  ggtitle('USA: Covid Deaths')

ggplotly(ggplot(data %>% filter(grepl('United States', location)), 
  aes(x=date, y=new_deaths, color="#990239")) + geom_point() +
  xlab('Time') + ylab('New Deaths') + theme(legend.position = "none") +
  ggtitle('USA: Covid New Deaths (Daily)'))

ggplotly(ggplot(data %>% filter(grepl('Brazil', location)), 
  aes(x=date, y=new_deaths, color="#990239")) + geom_point() +
  xlab('Time') + ylab('New Deaths') + theme(legend.position = "none") +
  ggtitle('Brazil: Covid New Deaths (Daily)'))

ggplotly(ggplot(data %>% filter(grepl('India', location)), 
  aes(x=date, y=new_deaths, color="#990239")) + geom_point() +
  xlab('Time') + ylab('New Deaths') + theme(legend.position = "none") +
  ggtitle('India: Covid New Deaths (Daily)'))

ggplotly(ggplot(data %>% filter(grepl('India', location)), 
  aes(x=date, y=new_cases, color="#990239")) + geom_point() +
  xlab('Time') + ylab('New Cases') + theme(legend.position = "none") +
  ggtitle('India: Covid New Cases (Daily)'))

gplotly(ggplot(data %>% filter(grepl('India', location)), 
  aes(x=date, y=new_deaths, color="#990239")) + geom_point() +
  xlab('Time') + ylab('New Deaths') + theme(legend.position = "none") +
  ggtitle('India: Covid New Deaths (Daily)'))
