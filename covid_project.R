library(tidyverse)
library(plotly)
library(rsconnect)

url <- 'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv'
data <- as_tibble(read.csv(url))
data$date <- as.Date(data$date, '%Y-%m-%d')

data_cols <- colnames(data)
unique(data$location)

usa <- data %>% # Current United States data
  select(iso_code, continent, location, date, total_cases, new_cases, total_deaths, total_deaths_per_million, new_deaths) %>%
  filter(grepl('United States', location)) #%>% arrange(desc(date))

countries <- data %>% # Key data for all countries
  select(location, date, new_cases, new_deaths, total_cases, total_cases_per_million, total_deaths, total_deaths_per_million, population_density, gdp_per_capita, median_age, human_development_index, aged_65_older, aged_70_older) %>%
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

ggplotly(ggplot(data %>% filter(grepl('India', location)), 
  aes(x=date, y=new_deaths, color="#990239")) + geom_point() +
  xlab('Time') + ylab('New Deaths') + theme(legend.position = "none") +
  ggtitle('India: Covid New Deaths (Daily)'))



continents <- c('World', 'North America', 'Europe', 'European Union', 'South America',
                'Asia', 'Africa')

top_10 <- (countries%>%arrange(desc(total_deaths)) %>% 
             filter(!(location %in% continents)))$location[1:10]

top_20 <- (countries%>%arrange(desc(total_deaths)) %>% 
             filter(!(location %in% continents)))$location[1:20]

top_10
top_20


# New deaths smoothed, top 10 countries
ggplot(subset(data, location %in% top_10), aes(x=date, y=new_deaths_smoothed, color=location)) + geom_line() +
           xlab('Time') + ylab('New Deaths') + ylim(0,4000) + ggtitle('Daily Covid Deaths Worldwide') +
           theme(legend.position = "bottom") + scale_x_date(date_breaks = '1 month',date_labels = "%b%y", limits = as.Date(c('2020-02-01',(Sys.Date()-1))))
ggplotly(ggplot(subset(data, location %in% top_10), aes(x=date, y=new_deaths_smoothed, color=location)) + geom_line() +
           xlab('Time') + ylab('New Deaths') + ylim(0,4000) + ggtitle('Daily Covid Deaths Worldwide') +
           theme(legend.position = "bottom") + scale_x_date(date_breaks = '1 month',date_labels = "%b%y", limits = as.Date(c('2020-02-01',(Sys.Date()-1)))))

# New deaths/million smoothed, top 20 countries
ggplot(subset(data, location %in% top_20), aes(x=date, y=new_deaths_smoothed_per_million, color=location)) + geom_line() +
           xlab('Time') + ylab('New Deaths/Million') + ylim(0,25) + ggtitle('Daily Covid Deaths per Million, Worldwide') +
           theme(legend.position = "bottom") + scale_x_date(date_breaks = '1 month',date_labels = "%b%y", limits = as.Date(c('2020-02-01',(Sys.Date()-1))))
