library(tidyverse)
library(plotly)
library(rsconnect)

url <- 'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv'
data <- as_tibble(read.csv(url))
data$date <- as.Date(data$date, '%Y-%m-%d')

colnames(data)
unique(data$location)

continents <- c('North America', 'Europe', 'European Union', 'South America','Asia', 'Africa')

# All aggregated world data
world <- data %>% filter((location %in% 'World')) %>%
  select(-iso_code,-continent) %>% arrange(desc(date))

# All aggregated continental data
continental <- data %>% filter((location %in% continents)) %>%
  select(-iso_code,-continent) %>% arrange(desc(date))

# All USA data
usa <- data %>% filter((location %in% 'United States')) %>%
  select(-iso_code,-continent) %>% arrange(desc(date))

# Current USA data
usa %>% filter(grepl((Sys.Date()-1),date))

# Current countries data, ranked most new deaths per million
countries <- data %>% filter(!(location %in% continents)) %>%
  select(-iso_code,-continent) %>% filter(grepl((Sys.Date()-1),date)) %>% arrange(desc(new_deaths_per_million))

# top 10 countries by fatality
top_10 <- (countries%>%arrange(desc(total_deaths)) %>% 
             filter(!(location %in% continents)))$location[1:10]

# top 20 countries by fatality
top_20 <- (countries%>%arrange(desc(total_deaths)) %>% 
             filter(!(location %in% continents)))$location[1:20]

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
