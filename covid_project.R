library(tidyverse)
library(plotly)

url <- 'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv'
data <- as_tibble(read.csv(url)) %>% select(-iso_code,-continent)
data$date <- as.Date(data$date, '%Y-%m-%d')

colnames(data)
unique(data$location)

continents <- c('World','North America','Europe','European Union','South America','Asia','Africa')

countries <- data %>% filter(!(location %in% continents)) %>%
  filter(grepl((Sys.Date()-1),date))

countries

# Top 10 countries by deaths: 
top_10_deaths <- (countries%>%arrange(desc(total_deaths)))$location[1:10]
top_10_deaths

#Top 10 countries by deaths-per-million:
top_10_deathsPerMillion <- (countries%>%arrange(desc(total_deaths_per_million)))$location[1:10]

# Plot 1: Top-10 fatality countries, new deaths over time
ggplot(subset(data, location %in% top_10_deaths), aes(x=date, y=new_deaths_smoothed, color=location, na.rm=T)) +
  geom_line(na.rm=T) + ylab('New Deaths') + ylim(0,3500) + ggtitle('Top 10 Countries: New Deaths') + 
  theme(axis.title.x=element_blank(), legend.position = "bottom") +
  scale_x_date(date_breaks = '3 month',date_labels = "%b%y",limits = as.Date(c('2020-03-01','2021-05-01')))

ggplotly(ggplot(subset(data, location %in% top_10_deaths), aes(x=date, y=new_deaths_smoothed, color=location, na.rm=T)) +
    geom_line(na.rm=T) + ylab('New Deaths') + ylim(0,3500) + ggtitle('Top 10 Countries: New Deaths') + 
    theme(axis.title.x=element_blank(), legend.position = "none") +
    scale_x_date(date_breaks = '3 month',date_labels = "%b%y",limits = as.Date(c('2020-03-01','2021-05-01'))))

worldWide <- data %>% filter(data$location %in% c('World'))

ggplot( # Plot 2: Most dense fatalities vs. World trend
  subset(data, location == 'World' | location %in% top_10_deathsPerMillion), aes(x=date, y=new_deaths_smoothed_per_million, color=(location), na.rm=T)) +
  geom_line(na.rm=T) + ylab('New Deaths per Million') + ggtitle('Worldwide: New Deaths per Million') + 
  theme(axis.title.x=element_blank(), legend.position = "bottom") + ylim(0,30) +
  scale_x_date(date_breaks = '1 month',date_labels = "%b%y",limits = as.Date(c('2020-10-01',(Sys.Date()-1))))

ggplotly(ggplot( # Interactive version of plot 2
  subset(data, location == 'World' | location %in% top_10_deathsPerMillion), aes(x=date, y=new_deaths_smoothed_per_million, color=(location), na.rm=T)) +
    geom_line(na.rm=T) + ylab('New Deaths per Million') + ggtitle('Worldwide: New Deaths per Million') + 
    theme(axis.title.x=element_blank(), legend.position = "none") + ylim(0,30) +
    scale_x_date(date_breaks = '1 month',date_labels = "%b%y",limits = as.Date(c('2020-10-01',(Sys.Date()-1)))))

ggplot( # Plot 3: Most total fatalities (density) vs. World trend
  subset(data, location == 'World' | location %in% top_10_deaths), aes(x=date, y=new_deaths_smoothed_per_million, color=(location), na.rm=T)) +
  geom_line(na.rm=T) + ylab('New Deaths per Million') + ggtitle('Worldwide: New Deaths per Million') + 
  theme(axis.title.x=element_blank(), legend.position = "bottom") + ylim(0,30) +
  scale_x_date(date_breaks = '1 month',date_labels = "%b%y",limits = as.Date(c('2020-10-01',(Sys.Date()-1))))

ggplotly(ggplot( # Interactive version of plot 3
  subset(data, location == 'World' | location %in% top_10_deaths), aes(x=date, y=new_deaths_smoothed_per_million, color=(location), na.rm=T)) +
  geom_line(na.rm=T) + ylab('New Deaths per Million') + ggtitle('Worldwide: New Deaths per Million') + 
  theme(axis.title.x=element_blank(), legend.position = "none") + ylim(0,NA) +
  scale_x_date(date_breaks = '1 month',date_labels = "%b%y",limits = as.Date(c('2020-10-01',(Sys.Date()-1)))))











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
