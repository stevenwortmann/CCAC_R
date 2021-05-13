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

# Average age of 10 most densely-fatal countries: 42.17778
mean((countries %>% arrange(desc(total_deaths_per_million)))$median_age[1:10],na.rm=T)

# Average age of countries with 10 highest median age: 45.81
mean((countries %>% arrange(desc(median_age)))$median_age[1:10],na.rm=T)

(countries %>% arrange(desc(median_age)))$location[1:50] # Oldest 50 countries on Earth
mean((countries %>% arrange(desc(median_age)))$median_age[1:50]) # Their age: 42.292

(countries %>% arrange((median_age)))$location[1:50] # Youngest 50 countries on Earth
mean((countries %>% arrange((median_age)))$median_age[1:50]) # Their age: 19.068

# T-test: Comparing fatalities of lowest and highest median age countries...

t.test((countries%>%arrange((median_age)))$total_deaths_per_million[1:50],
       (countries%>%arrange(desc(median_age)))$total_deaths_per_million[1:50])
#t = -9.1265, df = 48.279, p-value = 4.383e-12
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1397.7304  -893.1145
#sample estimates:
#  mean of x mean of y 
#77.8436 1223.2660 

(countries %>% arrange(desc(population_density)))$location[1:50] # 50 most population-dense countries
mean((countries %>% arrange(desc(population_density)))$population_density[1:50]) # 1511.26 people/sq km

(countries %>% arrange((population_density)))$location[1:50] # 50 most population-sparse countries
mean((countries %>% arrange((population_density)))$population_density[1:50]) # 17.71 people/sq km

# T-test: Comparing overall case density of lowest and highest population density countries...

t.test((countries%>%arrange(population_density))$total_cases_per_million[1:50],
       (countries%>%arrange(desc(population_density)))$total_cases_per_million[1:50])
#t = -1.6751, df = 83.521, p-value = 0.09765
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -25753.001   2204.774
#sample estimates:
#  mean of x mean of y 
#23656.80  35430.91 

# T-test: Comparing fatalities of lowest and highest population density countries...

t.test((countries%>%arrange(population_density))$total_deaths_per_million[1:50],
       (countries%>%arrange(desc(population_density)))$total_deaths_per_million[1:50])
#t = -0.28893, df = 89.082, p-value = 0.7733
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -284.1111  211.9740
#sample estimates:
#  mean of x mean of y 
#444.5189  480.5875 


ggplot(subset(data, location == 'United States'), aes(x=date)) +
  geom_line(aes(y=new_cases_smoothed_per_million), color = "darkred", na.rm=T) + 
  geom_line(aes(y=total_vaccinations_per_hundred), color="steelblue", na.rm=T) +          
  ylab('New Deaths per Million') + ggtitle('United States: New Cases per Million vs. Total Vaccinations per Hundred') + 
  theme(axis.title.x=element_blank(), legend.position = "bottom") + 
  scale_x_date(date_breaks = '1 month',date_labels = "%b%y",limits = as.Date(c('2020-10-01',(Sys.Date()-1))))




colnames(countries)


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
