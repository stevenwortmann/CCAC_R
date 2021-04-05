library(dplyr)
library(lubridate)
library(ggplot2)
library(measurements)

phil <- read.csv('/users/steve_wortmann/desktop/Dat-204 R/phil.csv')

# Filtering data to "No Shadow" and "Full Shadow" years
normal_year <- subset(phil, !(shadow %in% c("No Record", "Partial Shadow", "")))

length(phil$shadow) # 136 total records
length(normal_year$shadow) # 123 normal years
length(normal_year$shadow[normal_year$shadow=="Full Shadow"]) # 105 full shadow years
length(normal_year$shadow[normal_year$shadow=="No Shadow"]) # 18 no shadow years

# Percentage "No Shadow": 0.1463415
length(normal_year$shadow[normal_year$shadow=="No Shadow"]) / length(normal_year$shadow)

# Visually comparing years of shadow seen/unseen
qplot(normal_year$shadow, ylab='Observation Count (Years)', main ="Phil: Shadow vs. No Shadow") + theme(axis.title.x = element_blank())

# Boxplots for Feb-Mar temperatures differentiated by Shadow/No Shadow years
qplot(x=shadow, y= feb_avg, geom='boxplot', data= normal_year, ylab= "Average Temperature", 
      fill= I('pink'), main ="February Temperatures, Everywhere") + ylim(25,50) + theme(axis.title.x = element_blank())
qplot(x=shadow, y= mar_avg, geom='boxplot', data= normal_year, ylab= "Average March Temperature", 
      fill= I('lightgreen'), main ="March Temperatures, Everywhere") + ylim(25,50) + theme(axis.title.y = element_blank(), axis.title.x = element_blank())

# Creating full vectors of Shadow/No Shadow temperatures for our t-test
no_shadow_temps <- c((normal_year$feb_avg[normal_year$shadow == 'No Shadow']),
                     (normal_year$mar_avg[normal_year$shadow == 'No Shadow']))
full_shadow_temps <- c((normal_year$feb_avg[normal_year$shadow == 'Full Shadow']),
                       (normal_year$mar_avg[normal_year$shadow == 'Full Shadow']))

t.test(no_shadow_temps, full_shadow_temps, na.rm=T)
# t = 1.7543, df = 40.585, p-value = 0.08693
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.2395779  3.4012779
#sample estimates:
#  mean of x mean of y
#39.27200  37.69115 

weather <- read.csv('/users/steve_wortmann/desktop/Dat-204 R/weather_data.csv')

#Taking a look at which years in dataset are "No Shadow" years
unique(weather$Year[weather$Year %in% c(as.numeric(phil$year[phil$shadow == 'No Shadow']))])

# First we create a datetime object from the strings in 'DATE' column
# POSIXct class stores date/time values as the number of seconds since 1/1/70
# Day, Month, Year columns created from POSIXct second counts
weather$DATE <- as.POSIXct(strptime(weather$DATE, "%Y-%m-%d"))
weather <- mutate(weather, Day = day(DATE))
weather <- mutate(weather, Month = month(DATE))
weather <- mutate(weather, Year = year(DATE))

# 'Hourly temperature' and 'Year' columns populated as numeric values
weather$hourly_temp <- as.numeric(weather$hourly_temp)
weather$Year <- as.numeric(as.character(weather$Year))

# 'MonthYear' created holding year and two-digit month values
# 'weather_month' df populated with  average temperature aggregated to one point/month
weather <- mutate(weather, MonthYear = paste(year(DATE),formatC(month(DATE), width=2,flag="0")))
weather_month <- aggregate(weather, by=list(weather$MonthYear), FUN = function(x) mean(x, na.rm=T))

# 'Month' is stored in new df as numeric, 'Year' as categorical factor of year variables
weather_month$Month <- as.numeric(weather_month$Month)
weather_month$Year <- factor(weather_month$Year)

# Line plot for monthly year temperatures
ggplot() + geom_line(data=weather_month, aes(x=Month, y=hourly_temp, color=Year)) + 
  scale_x_continuous(breaks= pretty_breaks()) + ylab("Hourly Temperature") + xlab("Months") + 
  ggtitle("Yearly Comparison: Average Temperature")

# Plotting the subset of monthly readings created for spring months (Feb-Apr)
weather_month_spring <- subset(weather_month, (weather_month$Month >= 2 & weather_month$Month <= 5), na.rm=T)

# Spring temperature comparison, years 2010-19
ggplot() + geom_line(data=weather_month_spring, aes(x=Month, y=hourly_temp, color=Year)) + 
  ylab("Average Temperature") + xlab("Months") + ggtitle("Spring Temperature Comparison")

# Matching years in spring plot with years Phil saw no shadow
# 'No Shadow' boolean column populated with True for 'No Shadow,' False for 'Shadow'
weather_month_spring <- mutate(weather_month_spring, 
                               NoShadow = (weather_month_spring$Year %in%
                                             (as.numeric(phil$year[phil$shadow == 'No Shadow']))))

# 2010-19 spring temperature comparison, grouped by Shadow/No Shadow
ggplot() + geom_line(data=weather_month_spring, aes(x=Month, y=hourly_temp, color=NoShadow, group = Year)) + 
  ggtitle("Spring Month Comparison: Shadow vs. No Shadow") +
  xlab("Months") + ylab("Average Temperature") + 
  theme(legend.justification = c(1,0), legend.position = c(1,0))

# Creating full numeric-populated vectors of Shadow/No Shadow hourly temperatures for our t-test
no_shadow_temps_hourly <- c(as.numeric(weather_spring$hourly_temp[normal_year$shadow == 'No Shadow']))
full_shadow_temps_hourly <- c(as.numeric(weather_spring$hourly_temp[normal_year$shadow == 'Full Shadow']))

t.test(no_shadow_temps_hourly, full_shadow_temps_hourly, na.rm=T)
#t = 0.28935, df = 4270.2, p-value = 0.7723
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.4265550  0.5742665
#sample estimates:
#  mean of x mean of y 
#35.34190  35.26805 

cities <- read.csv('/users/steve_wortmann/desktop/Dat-204 R/cities.csv')

# All temperatures are in Kelvin, convert to Fahrenheit via 'conv_unit()'
convert <- function(city_column_k){
  city_column_f = NULL
  for (k in city_column_k){
    city_column_f = c(city_column_f, conv_unit(k, "K", "F"))
  }
  city_column_f
}

# Adding Pittsburgh, Philadelphia columns for converted temperatures
cities <- mutate(cities, Pitt_f = convert(cities$Pittsburgh))
cities <- mutate(cities, Philly_f = convert(cities$Philadelphia))

# Month, Year columns created from POSIXct second counts for graphing
cities$datetime <- as.POSIXct(strptime(cities$datetime, "%Y-%m-%d"))
cities <- mutate(cities, Month = month(datetime))
cities <- mutate(cities, Year = year(datetime))

# Isolating Feb-Apr months and creating boolean column separating Shadow/No Shadow
cities_spring <- subset(cities, (cities$Month >= 2 & cities$Month < 5), na.rm=T)
cities_spring <- mutate(cities_spring, NoShadow = (cities_spring$Year %in%
                                                     (as.numeric(phil$year[phil$shadow == 'No Shadow']))))

# Boxplots for Pittsburgh, Philadelphia Feb-Apr temperatures differentiated by Shadow/No Shadow years
qplot(x=NoShadow, y= Pitt_f, geom='boxplot', data= cities_spring, xlab= "Phil: No Shadow", na.rm=T, 
      fill= I('yellow'), main ="Pittsburgh (Feb-Apr): Avg. Hourly Temperatures") + ylim(25,50) + theme(axis.title.y = element_blank())
qplot(x=NoShadow, y= Philly_f, geom='boxplot', data= cities_spring, xlab= "Phil: No Shadow", na.rm=T, 
      fill= I('orange'), main ="Philadelphia (Feb-Apr): Avg. Hourly Temperatures") + ylim(25,50) + theme(axis.title.y = element_blank())

# Creating full vectors of Pitt, Philly Shadow/No Shadow hourly temperatures for our t-test
Philly_no_shadow_temps <- c(as.numeric(cities$Philly_f[phil$shadow == 'No Shadow']))
Philly_full_shadow_temps <- c(as.numeric(cities$Philly_f[phil$shadow == 'Full Shadow']))

# Testing Pittsburgh...
t.test(Pitt_no_shadow_temps, Pitt_full_shadow_temps, na.rm=T)
#t = 0.18225, df = 8184.6, p-value = 0.8554
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.4667709  0.5624626
#sample estimates:
#  mean of x mean of y 
#51.68731  51.63946

#Testing Philadelphia...
t.test(Philly_no_shadow_temps, Philly_full_shadow_temps, na.rm=T)
#t = 0.4146, df = 8145.2, p-value = 0.6784
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.3999193  0.6144639
#sample estimates:
#  mean of x mean of y 
#54.11208  54.00481
