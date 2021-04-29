library(tidyverse)

wine <- as.tibble(read_csv('/users/steve_wortmann/desktop/dat204/wk11/wineRatings.csv'))
wine

# Correlation between price and rating of all wines:
cor.test(wine$price, wine$points) # 0.4598634

# Correlation between price and rating of American wines: 
usa <- filter(wine, grepl('US', country))
cor.test(usa$points,usa$price) # 0.4612999

# Correlation between price and rating of Italian wines: 
italy <- filter(wine, grepl('Italy', country))
cor.test(italy$points,italy$price) # 0.5969127

# Correlation between price and rating of French wines:
france <- filter(wine, grepl('France', country))
cor.test(france$points,france$price) # 0.5079387

# Plot price vs. rating of all wines
ggplot(wine, aes(x=points, y=price, color="#990239")) + geom_point() +
  xlab('Rating (points)') + ylab('Price') + theme(legend.position = "none") +
  ggtitle('Wines Worldwide: Price vs. Rating')

wineModel <- lm(price ~ points, data = wine) # Linear model of price/points data

wineModel
# (Intercept)       points  
#   -422.021         5.185

# Linear regression formula line added to plotted points graph
ggplot(wine, aes(x=points, y=price, color="#990239")) + geom_point() +
  xlab('Rating (points)') + ylab('Price') + ylim(0,1000) + theme(legend.position = "none") +
  ggtitle('Wines Worldwide: Price vs. Rating') + geom_abline(intercept=-422.021, slope=5.185, color='purple')

linReg <- function(pts){
  price <- 5.185*pts - 422.021
  print(round(price,2))
} # Linear regression formula as R function for quick use

linReg(83) # $8.33
linReg(87) # $29.07
linReg(89) # $39.44

linReg(81) # -$2.04 <- get paid to drink decent(?) wine

# This linear regression model certainly has its limitations.
# Any ranking under 81.39 under this model results in negative price.
# Last time I checked, I can't pick up wine at the store for a living.
# This model is idyllic, however it is simply not the world we live in unfortunately.
# We can recognize that this is a rudimentary form of predictive modeling.