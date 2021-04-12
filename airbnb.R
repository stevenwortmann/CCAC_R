airbnb <- read.csv('/users/steve_wortmann/desktop/Dat-204 R/abbNYCdata.csv')

qplot(airbnb$price, main='Daily Airbnb Rates, NYC', xlab='Price', ylab= 'No. Listings')
expRoom <- which(airbnb$price>=500) # Seemed like a fair cutoff
airbnb <- airbnb[-expRoom,] # All rows under $500
qplot(airbnb$price, main='Daily Airbnb Rates, NYC', xlab='Price', ylab= 'No. Listings')

airbnb_mod1 <- subset(airbnb, room_type %in% c("Entire home/apt", "Private room"))
# Create modified dataframe without 'Shared room' rows
qplot(x=room_type, y= price, geom='boxplot', data= airbnb_mod1, xlab= "Airbnb Type", ylab= "Daily Rate", 
      fill= I('pink'), main ="Airbnb Price Comparison, NYC")

entire_home <- airbnb$price[airbnb$room_type == "Entire home/apt"]
private_room <- airbnb$price[airbnb$room_type =="Private room"]
t.test(entire_home, private_room)
# t = 151.82, df = 39471, p-value < 2.2e-16
# Room prices for groups are reliably different
# Results are comfortably statistically significant
# This makes sense. I wouldn't expect/want to pay the same for these two groups

airbnb_mod2 <- subset(airbnb, neighbourhood_group %in% c("Manhattan", "Brooklyn"))
# Create modified dataframe with Manhattan, Brooklyn rows only
qplot(x=neighbourhood_group, y= price, geom='boxplot', data= airbnb_mod2, xlab= "Neighborhood Groups", ylab= "Daily Rate", 
      fill= I('lightyellow'), main ="Airbnb Price Comparison, Manhattan vs. Brooklyn")

manhattan <- airbnb$price[airbnb$neighbourhood_group == "Manhattan"]
brooklyn <- airbnb$price[airbnb$neighbourhood_group =="Brooklyn"]
t.test(manhattan, brooklyn)
# t = 60.246, df = 39369, p-value < 2.2e-16
# Room prices between Manhattan/Brooklyn are reliably different
# These results are again statistically significant
# This also makes sense. Manhattan is one of the most expensive places on Earth

hoods <- c(unique(airbnb$neighbourhood)) # Individual neighborhoods
hood_costs <- NULL # Null vector to populate...
for (i in hoods){
  this_hood_cost <- mean(airbnb$price[airbnb$neighbourhood == i])
  hood_costs <- c(hood_costs, this_hood_cost)}
names(hood_costs) <- hoods # Name/label neighborhoods
hood_costs # Names of neighborhoods with average price

exp_hoods <- c(sort(hood_costs, decreasing = T)[1:2])
# Neponsit 274.6667, Tribeca 255.7754

airbnb_mod3 <- subset(airbnb, neighbourhood %in% c("Neponsit", "Tribeca"))
# Create modified dataframe with Neponsit, Tribeca rows only
qplot(x=neighbourhood, y= price, geom='boxplot', data= airbnb_mod3, xlab= "Neighborhoods", ylab= "Daily Rate", 
      fill= I('lightgray'), main ="Airbnb Price Comparison, Neponsit vs. Tribeca")


# Assuming you meant "neighborhoods found in e" and not "neighborhoods found in c"
neponsit <- airbnb$price[airbnb$neighbourhood=='Neponsit']
tribeca <- airbnb$price[airbnb$neighbourhood=='Tribeca']
t.test(neponsit, tribeca)
# t = 0.42657, df = 2.188, p-value = 0.708
# There is no reliable difference between Neponsit and Tribeca prices
# 70% p-value suggests there's no effect in neighborhood price difference
# Average price difference is $18.89 in these neighborhoods
# There are 138 listings in Tribeca... only 3 listings in Neponsit!