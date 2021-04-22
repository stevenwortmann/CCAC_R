library(dplyr)
library(tibble)
library(hflights)
library(tidyverse)

imdb_d <- read.csv('/users/steve_wortmann/desktop/dat204/wk9/imdb_top_1000.csv')
imdb <- as_tibble(imdb_d) # tibble of csv file
imdb

#1 columns selected
imdb_1 <- select(imdb,Series_Title,Released_Year,Runtime,IMDB_Rating,Genre,Meta_score)
imdb_1

#2 Action genre filtered
imdb_2 <- filter(imdb_1, grepl('Action', Genre))
imdb_2

#3 Runtime arranged shortest to longest
imdb_3 <- imdb_2 %>% arrange(as.numeric(Runtime)) %>% separate(col=Runtime, into=c("Numeric","Char"))
imdb_3 <- imdb_3 %>% separate(col=Numeric, into=c("Runtime",NA), convert=T)
imdb_3 <- imdb_3 %>% arrange(Runtime)
imdb_3

#4 Create yes/no column for IMDB score 8+
imdb_4 <- imdb_3 %>% mutate(imdb8 = if_else(IMDB_Rating >= 8, "Yes", "No"))
imdb_4

#5 Count of number of action movies released per year
imdb_5 <- imdb_4 %>%
  group_by(Released_Year) %>% 
  summarize(Num_Films = n()) %>% # Size/count of grouping (years)
  print(n = Inf) # Display all rows
imdb_5

#6 Top two action movie meta scores for each year, oldest to newest
imdb_6 <- imdb_4 %>% 
  group_by(Released_Year) %>%
  select(Series_Title, Runtime, Genre, Meta_score) %>% # Most fields included
  filter(min_rank(desc(Meta_score)) <= 2) %>% # Top 2 non-null scores
  arrange(Released_Year, desc(Meta_score)) %>% # Arrange by year, score descending
  print(n = Inf) # Display all rows
imdb_6
