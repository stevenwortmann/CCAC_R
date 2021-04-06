triag <- function(n){
  Tri <- NULL
  Ev <- NULL
  Odd <- NULL
  for (i in (1:n)){
    Tr <- ((i^2)+i)/2
    Tri <- c(Tri, Tr) #Adds triangular formula result for each iteration
  }
  for (j in (1:n)){ #Adds 'Tri' of odd iterable to 'Odd' list
      if (j %% 2 != 0) Odd <- c(Odd, Tri[j]) 
  }
  Ev <- Tri[!(Tri %in% Odd)] #'Ev' list is 'Tri' of not-odd iterables (evens) 
  print(Tri)
  print(Ev)
}

triag(20)
# 1   3   6  10  15  21  28  36  45  55  66  78  91 105 120 136 153 171 190 210
# 3  10  21  36  55  78 105 136 171 210

### It was at this point I realized I didn't need to make
### an all-encompassing function...

Tr <- NULL
Tri <- NULL

for (i in (1:20)){
  Tr <- ((i^2)+i)/2
  Tri <- c(Tri, Tr) #Adds triangular formula result for each iteration
}

Odd = NULL
Ev = NULL

for (i in (1:20)){
  if (i %% 2 != 0) Odd <- c(Odd, Tri[i]) 
}

Ev <- Tri[!(Tri %in% Odd)]

Tri
# 1   3   6  10  15  21  28  36  45  55  66  78  91 105 120 136 153 171 190 210
Ev
# 3  10  21  36  55  78 105 136 171 210

new <- c(1:20)
new <- ((new^2)+new)/2

new
# 1   3   6  10  15  21  28  36  45  55  66  78  91 105 120 136 153 171 190 210

new[seq(2, length(new), by = 2)]
# 3  10  21  36  55  78 105 136 171 210



repeater <- function(){
  vec <- NULL
  x <- 1
  repeat{
    vec <- c(vec, x)
    x <- (3*x) - 1
    if (length(vec)>19) break
  }
  vec
}

vec1 <- repeater()
vec1
# 1         2         5        14        41       122       365      1094      3281
# 9842     29525     88574    265721    797162   2391485   7174454  21523361  64570082
# 193710245 581130734

vec2 <- NULL
index <- 1
repeat{
  i <-  vec1[index]
  if (i %% 2 != 0) (i <- i+1)
  vec2 <- c(vec2, i)
  index <- index + 1
  if (index > length(vec1)) break
}
vec1 <- vec2

vec1
# 2         2         6        14        42       122       366      1094      3282
# 9842     29526     88574    265722    797162   2391486   7174454  21523362  64570082
# 193710246 581130734

vec1 <- repeater()
vec1

vec1[vec1 %% 2 != 0] <- vec1[vec1 %% 2 != 0] + 1
vec1

# This non-loop method is much more preferable.
# This method is much less verbose and is easier to understand.
# New placeholder vectors aren't necessary, nor are index variables

globVar <- 1

for (i in 1:20){
  globVar <- globVar + 2
}

globVar # 41 yes, it updated

globVar <- 1

glob_func <- function(){
  globVar <- 1
  for (i in 1:20){
    globVar <- globVar + 2
  }
  globVar
}

glob_func() # 41

globVar # 1 no, it did not update
# variables within functions are limited to within their function
# these variables are not made into global variables outside the function

super <- read.csv('/users/steve_wortmann/desktop/Dat-204 R/superbowl.csv')

team_wins <- NULL # Create vector for number of wins per team
unique_winners <- unique(super$Winner)

for(i in 1:length(unique_winners)){
  winner_count <- length(which(super$Winner == unique_winners[i]))
  team_wins <- c(team_wins, winner_count)
} # Wins per team created by counting length of unique winners matching the overall column

names(team_wins) <- unique_winners
# Winning teams are added as names to their corresponding number of wins

sort(team_wins, decreasing=T)

winCount <- function(team_name){ #if input returns NA, then return 'Sorry'
  if(is.na(team_wins[team_name])) return('Sorry, that team has not won yet.')
  else unname(team_wins[team_name]) # Returns number assoc. with team name input
}

winCount('San Diego Chargers') # "Sorry, that team has not won yet."
winCount('Denver Broncos') # 3
winCount('Cleveland Browns') # "Sorry, that team has not won yet."
winCount('Pittsburgh Steelers') # 6
