
nba16 <- read.csv("17jan4nba.csv")
#nba 16-17 data for per game stats, CSV copied into a text file from basketball-reference.com 

#due to backslashes being the escape character in R, we must split the players' names from their
#basketball-reference IDs at the \ and then select only their name 

median(nba16$MP)

#Median minutes played PER GAME is 20.3 

players <- nba16$Player   #take the players names
players <- as.character(players) #turn them into characters for splitting 
players <- strsplit(players, "\\\\") #doing as.character makes \ -> \\ which requires "\\\\" to remove 
players <- lapply(players, function(elem) elem[1]) #the column is c(name, basketball-reference ID) 
players <- sapply(players, as.factor) #change it back into a factor 
nba16$Player <- players #replace the original 

# nba16 <- subset(nba16, select = -c(X,X.1))
#gap columns from Basketball-reference CSV are removed - GAP COLUMNS NOT IN PER GAME STATS  
 
minminutes <- function(mins) {                            #for per game, select Player:PS.G
  subset(nba16, subset = MP > mins, select = Player:PS.G)  #for advanced stats use Player:VORP 
}

nba16min20 <- minminutes(20) #20.3 is median minutes played per game 
library(dplyr)  #I like tibbles

nba <- tbl_df(nba16min20)
nbateams <- group_by(nba,Tm)

#plots for players <= 35  [vast majority of current players, as seen through with(nba,boxplot(Age)]
nba35 <- filter(nbateams, Age <=35)
with(nba35, plot(Age, FGA)) #everyone under 35, FGA vs Age 
title(main = "Field Goal Attempts through the Ages")
legend("topright", legend = c("<3 AST","3+"), col = c("black","blue"), pch = c(1,19))

nba35AST3 <- filter(nba35, AST >= 3) #players under 3 that average 3+ assists/game 
with(nba35AST3, points(Age, FGA, col = "blue", pch = 19))#color assisters blue, note: 1 player falls in the legend 



