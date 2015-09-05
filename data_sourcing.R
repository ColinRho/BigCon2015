########## 0. preliminaries ######################################################################

library(XML) 
library(rvest)
library(plyr)
library(data.table)
library(ggplot2)
library(reshape2)
library(randomForest)
library(cvTools)

## objects that must be included in Global Environment to run every function without error 
## :gamelist, lineup, samename, player_id, trade_2015

## source function scripts 
source("crawl_part.R")
source("match_playerdata.R",encoding="UTF-8")
source("modeling.R")

##################################################################################################
########## 1. reading csv files in the working directory #########################################

filelist <- list.files(path=getwd(), pattern=".csv")
filename <- substr(filelist, 1, nchar(filelist)-4)
## aggregated database, list
DB <- lapply(filelist, read.csv, header=T)  
## assign data matrix as the name of file
for ( i in 1:length(filename)) {
  assign(filename[i], DB[[i]])  
}

##################################################################################################
########## 2. some modifications in player list and traded player list ###########################

## players' name who have same name with another player
samename <- homonym(player_id)$samename
## modified player list
player_id <- homonym(player_id)$dat 
## trade list modifying
trade_2015$date <- as.Date(trade_2015$date)
trade_2015$name <- apply( trade_2015[,c(2,5)], 1, function(x) change.homonym( x[1], x[2]) )

##################################################################################################
########## 3. personal stats in 2014  ############################################################

a <- subset( pitcher_select, select=c(name, team) )
pitcher_select$name <- apply( a, 1, function(x) change.homonym(x[1], x[2] ) )
rm(a)
a <- subset( hitter_select, select=c(name, team) )
hitter_select$name <- apply( a, 1, function(x) change.homonym(x[1], x[2] ) )
rm(a)

pitcher_2014 <- subset(pitcher_select, year == 2014)
pitcher_2014 <- pitcher_2014[ , ! colnames(pitcher_2014) %in% c("year") ]
hitter_2014 <- subset(hitter_select, year == 2014)
hitter_2014 <- hitter_2014[ , ! colnames(hitter_2014) %in% c("year") ]
  
########## 3. crawling personal daily data of all players in the list ############################
### CAUTION: 

crawl.loop ( file = player_id, write.as.csv=F)

##################################################################################################
########## 4. list of all games in 2015 season ###################################################

month <- c("03","04","05","06","07","08","09")
gamelist <- gamelist.total ( month, year = "2015" )
games2015 <- subset ( gamelist, !is.na(score) ) # without cancelled games

##################################################################################################

