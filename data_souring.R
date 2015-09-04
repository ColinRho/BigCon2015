library(XML) 
library(rvest)
library(plyr)
library(data.table)
library(ggplot2)
library(reshape2)
library(randomForest)
library(cvTools)

## 함수 실행에 필요한 데이터 파일들: gamelist, lineup, samename, player_id

## 함수 파일 불러오기
source("crawl_part.R",encoding="UTF-8")
source("match_playerdata.R",encoding="UTF-8")
source("modeling.R")

########## 1. reading csv files in the working directory ###################################

filelist <- list.files(path=getwd(), pattern=".csv")
filename <- substr(filelist, 1, nchar(filelist)-4)
DB <- lapply(filelist, read.csv, header=T)  # 통합된 데이터, list
for ( i in 1:length(filename)) {
  assign(filename[i], DB[[i]])  # 각 파일명 matrix로 할당
}

############################################################################################
########## 2. some modifications in player list and traded player list #####################

samename <- homonym(player_id)$samename # 동명이인 리스트
player_id <- homonym(player_id)$dat # 수정된 선수목록

## trade list modifying
trade_2015$date <- as.Date(trade_2015$date)
trade_2015$name <- apply( trade_2015[,c(2,5)], 1, function(x) change.homonym( x[1], x[2]) )

############################################################################################
########## 3. personal stats in 2014  ######################################################

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
  
########## 3. crawling personal daily data of all players in the list ######################
### CAUTION: 

crawl.loop ( file = player_id, write.as.csv=F)

############################################################################################
########## 4. list of all games in 2015 season #############################################

month <- c("03","04","05","06","07","08","09")
gamelist <- gamelist.total ( month, year = "2015" )
games2015 <- subset ( gamelist2015, !is.na(score) ) # without cancelled games

############################################################################################





## 5. Regression Approach

##############################################################################



## real win rates
real2015 <- rate_func( games = games2015, rate = "real" )

## pythagorean win rates
pyth2015 <- rate_func( games = games2015, rate = "pyth", power = 2) # with typical power index

## plot examples
rate_plot(real2015, since = "2015-07-30") + stat_smooth(method = 'lm')
comp_plot( "LG", rate2015, pyth2015, since="2015-05-01"  )


## power index variation
pyth0.9 <- rate_func( games2015, rate = "pyth", power = 0.9)
pyth1 <- rate_func( games2015, rate = "pyth", power = 1)
pyth3 <- rate_func( games2015, rate = "pyth", power = 3)
pyth2.3 <- rate_func( games2015, rate = "pyth", power = 2.3)
comp_plot("넥센", rate2015, pyth1, since = "2015-08-01")

### https://en.wikipedia.org/wiki/Pythagorean_expectation

## 5. 경기목록의 모든 경기의 라인업
#### 주의: 매 경기마다 페이지를 크롤링해오기 때문에 시간이 꽤 소요됨(네트워크환경이 좋은상태를 권장)
#### 혹은 월별로 저장하는 것을 권장

lineupSep <- lineup.total(gamelist, by.month="09")
lineup0$date <- as.Date(lineup0$date)
lineup <- myrbind( list(lineup0, lineupSep) )

## 6. 각 경기별 라인업에 따라 선수들의 기록을 호출 
gameset <- subset(gamelist, !is.na(score) )
gameset <- gameset[-420,] # 올스타전 제외

w <- 0.1  # 임의로 지정한 weight
## gameset과 lineup의 자료가 같은 날까지 일치해야 한다.
# 원시 데이터
dat1 <- aggr.stat(gameset, w)
# 나눔 데이터
dat2 <- aggr.stat(gameset, w, T)
# 상대승률이 없는경우( 시즌 첫게임 ) 50%로 변경
dat1$vs_rate[which( is.nan(dat1$vs_rate) | is.na(dat1$vs_rate) )] <- 0.5
dat2$vs_rate[which( is.nan(dat2$vs_rate) )] <- 0.5
# 이분 데이터
dat3 <- dat2
# 각 열에 대하여 1보다 크면 1, 1보다 작으면 0 
temp <- function ( column ) {
  column[column < 1] <- 0
  column[column >= 1] <- 1
  return(column)
}
for ( i in 3:35 ) {
  dat3[,i] <- temp(dat3[,i])
}
# 연승과 상대승률 변수 
dat3$streak[dat3$streak >= 0 ] <- 1 ; dat3$streak[dat3$streak < 0 ] <- 0
dat3$vs_rate[dat3$vs_rate >= 0.5 ] <- 1 ; dat3$vs_rate[dat3$vs_rate < 0.5 ] <- 0

