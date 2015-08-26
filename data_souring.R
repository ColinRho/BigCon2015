library(XML) 
library(rvest)
library(plyr)
library(data.table)

## 함수 실행에 필요한 데이터 파일들: gamelist, lineup, samename, player_id

## 함수 파일 불러오기
source("crawl_part.R",encoding="UTF-8")
source("match_playerdata.R",encoding="UTF-8")

## 1. csv로 있는 파일 읽기
filelist <- list.files(path=getwd(), pattern=".csv")
filename <- substr(filelist, 1, nchar(filelist)-4)
DB <- lapply(filelist, read.csv, header=T)  # 통합된 데이터, list
for ( i in 1:length(filename)) {
  assign(filename[i], DB[[i]])  # 각 파일명 matrix로 할당
}

## 2. 선수목록에서 동명이인 이름 바꾸기, 리스트 출력
samename <- homonym(player_id)$samename # 동명이인 리스트
player_id <- homonym(player_id)$dat # 수정된 선수목록

## 3. 2014년도 개인 데이터
a <- subset( pitcher_select, select=c(name, team) )
pitcher_select$name <- apply( a, 1, function(x) change.homonym(x[1], x[2] ) )
rm(a)
a <- subset( hitter_select, select=c(name, team) )
hitter_select$name <- apply( a, 1, function(x) change.homonym(x[1], x[2] ) )
rm(a)

  ## 투수는 각 지표들을 이닝수로 나누었고, 타자들은 타수로 나누었다. 
  pitcher_2014 <- subset(pitcher_select, year == 2014)
  pitcher_2014[,6:12] <- round( pitcher_2014[,6:12]/pitcher_2014$IP, 3)
  pitcher_2014 <- pitcher_2014[ , ! colnames(pitcher_2014) %in% c("year") ]
  hitter_2014 <- subset(hitter_select, year == 2014)
  hitter_2014[,5:16] <- round( hitter_2014[,5:16]/hitter_2014$AB, 3 )
  hitter_2014 <- hitter_2014[ , ! colnames(hitter_2014) %in% c("year") ]
  
## 3. crawling 루프(선수 개인별 이번 시즌 데이터)
crawl.loop ( file = player_id, write.as.csv=F)
  
## 4. 2015년 모든 경기들 목록
month <- c("03","04","05","06","07","08") # 8월까지 입력함
gamelist <- gamelist.total ( month, year = "2015" ) # 경기목록

## 5. 경기목록의 모든 경기의 라인업
#### 주의: 매 경기마다 페이지를 크롤링해오기 때문에 시간이 꽤 소요됨(네트워크환경이 좋은상태를 권장)
#### 혹은 월별로 
# as csv 
lineupAug <- lineup.total(gamelist, by.month="08")
lineup0$date <- as.Date(lineup0$date)
lineup <- myrbind( list(lineup0, lineupAug) )

## 6. 각 경기별 라인업에 따라 선수들의 기록을 호출 
gameset <- subset(gamelist, !is.na(score) )
gameset <- gameset[-420,] # 올스타전 제외

w <- 0.3  # 임의로 지정한 weight
## gameset과 lineup의 자료가 같은 날까지 일치해야 한다.
dat1 <- aggr.stat(gameset, w)


