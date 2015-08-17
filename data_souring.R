# 함수 파일 불러오기
source("crawl_player.R",encoding="UTF-8")
source("crawl_boxscore.R",encoding="UTF-8")
## csv파일 읽기, 데이터 정리 
filelist <- list.files(path=getwd(), pattern=".csv")
filename <- substr(filelist, 1, nchar(filelist)-4)
DB <- lapply(filelist, read.csv, header=T)  # 통합된 데이터, list
for ( i in 1:length(filename)) {
  assign(filename[i], DB[[i]])  # 각 파일명 matrix로 할당
}

## crawling 루프(선수 개인별 데이터)
crawl.loop ( file = player_id, write.as.csv=F)

## 2015년 모든 경기들 목록
month <- c("03","04","05","06","07","08")
gamelist <- gamelist.total ( month, year = "2015" )


## 경기목록의 모든 경기의 라인업
#### 주의: 매 경기마다 페이지를 크롤링해오기 때문에 시간이 꽤 소요됨(네트워크환경이 좋은상태를 권장)
#### 혹은 월별로 
lineup <- lineup.total(gamelist)  
# as csv 
write.csv(lineup, "lineup.csv")

## 2014년도 개인 데이터
pitcher_2014 <- subset(pitcher_kor_modify, YEAR == 2014)
batter_2014 <- subset(batter_kor_modify, YEAR == 2014)







