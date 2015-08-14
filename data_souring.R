# 함수 파일 불러오기
source("crawl_player.R")
## csv파일 읽기, 데이터 정리 
filelist <- list.files(path=getwd(), pattern=".csv")
filename <- substr(filelist, 1, nchar(filelist)-4)
DB <- lapply(filelist, read.csv, header=T)  # 통합된 데이터, list
for ( i in 1:length(filename)) {
  assign(filename[i], DB[[i]])  # 각 파일명 matrix로 할당
}
## crawling 루프
crawl.loop ( file = player_id, write.as.csv=F)

## 2014년도 개인 데이터
pitcher_2014 <- subset(pitcher_kor_modify, YEAR == 2014)
batter_2014 <- subset(batter_kor_modify, YEAR == 2014)

crawl.loop(player_id[22,]) # 투수예제
crawl.loop(player_id[53,]) # 타자 예제






