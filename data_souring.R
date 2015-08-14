## 함수 파일 불러오기
source("crawl_functions.R")
## csv파일 읽기, 데이터 정리 
filelist <- list.files(path=getwd(), pattern=".csv")
filename <- substr(filelist, 1, nchar(filelist)-4)
DB <- lapply(filelist, read.csv, header=T)  # 통합된 데이터, list
for ( i in 1:length(filename)) {
  assign(filename[i], DB[[i]])  # 각 파일명 matrix로 할당
}
## crawling 루프
crawl.loop ( file = player_id, write.as.csv=F)

x <- crawl.kbo(player_id[2,]) # 투수예제
y <- crawl.kbo(player_id[53,]) # 타자 예제






