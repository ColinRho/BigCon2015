library(XML)
source("crawl_functions.R")
## csv파일 읽기, 데이터 정리 
filelist <- list.files(path=getwd(), pattern=".csv")
filename <- substr(filelist, 1, nchar(filelist)-4)
DB <- lapply(filelist, read.csv, header=T)  # 통합된 데이터, list
for ( i in 1:length(filename)) {
  assign(filename[i], DB[[i]])  # 각 파일명 matrix로 할당
}

## crawling 루프
for( i in 1:nrow(player_id) ) {
  vec <- player_id[i,] 
  name <- as.character(vec$name)
  temp <- crawl.kbo(vec, F)
  if( is.na(temp) ) { 
    cat(name," has no data.","\n") 
  } else {
    assign(name, temp) ; cat("Data set is s aved as", name,"\n")
  }
  rm(vec,name,temp)
}

## 루프 (csv 저장용)
for( i in 1:nrow(player_id) ) {
  vec <- player_id[i,] 
  name <- as.character(vec$name)
  temp <- crawl.kbo(vec, T)
  if( is.na(temp) ) { 
    cat(name," has no data.","\n") 
  } else {
    assign(name, temp) ; cat("Data set is saved as", name,"\n")
  }
  rm(vec,name,temp)
}


