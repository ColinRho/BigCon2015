rm(list=ls())

## 파일 읽기, 데이터 정리 
filelist <- list.files(path=getwd(), pattern=".csv")
filename <- substr(filelist, 1, nchar(filelist)-4)
DB <- lapply(filelist, read.csv, header=T)  # 통합된 데이터, list
for ( i in 1:length(filename)) {
  assign(filename[i], DB[[i]])  # 각 파일명 matrix로 할당
}

yearly <- function( x, year ) {
  ret <- list()
  for ( i in 1:length(x) ) {
    ret[[i]] <- subset(x[[i]], YEAR == year)
  }
  return (ret)
}

yearly(DB, 2014)  

