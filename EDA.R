rm(list=ls())

## 연도별 데이터 추출 함수
yearly <- function( x, year ) { 
  ret <- list()
  for ( i in 1:length(x) ) {
    ret[[i]] <- subset(x[[i]], YEAR == year)
  }
  return (ret )
}

