library(XML)
source("crawl.R")

## 루프
for( i in 1:nrow(player_id) ) {
  vec <- player_id[i,] 
  name <- as.character(vec$name)
  temp <- crawl(vec, F)
  if( is.na(temp) ) { 
    cat(name," 선수의 데이터가 존재하지 않습니다.","\n") 
  } else {
    assign(name, temp) ; cat("데이터가", name,"(으)로 저장되었습니다.","\n")
  }
  rm(vec,name,temp)
}

## 루프 (csv 저장용)
for( i in 1:nrow(player_id) ) {
  vec <- player_id[i,] 
  name <- as.character(vec$name)
  temp <- crawl(vec, T)
  if( is.na(temp) ) { 
    cat(name," 선수의 데이터가 존재하지 않습니다.","\n") 
  } else {
    assign(name, temp) ; cat("데이터가", name,"(으)로 저장되었습니다.","\n")
  }
  rm(vec,name,temp)
}
