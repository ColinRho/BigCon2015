library(XML)
source("crawl.R")

## 루프
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


