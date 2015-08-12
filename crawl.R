## package "XML" required  
## 선수 고유번호 데이터
player_id <- read.csv("player_id.csv", header=T) 

## 최초에 데이터를 읽어오는 함수
crawl.read <- function (row.player) {  
  id <- row.player$id ; pos <- row.player$pos
  if (pos == "p") { 
    u <- "http://www.koreabaseball.com/Record/Player/PitcherDetail/Daily.aspx?playerId="
  } else {
    u <- "http://www.koreabaseball.com/Record/Player/HitterDetail/Daily.aspx?playerId="
  }
  url <- paste(u, id, "", sep="")
  a <- readHTMLTable(url) # Table로 된 자료
  # 선수 고유번호가 잘못 입력될 경우의 디버깅 용도 
  b <- readHTMLList(url)
  player <- b[[17]][1]
  cat(player,"의 데이터를 읽어옵니다","\n") 
  return(a)
}
## 크롤링된 데이터를 정리
crawl.mod <- function(a) {
  for (i in 1:length(a)) {
    a[[i]] <- as.matrix(a[[i]])
  }
  a <- do.call(rbind, a)
  a <- as.data.frame(a)
  return(a)
}
## 최종 output
crawl <- function(row.player, write.as.csv=F) {
  a <- crawl.read(row.player)
  dat <- crawl.mod(a) # dat는 data.frame 형태로 변환된 자료
    # 열의 개수를 이용하여 데이터가 없는 선수를 걸러냄(열의 개수가 1개면 데이터가 없는 것)
  if (ncol(dat) != 1) {
    var.name <- colnames(dat)
      # 투수/타자에 따라 변수명 설정
    if (row.player$pos == "f") { 
      var.name[1:2] <- c("date", "vs")
    }  else  { 
      var.name[1:4] <- c("date", "vs", "type" ,"result")
    }
    colnames(dat) <- var.name
    ret <- dat
      # csv로 쓰는 것을 설정할 경우
    if (write.as.csv) { write.csv( dat, file=paste(row.player$name,".csv"), row.names=F)  }
  } else {
    ret <- NA  # 1군 데이터가 없는 선수
  }
  return( ret )
}