## package "XML" required  
## source() 로 이 페이지의 코드를 읽어올 때 한글이 출력될 때 Warning message
library(XML)
## numeric 변수로 변환하는 함수(input: 벡터)
convert.numeric <- function ( x ) {
  return(as.numeric( as.character (x) ))
}
## 투수의 이닝(IP) 데이터를 numeric으로 변환하는 함수
convert.IP <- function ( IP ) {
  IP <- as.character(IP)
  ## 정수부분과 분수부분을 분리
  spl <- strsplit(IP, split=" ")
  for ( i in 1:length(spl) ) { 
    ## 이닝수가 대분수인 경우 1과 1/3 등
    if ( length(spl[[i]]) == 2) {
      IP[i] <- as.numeric(spl[[i]][1]) + eval( parse( text=spl[[i]][2] ))
    } 
    ## 이닝수가 분수인 경우 2/3 이닝 등
    else if ( nchar(spl[[i]]) != 1)
    {
      IP[i] <- eval( parse( text=spl[[i]] ))
    }
    ## 이닝수가 정수인 경우
    else {
      IP[i] <- as.numeric(spl[[i]])
    }
  }
  return(round(as.numeric(IP),3))
}
## 최초에 웹페이지를 읽어오는 함수
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
  cat("Data set of",player,"is being read","\n") 
  return(a)
}
## 크롤링 데이터를 data.frame으로 정리하는 함수
crawl.mod <- function(row.player) {
  a <- crawl.read(row.player)
  for (i in 1:length(a)) {
    a[[i]] <- as.matrix(a[[i]])
  }
  a <- do.call(rbind, a)
  a <- as.data.frame(a)
  return(a)
}
## numeric 변환 및 누적 데이터 계산 함수(투수용)
cal.pitcher <- function ( dat ) { # dat crawl.mod 결과로 출력된 행렬이어야 한다.
  p <- ncol(dat)
  # numeric으로 변환
  dat$IP <- round(convert.IP(dat$IP), 3)
  dat[,5:p] <- apply(dat[,5:p], 2, convert.numeric)
  # 변수명 설정
  colnames(dat)[1:4] <- c("date","vs","type","result") ; colnames(dat)[p] <- "ERA"
  # 누적데이터 계산
  for ( row.num in 2:nrow(dat) ) {
    dat[row.num,5:(p-1)] <- dat[row.num,5:(p-1)] + dat[(row.num-1),5:(p-1)]
  }
  # 추가 지표 계산
  WHIP <- round( (dat$H + dat$BB)/dat$IP, 3) # WHIP 이닝당 출루 허용
  dat <- data.frame( dat, WHIP )
  # 당일 ERA 제거
  return(dat[,-5])
}
## numeric 변환 및 누적 데이터 계산 함수(타자용)
cal.hitter <- function ( dat ) { # dat crawl.mod 결과로 출력된 행렬이어야 한다.
  p <- ncol(dat)
  # numeric으로 변환
  dat[,3:p] <- apply(dat[,3:p], 2, convert.numeric)
  # 변수명 설정
  colnames(dat)[1:2] <- c("date","vs") ; colnames(dat)[p] <- "AVG"
  # 누적 데이터 계산
  for ( row.num in 2:nrow(dat) ) {
    dat[row.num,3:(p-1)] <- dat[row.num,3:(p-1)] + dat[(row.num-1),3:(p-1)]
  }
  # 추가 지표 계산
  SLG <- (dat$H + 2*dat$`2B` + 3*dat$`3B` + 4*dat$HR)/dat$AB # 장타율
  OBP <- (dat$H + dat$BB + dat$HBP)/(dat$AB + dat$BB + dat$HBP) # 출루율, 원래는 분모에 SF(희생플라이) 도 더해줘야 함 
  OPS <- SLG + OBP # OPS
  SLG <- round(SLG, 3) ; OBP <- round(OBP, 3) ; OPS <- round(OPS, 3)
  dat <- data.frame (dat, SLG, OBP, OPS)
  # 당일 타율 제거
  return(dat[,-3])
}
## output 형태 생성 함수
crawl.kbo <- function(row.player, write.as.csv=F) {
  dat <- crawl.mod(row.player) # dat는 data.frame 형태로 변환된 자료
  # 열의 개수를 이용하여 데이터가 없는 선수를 걸러냄(열의 개수가 1개면 데이터가 없는 것)
  if (ncol(dat) != 1) {
    # 투수 변수명 설정 및 numeric으로 변환
    if (row.player$pos == "p") { dat <- cal.pitcher(dat) }
    # 타자인 경우
    else  { dat <- cal.hitter(dat) }
    dat$date <- as.Date( gsub(".","/", dat$date,fixed=T), format="%m/%d")
    ret <- dat
    # csv로 쓰는 것을 설정할 경우
    if (write.as.csv) { write.csv( ret, file=paste(row.player$name,".csv"), row.names=F)  }
  } else {
    ret <- NA  # 1군 데이터가 없는 선수
  }
  return( ret )
}
## 파일 읽어오기 루프 함수
crawl.loop <- function(file=player_id, team=NULL, pos=NULL, write.as.csv=F) {
  if ( !is.null(team) ) { file <- file[file$team == team,]  }
  if ( !is.null(pos) ) { file <- file[file$pos == pos,] }
  for( i in 1:nrow(file) ) {
    vec <- file[i,] 
    name <- as.character(vec$name)
    temp <- crawl.kbo(vec, write.as.csv)
    if( !is.data.frame(temp) ) { 
      cat(name," has no data.","\n") 
    } else {
      assign(name, temp, envir=.GlobalEnv) ; cat("Data set is saved as", name,"\n")
    }
  }
}



## 결과값이 없는 경우 "---" 은 NA로 변환되는 Warning message 뜨지만 output은 괜찮다.
