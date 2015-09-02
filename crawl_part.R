## package "XML", "rvest", "data.table" required  
########## 몇가지 계산을 편하게 하기위한 함수들 ##################################################
## rbindlist와 as.data.frame을 결합한 함수
myrbind <- function( list, use.names = fill, fill = FALSE ) {
  l <- rbindlist(list, use.names = fill, fill = FALSE)
  return( as.data.frame(l) )
}
## numeric 변수로 변환하는 함수(input: 벡터)
convert.numeric <- function ( x ) {
  return(as.numeric( as.character (x) ))
}
##################################################################################################


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
# http://www.koreabaseball.com/Record/Player/PitcherDetail/Daily.aspx?playerId=75852
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
    a[[i]] <- as.data.frame(a[[i]])
  }
  a <- myrbind(a)
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
  colnames(dat)[8:13] <- c("HA","HRA","BBA","HBPA","SOA","RA")
  return(dat)
}
## numeric 변환 및 누적 데이터 계산 함수(타자용)
cal.hitter <- function ( dat ) { # dat crawl.mod 결과로 출력된 행렬이어야 한다.
  p <- ncol(dat)
  # numeric으로 변환
  dat[,3:p] <- apply(dat[,3:p], 2, convert.numeric)
  # 변수명 설정
  colnames(dat)[1:2] <- c("date","vs") ; colnames(dat)[p] <- "AVG"
  colnames(dat)[7:8] <- c("X2B", "X3B")
  return(dat)
}
## output 형태 생성 함수
crawl.kbo <- function(row.player, write.as.csv=F, year="2015") { # 나중에 다른연도 필요할 수도...
  dat <- crawl.mod(row.player) # dat는 data.frame 형태로 변환된 자료
  # 열의 개수를 이용하여 데이터가 없는 선수를 걸러냄(열의 개수가 1개면 데이터가 없는 것)
  if (ncol(dat) != 1) {
    # 투수 변수명 설정 및 numeric으로 변환
    if (row.player$pos == "p") { dat <- cal.pitcher(dat) }
    # 타자인 경우
    else  { dat <- cal.hitter(dat) }
    dat$date <- as.Date( gsub(".","", dat$date,fixed=T), format="%m%d")
    res <- dat
    # csv로 쓰는 것을 설정할 경우
    if (write.as.csv) { write.csv( ret, file=paste(row.player$name,".csv"), row.names=F)  }
  } else {
    res <- NA  # 1군 데이터가 없는 선수
  }
  return( res )
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


## 전체 매치업을 불러오는 단계
## http://sports.news.naver.com/schedule/index.nhn?uCategory=&category=kbo&year=2015&month=07&teamCode=&date=20150816
## naver페이지 크롤링에서 경기 매치업과 스코어 벡터를 생성하는 함수
gamelist.match <- function ( matchup ) { # 한 경기씩
  # 어웨이와 홈을 구분
  s <- unlist( strsplit( matchup, split=":") )
  s <- gsub(pattern="\t|\n|\r","",s)
  # 취소된 경우(":"로 나뉘지 않으면 경기가 치러지지 않은 것)
  if (length(s)==1) { 
    score <- NA
    away <- unlist( strsplit(s, "VS") )[1] ; home <- unlist( strsplit(s, "VS") )[2]
  }
  else {
    num <- gsub("\\D","", s)
    # 스코어
    score <- paste( num[1], num[2], sep=":")
    away <- gsub("\\d","",s)[1] ; home <- gsub("\\d","",s)[2]
  }
  mat <- data.frame(away, home, score)
  return(mat)
}
## naver페이지 크롤링에서 날짜 폼을 만드는 함수
date.trans <- function( x, year ) { # b[1,1]을 인풋으로
  md <- unlist(strsplit(x," "))[1]
  # 연도 결합
  ymd <- paste(year,md,sep="") 
  date <- as.Date( gsub(".","/", ymd, fixed=T), format="%Y%m/%d")
  return(date)
}
## 각 경기마다 크롤링된 html을 적절한 행렬로 조작하는 함수
gamelist.mod <- function ( b, year="2015" ) { # b 는 각 경기의 행렬
  n <- nrow(b)
  # 경기일
  date <- rep(date.trans(b[1,1], year), n)
  # 경기 시작시간, 매치업, 경기장
  # 우취로 인한 추가편성 등으로 하루에 경기가 하나뿐인 경우를 고려
  if ( nrow(b) == 1 ) {
    time <- b[2] ; matchup <- b[3] ; stadium <- b[6]
  } else {
    time <- c(b[1,2],b[2:n,1])  
    matchup <- as.vector( c(b[1,3], b[2:n,2]) )
    stadium <- c(b[1,6], b[2:n,5])
  }
  # 경기 스코어 추출
  match <- apply(as.matrix(matchup), 1, gamelist.match)
  match <- do.call(rbind, match)
  # 취소된 경기
  stadium[nchar(stadium) == 0] <- "취소"
  mat <- data.frame( date, time, match, stadium )
  return(mat)
}
## vector of opening day since 2008
opening <- c("2008-03-29", "2009-04-04", "2010-03-27", "2011-04-02", "2012-04-07", "2013-03-30", "2014-03-29","2015-03-28")
opening <- as.Date(opening)
## get postseason period by crawling KBO web page
post.day <- function( year = "2015" ){
  baseurl <- "http://www.koreabaseball.com/Schedule/GameList/PostSeason.aspx?gyear="
  url <- paste( baseurl, year, sep="")
  # crawling webpage and take the first date as a beginning day of post season
  a <- readHTMLTable(url)[[1]] ; day <- as.character(a[1,1])
  # let 'day' meet Date form
  d1 <- gsub("\\D","", unlist( strsplit(day, "\\.") ) )
  if ( length(d1) == 1 ) return(NA)
  d <- as.Date( paste( c(year, d1), collapse = "-" ) )
  return(d)
}
## 매월별로 경기결과를 크롤링하는 함수(네이버 스포츠)
gamelist.monthly <- function ( month="08", year ="2015" ) {
  # 오늘날짜를 url 형식에 맞도록
  today <- format(Sys.Date(), "%Y%m%d")
  # url 설정
  baseurl <- c("http://sports.news.naver.com/schedule/index.nhn?uCategory=&category=kbo&year=","&month=","&teamCode=&date=")
  url <- paste(baseurl[1], year, baseurl[2], month , baseurl[3], today, sep="")
  # crawl package: XML
  a <- readHTMLTable(url)
  # 매일마다 경기를 정리하는 list
  l <- list()
  # list의 범위중 맨처음과 끝은 열이름(data아님)
  for ( i in 2:(length(a)-1) ) {
    temp <- as.matrix(a[[i]])
    # 경기가 없는날은 제외
    if (ncol(temp) > 3) {
      temp <- gamelist.mod( temp , year)
      l <- c(l, list(temp))
    }
  }
  # list를 행렬로 만들고, 개막일부터 오늘 이전까지의 자료만
  l <- myrbind(l) 
  return(l)
}
## 최종 결과출력 함수 (Months 벡터가 input으로 들어가야 함)
gamelist.total <- function( month, year="2015" ) { 
  open <- opening[ which(format(opening, "%Y") == year) ]
  month <- month[ which (month >= format(open, "%m")) ]
  l <- list()
  # month가 크기 2이상의 벡터인 경우
  for (i in 1:length(month)) {
    l <- c(l, list(gamelist.monthly( month[i], year )) )
  }
  boxscore <- myrbind(l)
  bs <- subset(boxscore, date < Sys.Date() & date >= open)
  return(bs)
}


## 각 매치업의 선발 라인업을 불러오는 단계
## http://www.koreabaseball.com/Schedule/Game/BoxScore.aspx?leagueId=1&seriesId=0&gameId=20150801LGSK0&gyear=2015

## kbo의 url에 맞게 팀 코드를 바꾸는 함수
team.code <- function ( x ) { # x: single character
  if ( x == "삼성") { y <- "SS"}
  else if ( x == "넥센") { y <- "WO"}
  else if ( x == "한화") { y <- "HH"}
  else if ( x == "롯데") { y <- "LT"}
  else if ( x == "두산") { y <- "OB"}
  else if ( x == "kt") { y <- "KT"}
  else if ( x == "KIA" ) { y <- "HT"}
  # 올스타전
  else if ( x == "웨스턴"| x=="나눔") { y <- "WE"}
  else if ( x == "이스턴"| x=="드림") { y <- "EA"}
  # no change for LG, NC, SK
  else y <- x
  return(y)
}
## 각 게임의 kbo BOXSCORE url을 생성하는 함수
game.url <- function( row.game ) {
  vec <- row.game
  if (vec$stadium == "취소") { return(NA) }
  # 경기 날짜와 연도
  date <- format(vec$date, "%Y%m%d") ; year <- format(vec$date, "%Y")
  # 어웨이/홈팀 코드
  away <- team.code(vec$away) ; home <- team.code(vec$home)
  index <- paste(date,away,home,sep="")
  # kbo BoxScore url
  baseurl <- c("http://www.koreabaseball.com/Schedule/Game/BoxScore.aspx?leagueId=1&seriesId=","&gameId=","0&gyear=")
  # 포스트 시즌 날짜를 구분
  pday <- post.day( year )
  # 시리즈코드 정규, 포스트시즌, 올스타전
  if ( vec$date >= pday+15 & !is.na(pday) ) { seriesid <- 7 } # 한국시리즈 (포스트시즌 시작일 기준 15일 후)
  else if ( vec$date >= pday+7 & !is.na(pday) ) { seriesid <- 5 } # 플레이오프 (포스트시즌 시작일 기준 7일 후)
  else if ( vec$date >= pday & !is.na(pday) ) { seriesid <- 3 } # 준플레이오프
  else if ( away == "EA" | away == "WE" ) { seriesid <- 9 }
  else { seriesid <- 0 }
  url <- paste(baseurl[1], seriesid, baseurl[2], index,baseurl[3],year,sep="")
  return(url)
}
## 각 경기 페이지에서 선발타자를 추출하는 함수
lineup.hitter <- function( a ) { # a는 crawl된 행렬중 3번 4번행렬(타자정보)
  # 타순과 이름만 정렬
  l <- a[,c(1,2,3)]
  # 각 타순번호의 첫번째 선수를 선발라인업으로
  l <- l[!duplicated(l[,1]),]
  lineup <- data.frame( pos=substr(l[,2],1,1) ,name=l[,3], stringsAsFactors = F)
  return(lineup)
}
## 이병규 처리용 함수
byungkyu <- function ( x ){
  # x 는 hitter_list
  out <- c("좌","중","우")
  num <- which(x$name == "이병규")
  bk <- x[num,]
  if (nrow(bk) == 0) return(x)
  else {
    bk$name[which(bk$pos %in% out)] <- "이병규Y"
    bk$name[which(bk$pos == "지")] <- "이병규O"
    if (nrow(bk) == 2 & bk$name[1] == bk$name[2]) {
      bk$name[2] <- "이병규O"
    }
    x$name[num] <- bk$name
  }
  
  return(x)
}
## 각 경기의 선발명단을 추출하는 함수
lineup.each <- function( row.game ) {
  vec <- row.game
  # 각 경기의 박스스코어 웹페이지 url 생성
  url <- game.url(vec)
  # kbo에서 각 경기 박스스코어 페이지 크롤링
  a <- readHTMLTable(url)
  for( i in 1:length(a) ) {
    a[[i]] <- as.matrix(a[[i]])
  }
  ## 1: 경기 스코어
  ## 2: 주요기록
  ## 3: 어웨이 타자 라인업
  ## 4: 홈 타자 라인업
  ## 5: 어웨이 투수 
  ## 6: 홈 투수
  away_team <- as.character(vec$away) ; home_team <- as.character(vec$home)
  
  # 원정팀 선발투수
  away_start <- as.vector( a[[5]][1,1] )  
  # 홈팀 선발투수
  home_start <- as.vector( a[[6]][1,1] )
  # 원정팀 선발타선
  away_hitter <- lineup.hitter(a[[3]])
  # 홈팀 선발타선
  home_hitter <- lineup.hitter(a[[4]])
  # 이병규 해결
  if ( away_team == "LG") { away_hitter <- byungkyu(away_hitter) }
  if ( home_team == "LG") { home_hitter <- byungkyu(home_hitter) } 
  # 원정/홈팀의 라인업 정리
  away <- c(away_team , away_start, away_hitter$name)
  home <- c(home_team , home_start, home_hitter$name)
  # 결과행렬(날짜, 팀명, 선발투수, 선발라인업)
  mat <- t(data.frame(away, home))
  colnames(mat) <- c("team", "start_pitcher",1:9)
  mat <- data.frame(date=vec$date, mat)
  
  return(mat)
}
## Gamelist가 주어졌을때 최종 라인업을 출력
lineup.total <- function( x, by.month=NULL ) { # x gamelist여야함, 월별로 출력하려면 by.month="05" 등
  l <- list()
  # 취소되었던 경기는 제외
  x <- subset(x, !is.na(score))
  # 월별로 출력할 경우
  if ( !is.null(by.month) ) x <- subset(x, format(date, "%m") == by.month )
  for (i in 1:nrow(x)) {
    l <- c(l, list(lineup.each( x[i,] )) )
  }
  lineup <- myrbind(l)
  return(lineup)
}

