## package "XML", "rvest", "data.table" required  

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
# 네이버 검색결과를 통해 각 해의 개막일을 추출(포스트 시즌 추가)
opening.day <- function ( year="2015", post.season=F ) {
  preurl <- "http://search.naver.com/search.naver?sm=tab_hty.top&where=nexearch&ie=utf8&query="
  # 포스트 시즌이 시작되는 날짜
  if ( post.season ) { posturl <- "프로야구+포스트시즌" }
  else { posturl <- "프로야구" }
  url <- paste( preurl, year, posturl, sep="")
  # html 스크립트와 개막일 정보가 있는 node
  script <- html(url)
  node <- html_nodes(script, "dd")[[5]]
  date <- substr( html_text(node), 1, 12)
  return(as.Date( gsub("\\D", " ", date), "%Y %m %d"))
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
  open <- opening.day(year)
  l <- rbindlist(l) ; l <- subset(l, date < Sys.Date() & date >= open)
  return(as.data.frame(l))
}
## 최종 결과출력 함수 (Months 벡터가 input으로 들어가야 함)
gamelist.total <- function( month, year="2015" ) { 
  l <- list()
  # month가 크기 2이상의 벡터인 경우
  for (i in 1:length(month)) {
    l <- c(l, list(gamelist.monthly( month[i], year )) )
  }
  boxscore <- rbindlist(l)
  return(as.data.frame(boxscore))
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
  pday <- opening.day( year, T)
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
  lineup <- rbindlist(l)
  return(as.data.frame(lineup))
}

