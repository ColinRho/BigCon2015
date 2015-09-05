## package "XML", "rvest", "data.table" required  
########## 0. tiny functions make calculation easier #############################################

## a function that combining rbindlist() and as.data.frame()
myrbind <- function( list, use.names = fill, fill = FALSE ) {
  l <- rbindlist(list, use.names = fill, fill = FALSE)
  return( as.data.frame(l) )
}
## a function converting numeric variable (input: vector)
convert.numeric <- function ( x ) {
  return(as.numeric( as.character (x) ))
}

##################################################################################################
########## 1. scraping player data ###############################################################

## converting IP variable of pitchers into numeric type
convert.IP <- function ( IP ) {
  IP <- as.character(IP)
  # dividing integer part and fraction part
  spl <- strsplit(IP, split=" ")
  for ( i in 1:length(spl) ) { 
    ## if IP is mixed fraction such as 1 1/3 
    if ( length(spl[[i]]) == 2) {
      IP[i] <- as.numeric(spl[[i]][1]) + eval( parse( text=spl[[i]][2] ))
    } 
    ## if IP is proper fraction such as 2/3
    else if ( nchar(spl[[i]]) != 1)
    {
      IP[i] <- eval( parse( text=spl[[i]] ))
    }
    ## if IP is an integer
    else {
      IP[i] <- as.numeric(spl[[i]])
    }
  }
  return(round(as.numeric(IP),3))
}
## a function reading webpage initially 
# http://www.koreabaseball.com/Record/Player/PitcherDetail/Daily.aspx?playerId=75852
crawl.read <- function (row.player) {  
  id <- row.player$id ; pos <- row.player$pos
  if (pos == "p") { 
    u <- "http://www.koreabaseball.com/Record/Player/PitcherDetail/Daily.aspx?playerId="
  } else {
    u <- "http://www.koreabaseball.com/Record/Player/HitterDetail/Daily.aspx?playerId="
  }
  url <- paste(u, id, "", sep="")
  a <- readHTMLTable(url) # Table
  # for debugging
  b <- readHTMLList(url)
  player <- b[[17]][1]
  cat("Data set of",player,"is being read","\n") 
  return(a)
}
## a function transfering crawled data into data.frame
crawl.mod <- function(row.player) {
  a <- crawl.read(row.player)
  for (i in 1:length(a)) {
    a[[i]] <- as.data.frame(a[[i]])
  }
  a <- myrbind(a)
  return(a)
}
## numeric converting and colnames setting (for pitchers)
cal.pitcher <- function ( dat ) { # dat should be a matrix from crawl.mod()
  p <- ncol(dat)
  # numeric conversion
  dat$IP <- round(convert.IP(dat$IP), 3)
  dat[,5:p] <- apply(dat[,5:p], 2, convert.numeric)
  # variable names setting
  colnames(dat)[1:4] <- c("date","vs","type","result")
  colnames(dat)[8:13] <- c("HA","HRA","BBA","HBPA","SOA","RA")
  return(dat)
}
## numeric converting and colnames setting (for hitters)
cal.hitter <- function ( dat ) { # dat should be a matrix from crawl.mod()
  p <- ncol(dat)
  # numeric conversion
  dat[,3:p] <- apply(dat[,3:p], 2, convert.numeric)
  # variable names setting
  colnames(dat)[1:2] <- c("date","vs") 
  colnames(dat)[7:8] <- c("X2B", "X3B")
  return(dat)
}
## a function generating proper output form
crawl.kbo <- function(row.player, write.as.csv=F, year="2015") { 
  dat <- crawl.mod(row.player)
  # filtering a player with no data using number of columns ( if it is 1, no data )
  if (ncol(dat) != 1) {
    # numeric conversion and variable names setting for pitchers 
    if (row.player$pos == "p") { dat <- cal.pitcher(dat) }
    # for hitters
    else  { dat <- cal.hitter(dat) }
    dat$date <- as.Date( gsub(".","", dat$date,fixed=T), format="%m%d")
    res <- dat
    # to write csv file
    if (write.as.csv) { write.csv( ret, file=paste(row.player$name,".csv"), row.names=F)  }
  } else {
    res <- NA  # a player with no data
  }
  return( res )
}
## reading loop
crawl.loop <- function(file=player_id, team=NULL, pos=NULL, write.as.csv=F) {
  # loop by team 
  if ( !is.null(team) ) { file <- file[file$team == team,]  }
  # loop by position
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

## In case of no data "---" changes into NA with Warning message, but output is ok.

##################################################################################################
########## 2. loading total match-up #############################################################

## http://sports.news.naver.com/schedule/index.nhn?uCategory=&category=kbo&year=2015&month=07&teamCode=&date=20150816
## a function generating match-up and score vector by scraping "NAVER SPORTS"
gamelist.match <- function ( matchup ) { # 한 경기씩
  # tell away and home
  s <- unlist( strsplit( matchup, split=":") )
  s <- gsub(pattern="\t|\n|\r","",s)
  # a case of cancelled ( no ":" in string, no game )
  if (length(s)==1) { 
    score <- NA
    away <- unlist( strsplit(s, "VS") )[1] ; home <- unlist( strsplit(s, "VS") )[2]
  }
  else {
    num <- gsub("\\D","", s)
    # score
    score <- paste( num[1], num[2], sep=":")
    away <- gsub("\\d","",s)[1] ; home <- gsub("\\d","",s)[2]
  }
  mat <- data.frame(away, home, score)
  return(mat)
}
## a function making proper date form from scraped NAVER page
date.trans <- function( x, year ) { # b[1,1] as a input
  md <- unlist(strsplit(x," "))[1]
  # combining year
  ymd <- paste(year,md,sep="") 
  date <- as.Date( gsub(".","/", ymd, fixed=T), format="%Y%m/%d")
  return(date)
}
## modifying matrix of each game from crawled html page
gamelist.mod <- function ( b, year="2015" ) { # b is a matrix of each game
  n <- nrow(b)
  # game day
  date <- rep(date.trans(b[1,1], year), n)
  # beginning time, match-up, stadium
  # considering a day with only one game
  if ( nrow(b) == 1 ) {
    time <- b[2] ; matchup <- b[3] ; stadium <- b[6]
  } else {
    time <- c(b[1,2],b[2:n,1])  
    matchup <- as.vector( c(b[1,3], b[2:n,2]) )
    stadium <- c(b[1,6], b[2:n,5])
  }
  # extracting game score
  match <- apply(as.matrix(matchup), 1, gamelist.match)
  match <- do.call(rbind, match)
  # cancelled game
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
## scraping game result by each month ( NAVER Sports )
gamelist.monthly <- function ( month="08", year ="2015" ) {
  # today as a proper date format
  today <- format(Sys.Date(), "%Y%m%d")
  # setting url
  baseurl <- c("http://sports.news.naver.com/schedule/index.nhn?uCategory=&category=kbo&year=","&month=","&teamCode=&date=")
  url <- paste(baseurl[1], year, baseurl[2], month , baseurl[3], today, sep="")
  # crawl package: XML
  a <- readHTMLTable(url)
  # a list listing games everyday
  l <- list()
  # the first and the last of list is not data part
  for ( i in 2:(length(a)-1) ) {
    temp <- as.matrix(a[[i]])
    # except a day with no game
    if (ncol(temp) > 3) {
      temp <- gamelist.mod( temp , year)
      l <- c(l, list(temp))
    }
  }
  # rbinding list
  l <- myrbind(l) 
  return(l)
}
## generating final result of gamelist ( Months vector should be input )
gamelist.total <- function( month, year="2015" ) { 
  open <- opening[ which(format(opening, "%Y") == year) ]
  month <- month[ which (month >= format(open, "%m")) ]
  l <- list()
  # if month is a vector more than 2 elements
  for (i in 1:length(month)) {
    l <- c(l, list(gamelist.monthly( month[i], year )) )
  }
  boxscore <- myrbind(l)
  bs <- subset(boxscore, date < Sys.Date() & date >= open)
  return(bs)
}

##################################################################################################
########## 3. loading starting lineup of each match-up ###########################################

## http://www.koreabaseball.com/Schedule/Game/BoxScore.aspx?leagueId=1&seriesId=0&gameId=20150801LGSK0&gyear=2015
## a function changing team code according to KBO url
team.code <- function ( x ) { # x: single character
  if ( x == "삼성") { y <- "SS"}
  else if ( x == "넥센") { y <- "WO"}
  else if ( x == "한화") { y <- "HH"}
  else if ( x == "롯데") { y <- "LT"}
  else if ( x == "두산") { y <- "OB"}
  else if ( x == "kt") { y <- "KT"}
  else if ( x == "KIA" ) { y <- "HT"}
  # All-star games
  else if ( x == "웨스턴"| x=="나눔") { y <- "WE"}
  else if ( x == "이스턴"| x=="드림") { y <- "EA"}
  # no change for LG, NC, SK
  else y <- x
  return(y)
}
## generating KBO boxsocre url of each game
game.url <- function( row.game ) {
  vec <- row.game
  if (vec$stadium == "취소") { return(NA) }
  # game date and year
  date <- format(vec$date, "%Y%m%d") ; year <- format(vec$date, "%Y")
  # away/home code
  away <- team.code(vec$away) ; home <- team.code(vec$home)
  index <- paste(date,away,home,sep="")
  # kbo BoxScore url
  baseurl <- c("http://www.koreabaseball.com/Schedule/Game/BoxScore.aspx?leagueId=1&seriesId=","&gameId=","0&gyear=")
  # put post-season period
  pday <- post.day( year )
  # series code 
  # Korean Series ( 15 days after post-season started )
  if ( vec$date >= pday+15 & !is.na(pday) ) { seriesid <- 7 } 
  # Playoff ( 7 days after post-season started )
  else if ( vec$date >= pday+7 & !is.na(pday) ) { seriesid <- 5 }
  # semi-playoff
  else if ( vec$date >= pday & !is.na(pday) ) { seriesid <- 3 } 
  else if ( away == "EA" | away == "WE" ) { seriesid <- 9 }
  else { seriesid <- 0 }
  url <- paste(baseurl[1], seriesid, baseurl[2], index,baseurl[3],year,sep="")
  return(url)
}
## extracting starting hitters in each games web page 
lineup.hitter <- function( a ) { # a는 crawl된 행렬중 3번 4번행렬(타자정보)
  # hitting order and names 
  l <- a[,c(1,2,3)]
  # the first player of each order as starting player
  l <- l[!duplicated(l[,1]),]
  lineup <- data.frame( pos=substr(l[,2],1,1) ,name=l[,3], stringsAsFactors = F)
  return(lineup)
}
## a function only for "이병규"
byungkyu <- function ( x ){
  # x is hitter_list
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
## extracing starting lineup of each games
lineup.each <- function( row.game ) {
  vec <- row.game
  # generating web page of each games' boxscore 각 경기의 박스스코어 웹페이지 url 생성
  url <- game.url(vec)
  # scraping each games' boxscore from kbo
  a <- readHTMLTable(url)
  for( i in 1:length(a) ) {
    a[[i]] <- as.matrix(a[[i]])
  }
  ## 1: game score
  ## 2: primary record 
  ## 3: away hitters lineup
  ## 4: home hitters hineup
  ## 5: away pitcher
  ## 6: home pitcher
  away_team <- as.character(vec$away) ; home_team <- as.character(vec$home)
  # away team starting pitcher
  away_start <- as.vector( a[[5]][1,1] )  
  # home team starting pitcher
  home_start <- as.vector( a[[6]][1,1] )
  # away team starting hitters 
  away_hitter <- lineup.hitter(a[[3]])
  # home team starting hitters 
  home_hitter <- lineup.hitter(a[[4]])
  # 이병규
  if ( away_team == "LG") { away_hitter <- byungkyu(away_hitter) }
  if ( home_team == "LG") { home_hitter <- byungkyu(home_hitter) } 
  # arranging lineup of away/home 
  away <- c(away_team , away_start, away_hitter$name)
  home <- c(home_team , home_start, home_hitter$name)
  # result matrix( date, team, start_pitcher, start hitters )
  mat <- t(data.frame(away, home))
  colnames(mat) <- c("team", "start_pitcher",1:9)
  mat <- data.frame(date=vec$date, mat)
  return(mat)
}
## print final lineup given gamelist
lineup.total <- function( x, by.month=NULL ) { # x should be gamelist, use by.month="05" for monthly output 
  l <- list()
  # deleting cancelled games
  x <- subset(x, !is.na(score))
  # when printing by a month
  if ( !is.null(by.month) ) x <- subset(x, format(date, "%m") == by.month )
  for (i in 1:nrow(x)) {
    l <- c(l, list(lineup.each( x[i,] )) )
  }
  lineup <- myrbind(l)
  return(lineup)
}

##################################################################################################
