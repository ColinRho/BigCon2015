## package "plyr" required  

##################################################################################################
## extracting homonyms ( needed in the first stage of data_sourcing.R )
homonym <- function( player_id ) {
  # extract duplicated names
  whosame <- player_id[ (duplicated(player_id$name) | duplicated(player_id$name, fromLast=T)), ]
  samename <- unique(whosame$name)
  # distinguish homonyms by adding team code after name 
  code <- sapply(as.character(whosame$team), team.code)
  whosame$name <- paste(whosame$name, code, sep="")
  # combining, sorting as team
  dat <- rbind( subset( player_id, !(name %in% samename) ), whosame )
  dat <- arrange( dat, team )
  # print changed data set and list of homonyms
  res <- list(dat=dat, samename=samename)
  return(res)
}
## a function changing players who have same name ( homonyme list, 'samename' needed )
change.homonym <- function( x, team ) { # x should be a vector in lineup of each team
  x <- as.matrix(x)
  # no homonyms
  if ( sum(x %in% samename) == 0 ) { return(x) }
  else {
    # players in the list of homonyms
    target <- x[which(x %in% samename)]
    # print after paste team code
    code <- team.code(team) 
    x[which(x %in% samename)] <- paste(target,code,sep="")
    return(x)  
  }
}
## a function loading lineup of teams for each games
get.lineup <- function( game ) { # game is one row vecotr of gamelist
  d <- as.character( game$date )
  aw <- as.character(game$away) ; ho <- as.character(game$home)
  mat <- subset(lineup, date == d & ( team == aw | team == ho  ) )
  return(mat)
}
## adding additional stats for hitters & pitchers
add.stat.hit <- function( row ) { # row should be an object from get.latest()
  # calculating additional stats
  AVG <- round( row[,'H'] / row[,'AB'], 3) # hit average
  SLG <- round( (row[,'H'] + 2*row[,'X2B'] + 3*row[,'X3B'] + 4*row[,'HR'])/row[,'AB'], 3)
  OBP <- round( (row[,'H'] + row[,'BB'] + row[,'HBP'])/(row[,'AB'] + row[,'BB'] + row[,'HBP']), 3) 
  OPS <- SLG + OBP # OPS
  SBPER <- round( row[,'SB']/(row[,'SB']+row[,'CS']), 3) # stolen base success percentage 
  # combining
  res <- data.frame( row, AVG, SLG, OBP, OPS, SBPER)
  return( res )
}
add.stat.pit <- function( row ) { # row should be an object from get.latest()
  # calculating additional stats
  ERA <- round( row[,'ER'] * 9 / row[,'IP'], 3)
  WHIP <- round( (row[,'HA'] + row[,'BBA'])/row[,'IP'], 3) 
  SOAPER <- round( (row[,'SOA']/9), 3) 
  BBAPER <- round( (row[,'BBA']/9),3) 
  LOBPER <- round( (row[,'HA'] + row[,'BBA'] + row[,'HBPA'] - row[,'RA'])/(row[,'HA'] + row[,'BBA'] + row[,'HBPA'] -(1.4*row[,'HRA'])),3) 
  # combining
  res <- data.frame( row, ERA, WHIP, SOAPER, BBAPER, LOBPER )
  return( res )
}
## 각 선수마다 경기 전날까지의 누적기록을 출력하는 함수
get.latest <- function ( player, gameDate ) {
  # 데이터 불러오기, crawl.loop 등으로 이미 GlobalEnv에 선수이름으로 된 데이터 행렬이 필요함
  dat <- get( player, envir=.GlobalEnv)
  # 경기일 이전의 데이터
  lgc <- dat$date < gameDate
  # 해당 경기일 전까지 누적데이터가 없는 경우(투수)
  if ( sum(lgc) == 0 & subset(player_id, name==player)$pos == "p") { 
    res <- as.data.frame( matrix( rep(0, length.out=ncol(양현종)), nrow=1 ) )
    colnames(res) <- colnames(양현종) ; res$date <- gameDate
    # manually delete needless variables
    res <- res[-c(2:5, length(res))]
  }
  # 해당 경기일 전까지 누적데이터가 없는 경우(타자)
  else if ( sum(lgc) == 0 & subset(player_id, name==player)$pos != "p") {
    res <- as.data.frame( matrix( rep(0, length.out=ncol(박병호)), nrow=1 ) )
    colnames(res) <- colnames(박병호) ; res$date <- gameDate
    # manually delete needless variables
    res <- res[-c(2,3,length(res))]
  }
  # 투수대표 양현종, 타자대표 박병호의 데이터가 있다는 가정하에...
  # 경기기록이 있는 경우
  else { 
    v <- subset(dat, date < gameDate) 
    v2 <- colSums( v[ sapply(v, is.numeric) ], na.rm = T ) # 하나의 row
    # deleting AVG1, ERA1, AVG, ERA ... will be calculated later
    res <- data.frame( date = dat$date[max(which(lgc))] , t( v2[-c(1,length(v2))] ) )
  }
  row.names(res) <- player
  ## adding additional stats
  if ( player %in% subset(player_id, pos == "p")$name ) { res <- add.stat.pit(res) }
  else { res <- add.stat.hit(res) }
  return(as.data.frame(res))
}
## 경기가 주어졌을 때 그 날의 전체 선발 라인업과 그 선수들의 전날까지 기록을 불러오는 함수
get.player.stat <- function ( game ) { 
  ### match는 gamelist의 각 row 
  ### samename은 동명이인 목록 lineup은 모든경기 라인업 목록
  x <- get.lineup( game )
  gameDate <- as.character(game$date)
  a.team <- as.character(game$away) ; h.team <- as.character(game$home)
  # 각팀 투수들
  away_total <- subset(x, team==a.team)[-(1:2)]
  home_total <- subset(x, team==h.team)[-(1:2)]
  # 동명이인 컨트롤
  away_total <- change.homonym(away_total, team=a.team)
  home_total <- change.homonym(home_total, team=h.team)
  # 투수들 기록
  away_pit <- get.latest(away_total[1], gameDate)  
  home_pit <- get.latest(home_total[1], gameDate)
  # 타자들 기록
  away_hit <- lapply( away_total[-1], get.latest, gameDate ) 
  home_hit <- lapply( home_total[-1], get.latest, gameDate )
  away_hit <- myrbind(away_hit) ; home_hit <- myrbind(home_hit)
  row.names(away_hit) <- away_total[-1] ; row.names(home_hit) <- home_total[-1]  
  # 결과출력
  res <- list(away_pit=away_pit, home_pit=home_pit, away_hit=away_hit, home_hit=home_hit)
  return(res)
}

######### fill-in '0' for particular row with no data 

## a function calculating win rate
winlose <- function ( game, team ) { # each row of gamelist as input
  splscore <- unlist(strsplit(as.character(game$score), ":" ))
  # draw
  score_away <- as.numeric( splscore[1] ) ; score_home <- as.numeric( splscore[2] ) 
  if ( score_away == score_home ) { return(NA) }
  # away
  if (team == game$away) {
    if ( score_away < score_home ) { result <- 0 }
    else { result <- 1 }
  }
  # home
  else if (team == game$home) {
    if ( score_away > score_home ) { result <- 0 }
    else  { result <- 1 }
  }
  return(result)
}
## 연승, 상대승률 함수
streak.or.vs <- function ( gameset, team, vs=NULL ) { 
  # 가장 최근의 연승 혹은 연패만 기록된다. gamelist의 subset을 date변수를 이용해 지정하므로서 특정 시기의 연속기록을 출력한다.
  # 이전경기 기록 없는경우 연속기록은 0, 상대승률은 NA를 출력한다
  if ( nrow(gameset)==0 & is.null(vs) ) { return(0) }
  if ( nrow(gameset)==0 & !is.null(vs) ) { return(NA) }
  # 해당 팀으로만 부분집합
  sub <- subset(gameset, (away == team | home == team))
  # 상대승률 옵션이 있을 시
  if (!is.null(vs)) {
    # 특정 팀과의 기록
    sub2 <- subset(sub, (away==vs | home==vs))
    # 승패 계산
    v <- adply( sub2, 1, winlose, team=team)$V1
    win.rate <- sum(v, na.rm=T) / nrow(sub2)
    return( round(win.rate,3) )
  }
  # 상대승률 옵션이 NULL일 때, 연승기록
  else {
    # 승패 계산
    x <- adply( sub, 1, winlose, team=team )$V1
    y <- rle(x)$lengths
    z <- rle(x)$values
    # 연속의 수
    str <- tail(y, 1)
    # 이전 게임이 무승부인 경우, 그 전까지의 연승 혹은 연패
    if ( is.na(tail(z,1)) ) { 
      z <- z[-length(z)] 
      str <- tail(y,2)[1]
    }
    # 승패에 따라 부호 결정  
    if (tail(z,1) == 1) { ind <- 1 }
    else if (tail(z,1) == 0) { ind <- -1 }
    else ind <- NA
    return(str*ind)
  }
}
## 선수들 연봉 데이터 호출 함수
salary <- function( game ) {
  x <- get.lineup(game)
  a.line <- x[1,-c(1,2)] ; h.line <- x[2,-c(1,2)]
  a.sal <- mean(subset(player_id, name %in% as.matrix(a.line))$salary, na.rm=T)
  h.sal <- mean(subset(player_id, name %in% as.matrix(h.line))$salary, na.rm=T)
  return( c(a.sal, h.sal) )
}
## stat weighted avg
## 지난시즌(특정시즌의 데이터셋을 입력) 의 기록을 각 선수별로 불러오는 함수
last.season <- function ( player, dataset ) {
  if ( player %in% dataset$name ) return( subset( dataset, name == player ) )
  # 지난시즌 기록이 없으면 지난 시즌의 평균으로 출력
  else  {
    cat(player, "(은)는 이전 시즌의 데이터가 없습니다","\n")
    v <- colMeans( dataset[ ,which(sapply( dataset, is.numeric ))], na.rm=T)
    nu <- data.frame(player, NA, t(v) )
    names(nu) <- colnames(dataset)
    return( nu ) 
  }
}
## 지난시즌과 이번시즌의 가중평균된 stat을 출력하는 함수
mix.stat <- function ( x, w, pitcher_2014, hitter_2014 ) { # x should be get.player.stat의 결과
  # x는 이번시즌의 기록, w는 지난시즌에 대한 가중
  if ( 0 >= w & 1 <= w ) stop("weight는 0과 1사이의 값이어야 합니다.")
  # 지난 시즌 데이터를 불러오기
  last.season.pit1 <- last.season( row.names(x[[1]]), pitcher_2014 )
  last.season.pit2 <- last.season( row.names(x[[2]]), pitcher_2014 )
  last.season.hit1 <- lapply( row.names(x[[3]]) , last.season, dataset=hitter_2014 )
  last.season.hit1 <- do.call(rbind, last.season.hit1)
  last.season.hit2 <- lapply( row.names(x[[4]]) , last.season, dataset=hitter_2014 )
  last.season.hit2 <- do.call(rbind, last.season.hit2)
  last.season.l <- list(last.season.pit1,last.season.pit2,last.season.hit1,last.season.hit2)
  # 지난 시즌과 이번시즌을 가중평균
  for(i in 1:4){
    num <- names(last.season.l[[i]])[sapply(last.season.l[[i]], is.numeric)]
    x[[i]] <- x[[i]][num]*(1-w) + last.season.l[[i]][num]*w
  }
  return(x)
} 
## 최종 데이터셋에서 한 case를 생성하는 함수
sum.stat <- function (game, w, div=F) { # w is weight of stats of last season
  sub <- subset ( gamelist , ( date < game$date & !is.na(score)) )
  # 선수들의 스탯을 불러오기
  x <- get.player.stat( game )
  y <- mix.stat(x, w, pitcher_2014, hitter_2014)
  a.team <- as.character(game$away) ;  h.team <- as.character(game$home)
  # 선발투수들 기록
  ap <- y$away_pit ; hp <- y$home_pit 
  # 타자들의 기록 합산
  ah <- round(colMeans( y$away_hit, na.rm=T ), 3)
  hh <- round(colMeans( y$home_hit, na.rm=T ), 3)
  # 각 팀의 연승기록 계산
  a.streak <- streak.or.vs(sub, team=a.team ) ; h.streak <- streak.or.vs(sub, team=h.team )
  # 상대승률 계산
  a.vs <- streak.or.vs(sub, team=a.team, vs=h.team) ; h.vs <- streak.or.vs(sub, team=h.team, vs=a.team)
  # 각팀의 승패결과(y값)
  a.win <- winlose( game, as.character(game$away)) ; h.win <- winlose( game, as.character(game$home))
  # 연봉데이터
  payment <- salary(game)
  # 데이터 결합
  away0 <- c(is_home=0, ap, ah, payment = payment[1], streak=a.streak, vs_rate=a.vs, win=a.win )
  home0 <- c(is_home=1, hp, hh, payment = payment[2],streak=h.streak, vs_rate=h.vs, win=h.win )
  var.name <- names(away0)
  away <- as.numeric(away0) ; home <- as.numeric(home0)
  names(away) <- var.name ; names(home) <- var.name
  # 나눔데이터 생성하고자 할 때
  if ( div ) {
    vec <- c( is.home=0, ap/hp, ah/hh, payment[1]/payment[2], a.streak - h.streak, vs_rate=a.vs, win=a.win ) 
    names(vec) <- var.name
    vec <- vec[-1]
    total <- data.frame( date=game$date, matchup = paste(a.team,h.team,sep="vs"), vec )
  }
  else  total <- data.frame( date=rep(game$date), team=c(a.team, h.team), rbind(away, home)) 
  return(total)
}  # 변수추가 필요
## case를 합쳐 데이터셋을 만드는 함수
aggr.stat <- function( gameset, w, div=F) {
  # 임의의 행렬
  l <- list()
  # 매 경기마다 sum.stat을 실행
  for ( i in 1:nrow(gameset) ) {
    l[[i]] <- sum.stat(gameset[i,], w, div)  
  }
  # 결합된 데이터
  return( myrbind(l) )
}

##################################################################################################