## package "plyr" required  

## 동명이인 추출하는 함수(데이터 소스의 가장 첫단계에서 필요)
homonym <- function( player_id ) {
  # 동명이인들의 이름
  # 동명이인들만을 추출
  whosame <- player_id[ (duplicated(player_id$name) | duplicated(player_id$name, fromLast=T)), ]
  samename <- unique(whosame$name)
  # 이름에 팀 코드를 붙여서 동명이인을 구분
  code <- sapply(as.character(whosame$team), team.code)
  whosame$name <- paste(whosame$name, code, sep="")
  # 변경된 데이터를 결합, 팀별로 sorting
  dat <- rbind( subset( player_id, !(name %in% samename) ), whosame )
  dat <- arrange( dat, team )
  # 변경된 데이터셋과 동명이인의 목록을 리스트로 함께 출력
  res <- list(dat=dat, samename=samename)
  return(res)
}
## 동명이인인 선수들의 이름을 바꾸는 함수(동명이인 목록 필요함)
change.homonym <- function( x, team ) { # x는 각 팀별 라인업에 있는 선수들 벡터, team은 해당 팀명
  x <- as.matrix(x)
  # 동명이인 없음
  if ( sum(x %in% samename) == 0 ) { return(x) }
  else {
    # 동명이인 리스트에 있는 선수들
    target <- x[which(x %in% samename)]
    # 팀 코드를 붙여서 출력
    code <- team.code(team) 
    x[which(x %in% samename)] <- paste(target,code,sep="")
    return(x)  
  }
}
## 각 경기의 양팀 라인업을 불러오는 함수
get.lineup <- function( game ) { # game은 gamelist의 한 row벡터
  d <- as.character( game$date )
  aw <- as.character(game$away) ; ho <- as.character(game$home)
  mat <- subset(lineup, date == d & ( team == aw | team == ho  ) )
  return(mat)
}
## 각 선수마다 경기 전날까지의 누적기록을 출력하는 함수
get.latest <- function ( player, gameDate ) {
  # 데이터 불러오기, crawl.loop 등으로 이미 GlobalEnv에 선수이름으로 된 데이터 행렬이 필요함
  dat <- get( player, envir=.GlobalEnv)
  # 경기일 이전의 데이터
  lgc <- dat$date < gameDate
  # 해당 경기일 전까지 누적데이터가 없는 경우(투수)
  if ( sum(lgc) == 0 & subset(player_id, name==player)$pos == "p") { 
    res <- matrix( rep(0, length.out=ncol(양현종)), nrow=1 )
    colnames(res) <- colnames(양현종)
  }
  # 해당 경기일 전까지 누적데이터가 없는 경우(타자)
  else if ( sum(lgc) == 0 & subset(player_id, name==player)$pos != "p") {
    res <- matrix( rep(0, length.out=ncol(박병호)), nrow=1 ) 
    colnames(res) <- colnames(박병호)
  }
  # 투수대표 양현종, 타자대표 박병호의 데이터가 있다는 가정하에...
  # 경기기록이 있는 경우
  else { 
    v <- dat$date[lgc] 
    res <- subset(dat, date == max(v)) # 하나의 row
  }
  row.names(res) <- player
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

######### 데이터가 없는 경우에 해당 row를 0으로 채우게 된다

## 승패 계산 함수
winlose <- function ( game, team ) { # gamelist의 각 row를 input으로
  splscore <- unlist(strsplit(as.character(game$score), ":" ))
  # 무승부
  score_away <- as.numeric( splscore[1] ) ; score_home <- as.numeric( splscore[2] ) 
  if ( score_away == score_home ) { return(NA) }
  # 원정일 경우
  if (team == game$away) {
    if ( score_away < score_home ) { result <- 0 }
    else { result <- 1 }
  }
  # 홈일 경우
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

