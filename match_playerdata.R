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
change.homonym <- function( x, team, samename=samename ) { # x는 각 팀별 라인업에 있는 선수들 벡터, team은 해당 팀명
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
get.lineup <- function( match, lineup ) { # match 는 gamelist의 한 row벡터
  d <- as.character( match$date )
  aw <- as.character(match$away) ; ho <- as.character(match$home)
  mat <- subset(lineup, date == d & ( team == aw | team == ho  ) )
  return(mat)
}
## 각 선수마다 경기 전날까지의 누적기록을 출력하는 함수
get.latest <- function ( player="", gameDate ) {
  # 데이터 불러오기, crawl.loop 등으로 이미 GlobalEnv에 선수이름으로 된 데이터 행렬이 필요함
  dat <- get( player, envir=.GlobalEnv)
  # 경기일 이전의 데이
  lgc <- dat$date < gameDate
  # 해당 경기일 전까지 누적데이터가 없는 경우
  if ( sum(lgc) == 0) { return(NA) }
  else { 
    v <- dat$date[lgc] 
    res <- subset(dat, date == max(v))
    return(res)
  }
}
## 아직 이병규 문제 해결 안됨
get.player.stat <- function ( match, lineup, samename ) { 
  ### match는 gamelist의 각 row 
  ### samename은 동명이인 목록 lineup은 모든경기 라인업 목록
  x <- get.lineup( match, lineup)
  gameDate <- as.character(x$date[1])
  # 각팀 투수들
  pitchers <- as.character( x$start_pitcher ) 
  # 각팀 타자들
  away_hit_list <- as.character( as.matrix( x[1,4:12] ) )
  home_hit_list <- as.character( as.matrix( x[2,4:12] ) )
  away_total <- c(pitchers[1], away_hit_list)
  home_total <- c(pitchers[2], home_hit_list)
  # 동명이인 컨트롤
  away_total <- change.homonym(away_total, team=x$team[1], samename)
  home_total <- change.homonym(home_total, team=x$team[2], samename)
  # 투수들 기록
  away_pit <- get.latest(away_total[1], gameDate) 
  home_pit <- get.latest(home_total[1], gameDate)
  # 타자들 기록
  away_hit <- lapply( away_total[-1], get.latest, gameDate )
  home_hit <- lapply( home_total[-1], get.latest, gameDate )
  away_hit <- do.call(rbind, away_hit) ; home_hit <- do.call(rbind, home_hit)
  row.names(away_hit) <- away_hit_list
  row.names(home_hit) <- home_hit_list
  # 결과출력
  res <- list(away_pit, home_pit, away_hit, home_hit)
  names(res) <- c(pitchers, "away_hit","home_hit")
  return(res)
}



