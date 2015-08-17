

game <- gamelist[151,]

## 각 경기의 양팀 라인업을 불러오는 함수
get.lineup <- function( match, lineup ) { # match 는 gamelist의 한 row벡터
  d <- as.character( match$date )
  aw <- as.character(match$away) ; ho <- as.character(match$home)
  mat <- subset(lineup, date == d & ( team == aw | team == ho  ) )
  return(mat)
}
l <- get.lineup(game, lineup2)

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

##
get.player.stat <- function ( x ) { # x는 get.lineup에 의한 한 경기의 라인업
  if (nrow(x) !=2) stop("한 경기에는 두 팀의 정보가 있어야 합니다")
  gameDate <- as.character(x$date[1])
  # 각팀 투수들
  pitchers <- as.character( x$start_pitcher )
  away_pit <- get.latest(pitchers[1], gameDate) 
  home_pit <- get.latest(pitchers[2], gameDate)
  
  # 각팀 타자들
  away_hit_list <- as.character( as.matrix( x[1,4:12] ) )
  home_hit_list <- as.character( as.matrix( x[2,4:12] ) ) 
  away_hit <- lapply( away_hit_list, get.latest, gameDate )
  home_hit <- lapply( home_hit_list, get.latest, gameDate )
  away_hit <- do.call(rbind, away_hit) ; home_hit <- do.call(rbind, home_hit)
  row.names(away_hit) <- away_hit_list
  row.names(home_hit) <- home_hit_list
  
  res <- list(away_pit, home_pit, away_hit, home_hit)
  return(res)
}
get.player.stat(l)
