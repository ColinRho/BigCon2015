pit <- html("http://www.koreabaseball.com/Record/Player/PitcherDetail/Daily.aspx?playerId=75852")
html_nodes(pit, "select")

## 해결해야할 문제들: 이병규, lineup데이터에서 첫번째 열이 하나 추가된다. 

## 동명이인을 핸들링하는 알고리즘
## 1. 전체 선수목록(player_id.csv) 에서 동명이인을 찾아서 lableling,
## 2. lableling 규칙
##  1) 소속팀의 고유기호
##  2) 소속팀이 같은경우 ...배번 혹은 임의 indicator
## 3. labeling 하는 과정에서 동명이인이 있는 이름들을 수집
## 4. crawl.loop()을 통해 개인별 데이터는 labeling된 이름으로 저장됨
## 5. lineup.total()을 통해 크롤링된 데이터에는 labeling 되어있지 않음
##  if 팀명으로 구분이 가능하다면 앞서 수집된 동명이인들의 이름에 팀 code를 다시 label
##  else if 팀명만으로 구분이 불가능하다면(LG 이병규)...how?
## 6. 최종적으로 개인데이터가 저장된 이름(player_id에 저장된 이름)과 lineup데이터의 이름이 일치하도록 따라서 get.() 함수들이 
##  unique하게 데이터셋을 불러오게 한다.

## 팀 label team.code와 동일하게
## LG - LG, SK - SK,NC - NC, kt - KT, KIA - HT, 삼성 - SS,  한화 - HH,  롯데 -  LT, 두산 - OB, 넥센 - WO

winlose <- function ( game, team ) { # gamelist의 각 row를 input으로
  splscore <- unlist(strsplit(as.character(game$score), ":" ))
  # 무승부
  if ( splscore[1] == splscore[2] ) { return(NA) }
  # 원정일 경우
  if (team == game$away) {
    if ( splscore[1] < splscore[2] ) { result <- 0 }
    else { result <- 1 }
  }
  # 홈일 경우
  else if (team == game$home) {
    if ( splscore[1] > splscore[2] ) { result <- 0 }
    else  { result <- 1 }
  }
  return(result)
}
## 연승, 상대승률 
streak <- function ( gamelist, team ) {
  # 해당 팀으로만 부분집합
  sub <- subset(gamelist, (away == team | home == team))
  # 승패 계산
  x <- adply( sub, 1, winlose, team=team )
  y <- rle(x[,ncol(x)])$lengths
  z <- rle(x[,ncol(x)])$values
  # 연속의 수
  str <- tail(y, 1)
  # 승패에 따라 부호 결정
  if (tail(z,1) == 1) { ind <- 1 }
  else if (tail(z,1) == 0) { ind <- -1 }
  else ind <- NA
  return(str*ind)
}


finalset <- function( gamelist, lineup, samename ) {
  #
  set <- subset(gamelist, !is.na(score) )
  for ( i in 1:nrow(set) ) {
    x <- get.player.stat( set[i,] , lineup, samename )
    
  }
  
  p <- ncol(x[[1]])
  
  h <- ncol(x[[3]])
  
  ## 승패를 표시
  
  ah <- colSums( x$away_hit[3:h] )
  hh <- colSums( x$home_hit[3:h] )
  away <- data.frame( x[[1]], ah)
  home <- data.frame( x[[2]], hh)
  
  
}

