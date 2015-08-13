## package "XML" required  
## source() 로 이 페이지의 코드를 읽어올 때 한글이 출력될 때 Warning message

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
crawl.mod <- function(a) {
  for (i in 1:length(a)) {
    a[[i]] <- as.matrix(a[[i]])
  }
  a <- do.call(rbind, a)
  a <- as.data.frame(a)
  return(a)
}
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
## 최종 output생성 함수
crawl.kbo <- function(row.player, write.as.csv=F) {
  a <- crawl.read(row.player)
  dat <- crawl.mod(a) # dat는 data.frame 형태로 변환된 자료
    # 열의 개수를 이용하여 데이터가 없는 선수를 걸러냄(열의 개수가 1개면 데이터가 없는 것)
  if (ncol(dat) != 1) {
    var.name <- colnames(dat)
      # 투수/타자에 따라 변수명 설정 및 numeric으로 변환
    if (row.player$pos == "p") { 
      var.name[1:4] <- c("date", "vs", "type" ,"result")
      dat$IP <- convert.IP(dat$IP)
      dat[,5:ncol(dat)] <- apply(dat[,5:ncol(dat)], 2, convert.numeric)
    }  else  { 
      var.name[1:2] <- c("date", "vs")
      dat[,3:ncol(dat)] <- apply(dat[,3:ncol(dat)], 2, convert.numeric)
    }
    colnames(dat) <- var.name
    dat$date <- convert.numeric(dat$date)
    ret <- dat
      # csv로 쓰는 것을 설정할 경우
    if (write.as.csv) { write.csv( dat, file=paste(row.player$name,".csv"), row.names=F)  }
  } else {
    ret <- NA  # 1군 데이터가 없는 선수
  }
  return( ret )
}

## 결과값이 없는 경우 "---" 은 NA로 변환되는 Warning message 뜨지만 output은 괜찮다.

x <- crawl.kbo(player_id[2,]) # 투수예제
y <- crawl.kbo(player_id[53,]) # 타자 예제
