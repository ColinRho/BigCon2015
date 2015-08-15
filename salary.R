
crawl.salary <- function (row.player) {  
  id <- row.player$id ; pos <- row.player$pos
  if (pos == "p") { 
    u <- "http://www.koreabaseball.com/Record/Player/PitcherDetail/Daily.aspx?playerId="
  } else {
    u <- "http://www.koreabaseball.com/Record/Player/HitterDetail/Daily.aspx?playerId="
  }
  url <- paste(u, id, "", sep="")
  b <- readHTMLList(url)[[17]][8]
  b <- substr(b, start=5, nchar(b) )
  num <- as.numeric(substr(b, 1, nchar(b)-2))
  curr <- substr(b, nchar(b)-1, nchar(b))
  if ( curr == "달러" ) { num <- num * 1100 / 10000}
  return(num)
}
salary <- c()

crawl.salary(player_id)
for ( i in 506:nrow(player_id) ) {
  salary[i] <- crawl.salary(player_id[i,])
}

player_id_mod <- data.frame(player_id, salary)
write.csv(player_id_mod, "player_id_mod.csv")


