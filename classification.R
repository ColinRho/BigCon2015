########## Classification Approach  ##############################################################
##################################################################################################
## CAUTION: It takes huge loading time because of scraping every single games in the gamelist.
## stable network environment recommended, alternatively do it by monthly

lineupSep <- lineup.total(gamelist, by.month="09")
lineup0$date <- as.Date(lineup0$date)
lineup <- myrbind( list(lineup0, lineupSep) )
gameset <- games2015[-420,] # exclude all-star games 

## temporary weight
w <- 0.1  
## 'gameset' and 'lineup' data should meet same period in 'date' variable
## primitive data
dat1 <- aggr.stat(gamest, w)
## divided data
dat2 <- aggr.stat(gameset, w, T)
## without vs_rate, ( first series in season ) take 50%
dat1$vs_rate[which( is.nan(dat1$vs_rate) | is.na(dat1$vs_rate) )] <- 0.5
dat2$vs_rate[which( is.nan(dat2$vs_rate) )] <- 0.5
## binary data
dat3 <- dat2
## for each column take 1 or 0 by larger or smaller than 1
temp <- function ( column ) {
  column[column < 1] <- 0
  column[column >= 1] <- 1
  return(column)
}
for ( i in 3:35 ) {
  dat3[,i] <- temp(dat3[,i])
}
dat3$streak[dat3$streak >= 0 ] <- 1 ; dat3$streak[dat3$streak < 0 ] <- 0
dat3$vs_rate[dat3$vs_rate >= 0.5 ] <- 1 ; dat3$vs_rate[dat3$vs_rate < 0.5 ] <- 0
