#### 1. Pythagorean expectation

library(ggplot2)
library(reshape2)

####### functions ############################################################

## function to transform "score" into numeric vector with same order
.score <- function( score ) {
  cha <- as.character( score )
  x <- unlist( strsplit(cha, split=":") )
  return( as.numeric(x) )
}
## function to construct martirx of runs scored and runs allowed for each team from gameset$data_sourcing.R
runs_by_team <- function ( team ) {
  # subsetting by team name
  set <- subset( gameset, away==team | home==team )
  # arbitrary array
  runs <- array(NA, dim = c(nrow(set),2))
  # loop to split runs scored and allowed by the team played as away or home
  for ( i in 1:nrow(set) ) {
    s <- .score( set[i,]$score )
    if ( set[i,]$away == team ) runs[i,] <- s
    else runs[i,] <- rev( s )
  }
  # combining and naming columns
  mat <- data.frame(set$date, runs) ; colnames(mat) <- c("date","run_scored","run_allowed") ;
  return( mat )
}
## calculating real win percentage from dat1$data_sourcing.R
real.wpct <- function( x ) { # x should be team name
  set <- subset(dat1, team == x)
  draw <- which(is.na(set$win))
  cum <- cumsum( subset(set, !is.na(win))$win )
  if ( length(draw) != 0 ) {
    for ( i in 1:length(draw) ) {
      cum <- append(cum, NA, after = draw[i]-1)
    }
    v <- cum/1:length(cum)
    # fill out NA due to draw by a value of last element
    v[draw] <- v[draw-1]
  }
  # no draws
  else v <- cum/1:length(cum)
  return(v)
}
## simple calculation function for pythagorean wpct
pythagorean <- function (x, y) {
  return(x^2 / (x^2 + y^2))
}
## calculating pythagorean expectation of each team
pyth.exp <- function ( team ) {
  set <- runs_list[[team]]
  cum.scored <- cumsum(set$run_scored) ; cum.allowed <- cumsum(set$run_allowed)
  exp <- pythagorean( cum.scored, cum.allowed )
  return( exp )
}

##############################################################################
# vector of team names excluding all-star game
teams <- levels(gameset$away)[-11]
# actual win rate in time series
real <- sapply(teams, real.wpct)
# save runs data of each team as a list 
runs_list <- lapply( teams, runs_by_team ) ; names(runs_list) <- teams

# check the correlation bewteem runs scored and allowed of each team
cor_list <- list()
for ( i in 1:10 ) {
  cor_list[[i]] <- cor( runs_list[[i]][,2:3] )
}
names(cor_list) <- teams
cor_list

# pythagorean win rates in time series for each team
pyth <- sapply(teams, pyth.exp)

## function used after pyth.exp or real.wpct, to be used before melt and ggplot 
rates_by_date <- function ( x ) { # runs_list, teams needed. x should be an object like pyth or win_pcts
  # arbitrary list and fill it with (date, rate, team) vector
  .list <- list()
  for ( i in 1:10 ) {
    .list[[ teams[i] ]] <- data.frame(date = runs_list[[ teams[i] ]]$date, rate = x[[ teams[i] ]], 
                                          team = rep( teams[i], length.out = nrow(runs_list[[ teams[i] ]])))
  }
  sum_mat <- myrbind(.list)
  # every dates at least one game held
  dates <- sort( unique(sum_mat$date) )
  # target object 
  res <- data.frame( date = dates )
  # column adding loop
  while ( ncol(res) < 11 ) {
    p <- ncol(res) 
    res <- cbind(res, NA)
    set <- subset(sum_mat, team == teams[p] )
    for ( i in 1:nrow(res) ) {
      if ( res$date[i] %in% set$date ) {
        res[i, ncol(res)] <- subset(set, date == res$date[i])$rate
      }
      else res[i, ncol(res)] <- res[i-1, ncol(res)]
    }
  }
  colnames(res) <- c("date", teams)
  return(res)
}

pyth2 <- rates_by_date( pyth )
real2 <- rates_by_date( real )

## draw time series plot by ggplot
rate_plot <- function ( x, since = NULL ) {
  if ( !is.null(since) ) x <- subset(x, date >= since)
  # melted data set
  melted <- melt(x, id.vars = "date")
  # temporary ggplot form
  p.tmp <- ggplot(melted, aes(x=date, y=value, group=variable)) 
  p.tmp + geom_line(aes(colour=variable)) + xlab("Rate") + ylab("Date")
}
# examples
rate_plot(pyth2)
rate_plot(real2)
rate_plot(pyth2, since = "2015-05-01")
rate_plot(real2, since = "2015-05-01")

## comparing real winning rates and pyth. expectation rates-
comp_plot <- function ( x, real, pyth, since = NULL ) {
  d <- data.frame(date = real$date, real = real[[x]], pyth = pyth[[x]])
  if ( !is.null(since) ) d <- subset(d, date >= since)
  melted <- melt(d, id.vars = "date")
  p.tmp <- ggplot(melted, aes(x=date, y=value, group=variable)) 
  p.tmp + geom_line(aes(colour=variable)) + xlab("Rate") + ylab("Date")
}

comp_plot( "LG", real2, pyth2 )

### https://en.wikipedia.org/wiki/Pythagorean_expectation

library(MASS) # for LDA, QDA, Logistic Reg
library(nnet) # for Neural Network
library(e1071) # for SVM
library(rpart) # for DT

## LDA
## QDA
## Logistic Regression
## Decision Tree(CART or ...)
## Neural Network
## SVM
## Random Forest
