#### 1. Pythagorean expectation

library(ggplot2)
library(reshape2)

####### functions ######################################################################

#### 1. win rate calculation functions
## function to transform "score" into numeric vector with same order
.score <- function( score ) {
  cha <- as.character( score )
  x <- unlist( strsplit(cha, split=":") )
  return( as.numeric(x) )
}
## function to construct martirx of runs scored and runs allowed for each team from gameset$data_sourcing.R
runs_by_team <- function ( team, games ) {
  # subsetting by team name
  set <- subset( games, away==team | home==team )
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
real.wpct <- function( team, runs ) { # x should be team name
  set <- runs[[team]]
  # making 'win' variable
  for ( i in 1:nrow(set) ) {
    if ( set$run_scored[i] > set$run_allowed[i] ) { set$win[i] <- 1 }
    else if ( set$run_scored[i] < set$run_allowed[i] ) { set$win[i] <- 0 }
    else if ( set$run_scored[i] == set$run_allowed[i] )set$win[i] <- NA
  }
  draw <- which(is.na(set$win))
  cum <- cumsum( subset(set, !is.na(win))$win )
  if ( length(draw) != 0 ) {
    for ( i in 1:length(draw) ) {
      cum <- append(cum, NA, after = draw[i]-1)
    }
    v <- cum/1:length(cum)
    # fill out NA due to draw by a value of last element, considering consecutive draws
    for ( i in 1:length(draw) ) {
      v[ draw[i] ] <- v[ draw[i]-1 ]
    }
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
pyth.exp <- function ( team, runs ) {
  set <- runs[[team]]
  cum.scored <- cumsum(set$run_scored) ; cum.allowed <- cumsum(set$run_allowed)
  exp <- pythagorean( cum.scored, cum.allowed )
  return( exp )
}
## function used after pyth.exp or real.wpct, to be used before melt and ggplot 
rates_by_date <- function ( x, runs, teams ) { # runs_list, teams needed. x should be an object like pyth or win_pcts
  # arbitrary list and fill it with (date, rate, team) vector
  .list <- list()
  for ( i in 1:length(teams) ) {
    .list[[ teams[i] ]] <- data.frame(date = runs[[ teams[i] ]]$date, rate = x[[ teams[i] ]], 
                                      team = rep( teams[i], length.out = nrow(runs[[ teams[i] ]])))
  }
  sum_mat <- myrbind(.list)
  # every dates at least one game held
  dates <- sort( unique(sum_mat$date) )
  # target object 
  res <- data.frame( date = dates )
  # column adding loop
  while ( ncol(res) < length(teams) + 1 ) {
    p <- ncol(res) 
    res <- cbind(res, NA)
    set <- subset(sum_mat, team == teams[p] )
    for ( i in 1:nrow(res) ) {
      if ( res$date[i] %in% set$date ) {
        res[i, ncol(res)] <- subset(set, date == res$date[i])$rate[1] # considering double header games, no differences in rates
                                                                      # already considered in previus function
      }
      # first day, no game
      else if ( i == 1 ) res[i, ncol(res)] <- 0
      else res[i, ncol(res)] <- res[i-1, ncol(res)]
    }
  }
  colnames(res) <- c("date", teams)
  return(res)
}
## aggregated function for win rate calculation
rate_func <- function( games, rate = "real" ) {
  # vector of team names excluding all-star game
  teams <- names( which( table(games$away) !=1 ) )
  # save runs data of each team as a list 
  runs_list <- lapply( teams, runs_by_team, games = games ) ; names(runs_list) <- teams
  # actual win rate in time series
  if ( rate == "real" ) rates <- sapply(teams, real.wpct, runs = runs_list)
  # pythagorean win rates in time series for each team
  else if ( rate == "pyth" )  rates <- sapply(teams, pyth.exp, runs = runs_list)
  rbd <- rates_by_date( x = rates, runs = runs_list, teams = teams )
  return(rbd)
}

#### 2. plotting functions 
## draw time series plot by ggplot
rate_plot <- function ( x, since = NULL ) {
  if ( !is.null(since) ) x <- subset(x, date >= since)
  # melted data set
  melted <- melt(x, id.vars = "date")
  # temporary ggplot form
  p.tmp <- ggplot(melted, aes(x=date, y=value, group=variable)) 
  p.tmp + geom_line(aes(colour=variable)) + xlab("Date") + ylab("Rate")
}
## comparing real winning rates and pyth. expectation rates-
comp_plot <- function ( x, real, pyth, since = NULL ) {
  d <- data.frame(date = real$date, real = real[[x]], pyth = pyth[[x]])
  if ( !is.null(since) ) d <- subset(d, date >= since)
  from <- min(d$date) ; to <- max(d$date)
  melted <- melt(d, id.vars = "date")
  p.tmp <- ggplot(melted, aes(x=date, y=value, group=variable)) 
  p.tmp + geom_line(aes(colour=variable)) + xlab("Rate") + ylab("Date") + xlim(from,to)
}

########################################################################################

####### gamelists #######################################################################

month <- c("03","04","05","06","07","08","09")
gamelist2015 <- gamelist.total( month, year="2015" ) ; games2015 <- subset ( gamelist2015, !is.na(score) )
gamelist2014 <- gamelist.total( month, year="2014" ) ; games2014 <- subset ( gamelist2014, !is.na(score) )
gamelist2013 <- gamelist.total( month, year="2013" ) ; games2013 <- subset ( gamelist2013, !is.na(score) )
gamelist2012 <- gamelist.total( month, year="2012" ) ; games2012 <- subset ( gamelist2012, !is.na(score) )
gamelist2011 <- gamelist.total( month, year="2011" ) ; games2011 <- subset ( gamelist2011, !is.na(score) )
gamelist2010 <- gamelist.total( month, year="2010" ) ; games2010 <- subset ( gamelist2010, !is.na(score) )

########################################################################################

rate2015 <- rate_func( games = games2015, rate = "real" )
rate2014 <- rate_func( games = games2014, rate = "real" )
rate2013 <- rate_func( games = games2013, rate = "real" )
rate2012 <- rate_func( games = games2012, rate = "real" )
rate2011 <- rate_func( games = games2011, rate = "real" )
rate2010 <- rate_func( games = games2010, rate = "real" )

lm(data = rate2014, formula = KIA ~ date)

# examples
p2014 <- rate_plot(rate2014, since = "2014-09-06")
rate_plot(rate2013, since = "2013-09-06") + stat_smooth(method = 'lm')
rate_plot(rate2012, since = "2012-09-06")


comp_plot( "NC", real2, pyth2, since = "2015-04-30" )





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


