#### 1. Pythagorean Expectation and Time Series Analysis

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
  # divide cases with one team, or all team
  if ( !is.data.frame(runs) ) set <- runs[[team]]
  else set <- runs
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
pythagorean <- function (x, y, power = 2) {
  return(x^power / (x^power + y^power))
}
## calculating pythagorean expectation of each team
pyth.exp <- function ( team, runs, power = 2 ) {
  # divide cases with one team, or all team
  if ( !is.data.frame(runs) ) { set <- runs[[team]] }
  else { set <- runs }
  cum.scored <- cumsum(set$run_scored) ; cum.allowed <- cumsum(set$run_allowed)
  exp <- pythagorean( cum.scored, cum.allowed, power )
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
rate_func <- function( games, rate = "real", power = 2 ) {
  # vector of team names excluding all-star game
  teams <- names( which( table(games$away) !=1 ) )
  # save runs data of each team as a list 
  runs_list <- lapply( teams, runs_by_team, games = games ) ; names(runs_list) <- teams
  # actual win rate in time series
  if ( rate == "real" ) rates <- sapply(teams, real.wpct, runs = runs_list)
  # pythagorean win rates in time series for each team
  else if ( rate == "pyth" )  rates <- sapply(teams, pyth.exp, runs = runs_list, power)
  rbd <- rates_by_date( x = rates, runs = runs_list, teams = teams )
  return(rbd)
}
## simple coefficient calculating function 
reg.coef <- function( data, team ) {
  num <- which( colnames(data) == team )
  fit <- lm(data = data, formula = data[,num] ~ date)
  res <- fit$coef[2] ; names(res) <- team
  return( res )
}
## for each team, return coefs, ranks
coef_by_ranks <- function ( data, since ) {
  teams <- names(data)[ sapply( data, is.numeric) ]
  since <- as.Date( paste(format(data$date, "%Y"), since, sep="-") )
  set <- subset(data, date >= since)
  # fit linear regression on date
  coefs <- sapply( teams, reg.coef, data = set, USE.NAMES = F)
  # add ranks
  start_rank <- length(teams) + 1 - rank( set[1, teams] )
  end_rank <- length(teams) + 1 - rank( set[nrow(set), teams] )
  res <- cbind( coefs, start_rank, end_rank )
  return( res )
}
## calculating mse b/w real win rates and pythagorean rates
pyth_real_mse <- function ( games, since = NULL, power = 2) {
  pyth <- rate_func(games, rate = "pyth", power = power)
  real <- rate_func(games, rate = "real")
  if ( !is.null(since) ) {
    real <- subset( real, date >= since )
    pyth <- subset( pyth, date >= since )
  }
  real <- real[ ,sapply(real, is.numeric) ]
  pyth <- pyth[ ,sapply(pyth, is.numeric) ]
  mse <- colSums( ( real - pyth )^2 )
  return(mse)
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
  # mse value added
  print( sum ( (d$real - d$pyth)^2 ) )
  from <- min(d$date) ; to <- max(d$date)
  melted <- melt(d, id.vars = "date")
  # ggplot
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

## real win rates
rate2015 <- rate_func( games = games2015, rate = "real" )
rate2014 <- rate_func( games = games2014, rate = "real" )
rate2013 <- rate_func( games = games2013, rate = "real" )
rate2012 <- rate_func( games = games2012, rate = "real" )
rate2011 <- rate_func( games = games2011, rate = "real" )
rate2010 <- rate_func( games = games2010, rate = "real" )

## pythagorean win rates
pyth2015 <- rate_func( games = games2015, rate = "real", power = 2) # with typical power index

## regression coefficients for each year, each team
coef2010 <- coef_by_ranks( rate2010, since = "09-06")
coef2011 <- coef_by_ranks( rate2011, since = "09-06")
coef2012 <- coef_by_ranks( rate2012, since = "09-06")
coef2013 <- coef_by_ranks( rate2013, since = "09-06")
coef2014 <- coef_by_ranks( rate2014, since = "08-20")

## plot examples
rate_plot(rate2014, since = "2014-09-06")
rate_plot(rate2013, since = "2013-09-06") + stat_smooth(method = 'lm')
comp_plot( "kt", rate2015, pyth2015 )

## find the indices for each team which make mse b/w real rates and pyth rates
pow <- seq(from=0.1, to=3, by=0.1)
mses <- lapply( pow, pyth_real_mse, games = games2015, since = "2015-06-01")
find_index <- do.call(rbind, mses) ; row.names(find_index) <- pow
indices <- c()
for ( i in 1:10 ) {
  indices[i] <- names( which( find_index[,i] == min(find_index[,i]) ) )
}
names(indices) <- colnames(find_index)
indices

## power index variation
pyth1 <- rate_func( games2015, rate = "pyth", power = 1)
pyth3 <- rate_func( games2015, rate = "pyth", power = 3)
pyth2.3 <- rate_func( games2015, rate = "pyth", power = 2.3)
comp_plot("LG", rate2015, pyth2.3, since = "2015-06-01")

### https://en.wikipedia.org/wiki/Pythagorean_expectation


#### 2. Classification 

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


