## package "ggplot2", "reshape2", "cvTools", "randomForest" required  
########## 1. Pythagorean Expectation and Time Series Analysis ###################################
#### 1) win rate calculation functions

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
## find the indices for each team which make mse b/w real rates and pyth rates
pyth_index <- function ( since , games = games2015) {
  # possible power index range
  pow <- seq(from=0.1, to=3, by=0.1)
  # mse b/w real rate and pyth rate
  mses <- lapply( pow, pyth_real_mse, games = games, since = since)
  find_index <- do.call(rbind, mses) ; row.names(find_index) <- pow
  indices <- c()
  # which minimize mse
  for ( i in 1:10 ) {
    indices[i] <- names( which( find_index[,i] == min(find_index[,i]) ) )
  } 
  names(indices) <- colnames(find_index)
  indices
}

##################################################################################################
#### 2) plotting functions 

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
comp_plot <- function ( team, real, pyth, since = NULL ) {
  d <- data.frame(date = real$date, real = real[[team]], pyth = pyth[[team]])
  if ( !is.null(since) ) d <- subset(d, date >= since)
  # mse value added
  print( sum ( (d$real - d$pyth)^2 ) )
  from <- min(d$date) ; to <- max(d$date)
  melted <- melt(d, id.vars = "date")
  # ggplot
  p.tmp <- ggplot(melted, aes(x=date, y=value, group=variable)) 
  p.tmp <- p.tmp + geom_line(aes(colour=variable)) + xlab("Rate") + ylab("Date") + xlim(from,to) 
  p.tmp
}
## Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
# ### forked
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##################################################################################################
########## 2. for regression approach  ###########################################################

## producing since vector
get.since <- function( term1 ) {
  min.since <- opening[ format(opening, "%Y") == "2015"] - 1
  max.since <- Sys.Date() - 22 - term1
  since <- min.since + 1:as.numeric(max.since - min.since)
  return(since)
}
## function calling data of player during given period(since, until)
get.period <- function( player, since, until, trade.in = NULL, trade.out = NULL ) {
  if ( player %in% trade.in ) { since <- subset(trade_2015, name == player)$date }
  else if ( player %in% trade.out ) { until <- subset(trade_2015, name == player)$date }
  # call data set of player
  dat <- get(player, envir = .GlobalEnv)
  dat <- subset(dat, date >= since & date <= until)
  dat <- dat[ sapply(dat, is.numeric) ]
  # if no played game in given period
  if ( nrow(dat) == 0 ) { 
    v <- rep(NA, length.out = ncol(dat)) ; names(v) <- colnames(dat)  
  }
  else v <- colSums( dat, na.rm = T)
  return(v)
}
## select or produce variables considered important
selectvar <- function( x, pos = "p" ) { # x should be object produced by get.period()
  # delete total AVG and ERA
  x <- x[-length(x)]
  # additional stats for pitchers
  if ( pos == "p" ) {
    WHIP <- (x$HA + x$BBA + x$HBPA)/x$IP 
    LOBPER <- (x$HA + x$BBA + x$HBPA - x$RA)/(x$HA + x$BBA + x$HBPA -(1.4*x$HRA)) 
    v <- data.frame( x, WHIP, LOBPER )
  }
  # additional stats for hitters
  else {
    SLG <- (x$H + 2*x$X2B + 3*x$X3B + 4*x$HR)/x$AB 
    OBP <- (x$H + x$BB + x$HBP)/(x$AB + x$BB + x$HBP) 
    SBPER <- x$SB/(x$SB + x$CS)
    v <- data.frame( x, SLG, OBP, SBPER )
  }
  return(v)
}
## comprehensive function to get teams' stat
team.period.stat <- function ( team , pos, since, term ) {
  since <- as.Date(since) ; until <- since + term
  # for pitchers
  if ( pos == "p" ) { players <- subset( player_id[ player_id$team == team ,], pos == "p")$name }
  # for hitters
  else { players <- subset( player_id[ player_id$team == team ,], pos != "p")$name }
  # consider trade during season
  trade.in <- subset( trade_2015, name %in% players & date < until )
  if ( pos == "p" ) { trade.out <- subset( trade_2015, from == team & pos == "p" & date > since ) }
  else { trade.out <- subset( trade_2015, from == team & pos != "p" & date > since ) }
  # including players who traded out during season
  players <- c(as.character(players), as.character(trade.out$name))
  players <- players[ players %in% ls(envir = .GlobalEnv) ]
  # get stats
  x1 <- sapply( players, get.period, since = since, until = until, trade.in = trade.in, trade.out = trade.out )
  x2 <- as.data.frame( t( rowSums(x1, na.rm = T) ) )
  y <- selectvar(x2, pos = pos)
  return(y)
}
## setting x and y variables to conduct regression analysis
settingxy <- function ( team, pos, since, games, term1, term2  ) {
  # term1 represents term to cumulate x variable( team stat ), 
  # term2 represnets term to cumulate y variable( team runs )
  # x should be prior to y in time manner
  # time values
  x.since <- as.Date(since) ; x.cum.term <- x.since + (term1 - 1)
  y.since <- x.cum.term + 1 ; y.cum.term <- y.since + (term2 - 1)
  # y variable, runs
  runs <- runs_by_team( team, games = games )
  y <- subset( runs, date >= y.since & date <= y.cum.term )
  # for pitchers, y should be run allowed
  if ( pos == "p" ) { y <- y$run_allowed }
  # on the other hand, run scored for hitters
  else { y <- y$run_scored }
  y <- sum(y)
  # x variable, stats
  x <- team.period.stat( team = team, pos = pos, since = x.since, term = term1 )
  res <- cbind(x, y)
  return(as.data.frame(res))
}
## data generating
mat_func <- function( team, pos, since, games, term1 = 20, term2 = 23) {
  mat <- lapply( since, settingxy, team = team,  pos = pos, games = games, term1 = term1, term2 = term2 )
  mat <- myrbind(mat)
  return(mat)
}

##################################################################################################

