
source("data_sourcing.R")

########## 1. pythagorean win rate and finding appropriate index for each team ###################

## real win rates
real <- rate_func( games = games2015, rate = "real" )
## power index variation, generating pythagorean rate matrices 
pow <- seq( from = 0.1, to = 3, by = 0.1)
pyths <- paste( "pyth", pow, sep="")
for ( i in 1:length(pyths) ) {
  assign( pyths[i], rate_func( games2015, rate = "pyth", power = pow[i] ))
}

## find optimal indices for each team, given beginning point 
indices <- pyth_index( since = "2015-07-01", games = games2015 )

## plot examples
rate_plot(pyth0.5) + ylim(c(0.2,0.8))
comp_plot( "KIA", real, pyth2 + 0.025, since = "2015-07-01")

## KIA 
p_KIA <- comp_plot( "KIA", real, pyth2 + 0.025, since = "2015-07-01")
## kt
p_kt <- comp_plot( "kt", real, pyth2-0.005, since = "2015-07-01")
## LG
p_LG <- comp_plot( "LG", real, pyth2, since = "2015-07-01")
## NC
p_NC <- comp_plot( "NC", real, pyth2.7 - 0.06, since = "2015-07-01")
## SK
p_SK <- comp_plot( "SK", real, pyth2 + 0.005, since = "2015-07-01")
## 넥센
p_nex <- comp_plot( "넥센", real, pyth2 - 0.045, since = "2015-07-01")
## 두산
p_doo <- comp_plot( "두산", real, pyth1.2 + 0.04, since = "2015-07-01")
## 롯데
p_lot <- comp_plot( "롯데", real, pyth1.9 - 0.015, since = "2015-07-01")
## 삼성
p_sam <- comp_plot( "삼성", real, pyth2.1 - 0.02, since = "2015-07-01")
## 한화
p_han <- comp_plot( "한화", real, pyth2.6 + 0.055, since = "2015-07-01")
## pythagorean fitted coefficients
power.ind <- c( 2, 2, 2, 2.7, 2, 2, 1.2, 1.9, 2.1, 2.6 )
shift <- c( 0.025, -0.005, 0, -0.06, 0.005, -0.045, 0.04, 0.015, -0.02, 0.055 )
pyth.fit <- data.frame( power.ind, shift, row.names = teams)

## current winning rate
current_rate <- real[nrow(real),]


##################################################################################################
########## 2. random forest ######################################################################

## simple mse function
mse <- function(x,y) { mean((x-y)^2) }
set.seed(123)
## finding optiaml term1 minimizing mse, and save random forest fitted object
term1.mse<-function( team, pos = "f", d_day = "2015-09-05", term_vector = 20:60 ) {
  # temporary vector and list to save something
  msee<-c() ; 
  # object name which will contain rf fit object
  if ( pos == "p") { obj <- paste("fit",team,"pit",sep="_") }
  else { obj <- paste("fit",team,"hit",sep="_") }
  # finding optimal term1 loop ( minimizing mse )
  for(i in term_vector[1]:term_vector[length(term_vector)]) {
    temp_term1 <- i
    temp_since <- get.since(temp_term1, d_day)
    temp_mat <- mat_func( team, pos, temp_since, games = games2015, temp_term1, term2 = 23 )
    temp_fit <- randomForest( y~. , data = temp_mat, nodesize=nrow(temp_mat)*0.05, keep.forest=TRUE, ntree=500)
    temp_pred <- predict(temp_fit, temp_mat, type="response")
    msee <- c(msee, mse(temp_pred, temp_mat$y) )
  }
  # the best term1 and its mse
  min.mse <- min(msee)
  term1 <- term_vector[ which( msee == min.mse ) ] 
  # desirable random forest fitting
  since <- get.since(term1)
  mat <- mat_func( team, pos, since, games = games2015, term1 = term1 )
  fit <- randomForest( y ~ . , data = mat, nodesize=nrow(mat)*0.05, keep.forest=TRUE, ntree=500)
  assign( obj, fit, envir = .GlobalEnv)
  # plotting mse
  plot( term_vector, msee, type="l", main = substr(obj, 5, nchar(obj)) )
  v <- data.frame( term1, min.mse ) ; row.names(v) <- team
  print( v )
  # output 
  res <- data.frame( mse = t(msee) , term1 = term1, row.names = team) 
  return( res )
}
## making new since vector for test set
test.x.since <- function( term1, d_day = "2015-09-05" ) { 
  start_day <- max( get.since( term1 ) ) ; end_day <- as.Date(d_day) - term1 + 1
  start_day + 1:as.numeric( end_day - start_day )
}
## testing after model made 
test.result <- function ( team, pos, d_day,term1 ) {
  # get fitted object ( random forest )
  if ( pos == "p" ) { position <- "pit" }
  else { position <- "hit"}
  obj.name <- paste("fit", team, position, sep="_" )
  obj <- get(obj.name, envir = .GlobalEnv)
  # make x range for test set 
  test.since <- test.x.since(term1, d_day = d_day)
  df <- myrbind( lapply( test.since, team.period.stat, team = team, pos = pos, term = term1) )
  fitted.y <- predict( obj, newdata = df)
  return( data.frame( df, fitted.y ) )
}

##################################################################################################

fit_f <- sapply( teams, term1.mse, pos = "f" )
fit_p <- sapply( teams, term1.mse, pos = "p" )



## to use at Rmd 
save.image("work.RData")




