
source("data_sourcing.R")

## real win rates
real <- rate_func( games = games2015, rate = "real" )
## power index variation, generating pythagorean rate matrices 
pow <- seq( from = 0.1, to = 3, by = 0.1)
pyths <- paste( "pyth", pow, sep="")
for ( i in 1:length(pyths) ) {
  assign( pyths[i], rate_func( games2015, rate = "pyth", power = pow[i] ))
}

## plot examples
rate_plot(pyth0.5) + ylim(c(0.2,0.8))
comp_plot( "KIA", real, pyth2, since = "2015-07-01" )

## find optimal indices for each team, given beginning point 
indices <- pyth_index( since = "2015-07-01", games = games2015 )

## KIA 
comp_plot( "KIA", real, pyth2 + 0.025, since = "2015-07-01")
## kt
comp_plot( "kt", real, pyth2-0.005, since = "2015-07-01")
## LG
comp_plot( "LG", real, pyth2, since = "2015-07-01")
## NC
comp_plot( "NC", real, pyth2.7 - 0.06, since = "2015-07-01")
## SK
comp_plot( "SK", real, pyth2 + 0.005, since = "2015-07-01")
## 넥센
comp_plot( "넥센", real, pyth2 - 0.045, since = "2015-07-01")
## 두산
comp_plot( "두산", real, pyth1.2 + 0.04, since = "2015-07-01")
## 롯데
comp_plot( "롯데", real, pyth1.9 - 0.015, since = "2015-07-01")
## 삼성
comp_plot( "삼성", real, pyth2.1 - 0.02, since = "2015-07-01")
## 한화
comp_plot( "한화", real, pyth2.6 + 0.055, since = "2015-07-01")
