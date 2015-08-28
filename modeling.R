#### 1. Pythagorean expectation

## function to sum scores
.score <- function( score, left.sum = T ) {
  v <- as.character( score )
  x <- unlist( strsplit(v, split=":") )
  # away일때 Run Scored, home일 때 Run Allowed
  l.sum <- sum( as.numeric( x[1:length(x) %% 2 != 0] ) )
  # away일때 Run Allowed, home일 때 Run Scored
  r.sum <- sum( as.numeric( x[1:length(x) %% 2 == 0] ) )
  if ( left.sum ) return (l.sum)
  else return(r.sum)
}
## calculating pythagorean expectation of each team
pyth.exp <- function ( team ) {
  a.set <- subset( gameset, away==team ) ; h.set <- subset( gameset, home==team )
  # Run Scored & Run Allowed at away
  a.rs <- .score( a.set$score, left.sum=T )  ; a.ra <- .score( a.set$score, left.sum=F )
  # Run Scored & Run Allowed at home
  h.rs <- .score( h.set$score, left.sum=F )  ; h.ra <- .score( h.set$score, left.sum=T )
  # Aggregation
  ra <- a.ra + h.ra ; rs <- a.rs + h.rs
  # Pythagorean formula
  win <- rs^2 / (rs^2 + ra^2)
  return( win )
}

# result of pythagorean exp.
sort( sapply(levels(gameset$away), pyth.exp, USE.NAMES = T), decreasing = T )

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
