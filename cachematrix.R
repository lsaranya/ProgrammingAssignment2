## Caching helps to save valuable resources of a system and 
## it might be advantageous to cache the response i.e. inverse
##We have two functions described below and they can help in caching

## makeCacheMatrix makes a list containing a function in order to:
##  1.Set the:
##     1.1 value of the matrix
##     1.2 value of inverse of the matrix
##
##  2. Get the:
##     2.1 value of the matrix
##     2.2 value of inverse of the matrix
 

makeCacheMatrix <- function(p = matrix()) {
  v1 <- NULL
  equate <- function(y) {
    p <<- y
    v1 <<- NULL
  }
  bring <- function() p
  equateinverse <- function(invert) v1 <<- invert
  bringinverse <- function() v1
  list(equate = equate,
       bring = bring,
       equateinverse = equateinverse,
       bringinverse = bringinverse)
}

## this function returns the inverse of the matrix by first checking if the inverse is present in the memory or not. If inverse is present then the result is obtained and skips the
## computation. If it is not the case then the inverse is computed and the setinverse function sets the value in cache.


cacheSolve <- function(p, ...) {
  v1 <- p$bringinverse()
  if (!is.null(v1)) {
    message("cached")
    return(v1)
  }
  data <- p$bring()
  v1 <- solve(data, ...)
  p$equateinverse(v1)
  v1
}

