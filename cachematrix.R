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
 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function returns the inverse of the matrix by first checking if the inverse is present in the memory or not. If inverse is present then the result is obtained and skips the
## computation. If it is not the case then the inverse is computed and the setinverse function sets the value in cache.


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}