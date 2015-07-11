## This file contains a pair of functions that can be used to 
## calculate the inverse of a matrix, caching the result so
## that the inverse will not be recalculated if it is
## requested again

## this function returns a closure consisting of
## 2 data elements (matrices) and accessor functions
## set and get operate on the first matrix - x
## set will additionally clear "inverse" when it is called
## setinverse and getinverse operate on the second matrix ("inverse")

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  set <- function(newX) {
    x <<- newX
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(newInverse) inverse <<- newInverse
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function calculates the inverse if necessary
## and handles the caching using the closure above
## when the inverse is calculated, it is cached in the closure
## if inverse is requested again, the cached value from the
## closure will be returned

cacheSolve <- function(x, ...) {
  ## return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null((inverse))) {
    message("returning cached data")
    return(inverse)
  }
  
  message("calculating inverse")
  toInvert <- x$get()
  inverse <- solve(toInvert, ...)

  ## cache here
  x$setinverse(inverse)
  inverse
}
