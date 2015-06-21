## This script contains 2 functions. 
##  1) makeCacheMatrix, and
##  2) cacheSolve


## This creates a copy of the matrix passed in, 
## and appends 4 attributes to it to store cached data about the matrix 
## (i.e. setmatx, getmatx, setinv, and getinv).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatx <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatx <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(setmatx = setmatx, 
       getmatx = getmatx,
       setinv = setinv,
       getinv = getinv)  
}


## This function accesses the cached data in a matrix 
## to return the inversion result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inversed data")
    return(m)
  }
  data <- x$getmatx()
  m <- solve(data, ...)
  x$setinv(m)
  m  
}
