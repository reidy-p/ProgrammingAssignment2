## The goal of this assignment is to create a pair of functtions that can 
## cache the inverse of a matrix. This is useful because calculating the
## inverse of a matrix can be computationally intensive.

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes as its input the special matrix object created by
## makeCacheMatrix. If the inverse of the matrix has already been calculated
## then this function gets the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
