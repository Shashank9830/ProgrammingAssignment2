## Functions to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y){
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setInv <- function(solveMatrix) invr <<- solveMatrix
  getInv <- function() invr
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the matrix.

cacheSolve <- function(x, ...) {
  invr <- x$getInv()

  # return cache if matrix hasn't changed
  if(!is.null(invr)){
    message("getting cached data")
    return(invr)
  }
  # calculate new inveres
  data <- x$get()
  invr <- solve(data)
  x$setInv(invr)
  invr
}
