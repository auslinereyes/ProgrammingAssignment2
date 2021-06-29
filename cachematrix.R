## Two functions that cache the inverse of a matrix
## this contains the two functions of makeCacheMatrix, cacheSOlve

## functions of makeCacheMatrix
## contains set, get, setInv, getInv

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Function of cacheSolve
## This function is used to get the cache data

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message ("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv ## Return a matrix that is inverse of "x"
}
