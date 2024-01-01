## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mxt <- NULL
  set <- function(y) {
    x <<- y
    mxt <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) mxt <<- solve
  getInverse <- function() mxt
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##  This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If the inverse has already been calculated (and the 
##  matrix has not changed), then the cachesolve should retrieve the inverse 
##  from the cache.

cacheSolve <- function(x, ...) {
  mtx <- x$getInverse()
  if(!is.null(mxt)){
    message("getting cached data")
    return(mxt)
  }
  data <- x$get()
  mxt <- solve(data, ...)
  x$setInverse(mxt)
  mxt
  ## Return a matrix that is the inverse of 'x'
}
