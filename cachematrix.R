## These two functions calculate and cache the inverse of a matrix.

## This first function creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  imat <- NULL
  set <- function(y) {
    x <<- y
    imat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) imat <<- inverse
  getinverse <- function() imat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This second function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  imat <- x$getinverse()
  if(!is.null(imat)) {
    message("getting cached data")
    return(imat)
  }
  data <- x$get()
  imat <- solve(data, ...)
  x$setinverse(imat)
  imat
}
