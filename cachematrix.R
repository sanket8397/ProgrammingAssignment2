## Programming Assignment 2: Lexical Scoping

## makeCacheMatrix function creates a special "matrix" object that can cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y){
    x <<- y
    matrixInverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matrixInverse <<- inverse
  getinverse <- function() matrixInverse
  list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixInverse <- x$getinverse()
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  data <- x$get()
  matrixInverse <- solve(data, ...)
  x$setinverse(matrixInverse)
  matrixInverse
}
