##Caching the inverse of a matrix
## Function makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function CacheSolve
## Return a matrix that is the inverse of 'x'

CacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Test functions
x = matrix(1:4,nrow = 2, ncol = 2)
m = makeCacheMatrix(x)
m$get()
CacheSolve(m)

