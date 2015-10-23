## Matrix inversion is usually a costly computation and there may be some benefit
## in caching the inverse of a matrix rather than compute it repeatedly. 
## Below are two functions that are used to create a special object that stores 
## a square matrix and cache's its inverse.

## This first function creates a special "matrix" object that can cache its inverse.
## It also contains sub-functions to set and get a matrix and to set and get the
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {                # Function to modify matrix and set inverse to NULL
    x <<- y
    inv <<- NULL
  }
  get <- function()                   # Function to return matrix
    x
  setinverse <- function(solve)       # Function to cache inverse
    inv <<- solve
  getinverse <- function()            # Function to return matrix inverse
    inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This second function computes the inverse of the special "matrix" returned by
#  the makeCacheMatrix function above. If the inverse has already been calculated 
#  (and the matrix has not changed), then the cachesolve will retrieve the inverse
#  from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {                 # Checks inverse if it is not NULL
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

