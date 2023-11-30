## A set of functions to calculate the inverse of a matrix and
## cache the result, then return the cached inverse if the 
## function is called and the inverse is already cached.
## Done to avoid unnecessarily calculating the inverse twice.

## This function creates a list of functions that can store the
## inputted matrix, as well as it's inverse if that has
## been calculated.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }



## This function uses the list created in the previous
## step to determine if the inverse of the matrix has
## already been computed, and computes it if not. Then it returns
## the inverse.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
