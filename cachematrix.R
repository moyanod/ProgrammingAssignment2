## Functions that allow the creation of special "matrix" objects
## and the calculation of the inverese of these matrix objects.
## The calculation of the inverse matrix is cached; that is, the 
## inverse matrix is actually just computed once although the function
## to get the inverse matrix is called several times.
## If the matrix value changes, the inverse matrix will be recomputed
## next time the function to get the inverse matrix is called.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      x_inv <- NULL
      set <- function(y) {
            x <<- y
            x_inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) x_inv <<- inv
      getinverse <- function() x_inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached inverse matrix")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
