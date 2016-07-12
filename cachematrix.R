## Creates a special Matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
   cachedInverse <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setInverse <- function(inverse) cachedInverse <<- inverse
   getInverse <- function() cachedInverse
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Return the inverse of a Cache Matrix object
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      invFctn <- x$getInverse()
      if(!is.null(invFctn)) {
         message("getting cached data")
         return(invFctn)
      }
      data <- x$get()
      invFctn <- solve(data, ...)
      x$setInverse(invFctn)
      invFctn
   }
