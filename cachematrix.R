## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatly.
## The functions below will cache the inverse of a matrix

## This function creates a special matrix object that can cache
## it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv1 <- NULL
  set <- function (y)  {
    x <<- y
    inv1 <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv1 <<- inverse
  getinverse <- function() inv1
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse )

}


## This function computes the inverse of the special matrix returned
## by makeCacheMatrix above. If the inverse has already been calculated
## and the matrix has not changed, then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv1 <- x$getinverse()
  if(!is.null(inv1))     {
    message("getting cached data")
    return(inv1)
  }
  data <- x$get()
  inv1 <- solve(data)
  x$setinverse(inv1)
  
  ## Return the result
  inv1
}
