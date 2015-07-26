## Cache a matrix inversion to save time

## Special matrix object that is cached

makeCacheMatrix <- function(x = matrix()) {
  ## Cache the inverse at i
  i <- NULL
  set <- function( y ) {
    ## If we set the actual object invalidate the cache
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list( set = set, get = get, setinverse = setinverse,
        getinverse = getinverse)
}


## Invert the matrix from above if needed, use cache if available

cacheSolve <- function(x, ...) {
  ## Check to see if the inverse has been calculated and cached
  i <- x$getinverse()
  if ( !is.null(i) ) {
    ## If it has return it
    message("getting cached data")
    return( i )
  }
  ## if not, calculate the inverse and have it cached
  data <- x$get()
  i <- solve( data, ... )
  x$setinverse( i )
  i
}
