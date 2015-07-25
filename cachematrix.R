## Cache a matrix inversion to save time

## Special matrix object that is cached

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function( y ) {
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
  i <- x$getinverse()
  if ( !is.null(i) ) {
    message("getting cached data")
    return( i )
  }
  data <- x$get()
  i <- solve( data, ... )
  x$setinverse( i )
  i
}
