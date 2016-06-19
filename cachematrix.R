## calculating the inverse of a matrix is potentially time consuming. This series of functions will allow
## the inverse of a matrix to be cached, thus reducing computational load for large objects.

## makeCacheMatrix() creates a list of functions that helps to cache the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv   
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes a list of functions relating to a matirx, checks whether the inverse has been cached,
## before computing the inverse and storing it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
