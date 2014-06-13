## These two functions are used to cache the inverse of a matrix

## makeCacheMatrix is used to create a special "matrix", which
## includes a matrix and a list of functions.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve is used to read the inverse of a special matrix in the cache
## or calculate its inverse if there is no information of the "inverse"

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i       ## Return a matrix that is the inverse of 'x'
}
