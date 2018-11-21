## Caching the Inverse of a Matrix using a special object (list)

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = numeric()) {
  r <- NULL
  set <- function(y) {
    x <<- y
    r <<- NULL
  }
  get <- function() x
  setrev <- function(reverse) r <<- reverse
  getrev <- function() r
  list(set = set, get = get,
       setrev = setrev,
       getrev = getrev)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  r <- x$getrev()
  if(!is.null(r)) {
    message("getting cached data")
    return(r)
  }
  data <- x$get()
  r <- solve(data, ...)
  x$setrev(r)
  r
}
