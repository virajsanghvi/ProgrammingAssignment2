## This collection of functions creates a CacheMatrix
## that has cacheable operations. Currently, caching the result
## of 'solve' is supported.

## makeCacheMatrix creates a list containing methods that 
## give you the ability to generalize writing cache-enabled
## methods.
##
## Input: 
##   x: a matrix (optional)
## Output:
##   list(
##     set(y): set the current matrix
##     get(): returns the current matrix
##     setsolve(solve): set the solve value for the current matrix
##     getsolve(): get the solve value for the current matrix
##  )
makeCacheMatrix <- function(x = matrix()) {
  # initialize value of solve to NULL
  s <- NULL
  
  set <- function(y) {
    # on setting a new matrix, reset value of solve
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  
  setsolve <- function(solve) s <<- solve
  
  getsolve <- function() s
  
  # return list of methods
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve calculates the solve value for a CacheMatrix
## Input:
##   x: a CacheMatrix
## Output:
##   matrix: value of solve(x)
cacheSolve <- function(x, ...) {
  # attempt to get current value of solve if it exists
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  # otherwise, calculate value and set it
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  
  # return it
  s
}
