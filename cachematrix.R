## Two functions, to compute the inverse of a matrix. The result will be cached, to save time, 
## if the inversion of the same matrix is needed again.

## Creates an 'object' (correct: a list) that holds...
##    TWO matrices:   the original (x) and the inverted matrix (m)
##    FOUR functions: 
##       set: to change the value of the original matrix x and to reset m
##       get: returns the value of x
##       setInverse: receives a value and saves it to m (could be anything! 
##                   Doesn't check, whether inverse of x or not)
##       getInverse: returns value of m

makeCacheMatrix <- function(x = matrix()) {
  ## x: original value
  ## m: cache ("memory") for a value depending on the value of x -> erasing m whenever x changes
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(valForMem) m <<- valForMem
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Handles lists of makeCacheMatrix
## Computes the inverse of a matrix or reads it directly from the cache if existent

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Checks if m (in list of makeCacheMatrix) is not empty
  if(!is.null(m)) {
    message("getting cached data")
    ## m was not empty - return its value and "quit" the function
    return(m)
  }
  
  ##otherwise: compute inverse of x
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
