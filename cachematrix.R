## My functions will take a matrix (makeCacheMatrix) and create special "matrix"
## capable of caching its inverse, then (cacheSolve) take "matrix" like this and 
## compute its inverse (if it hasn't been done yet) or take the cached value.

## makeCacheMatrix takes a matrix and creates a list containing 4 functions:
## set, using which we can change the matrix we're working with (and sets the
## cached inversion to NULL whenever called);
## get, which allows us to use said matrix;
## setsolve setting the inversion we want to cache (in variable z which exists
## in function's closure);
## getsolve giving us acces to said inversion.
makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    z <<- NULL
    x <<- y
  }
  get = function() x
  setsolve <- function(solve) z <<- solve
  getsolve <- function() z
  list (set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## cacheSolve takes the "matrix" created by above function and returns cached
## value (if it's not NULL) or calculates the inversion (otherwise).
cacheSolve <- function(x, ...) {
  z <- x$getsolve()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setsolve(z)
  z
          
}
