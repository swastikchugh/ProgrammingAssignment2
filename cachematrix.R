## There are 2 functions in this file 
## 1) makeCacheMatrix- this function helps create the cachable object of matrix type that we need
## 2) cacheSolve- This functions checks whether the value has been cached or not and it either 
##                returns the value if it has been cached or calculates the value and caches it

## This function helps create the object. It has 2 variables:
## x- this represents the object 
## inv- a matrix used to cache the inverse of the object
## It has 4 methods:
## set()- to initialize the object
## get()- to return the value of the object
## setInverse()- to set the value of inv
## getInverse()- to return the vlue of inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse_input) inv <<- inverse_input
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This is the caching function. It has two steps:
## First is to check if the value has already been cached and return it if it has been cached.
## Second is if it has not been cached, the to calculate the value and cache it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting back cached data")
    return(inv)
  }
  info <- x$get()
  inverse_calculated <- solve(info, ...)
  x$setInverse(inverse_calculated)
  inverse_calculated
}
