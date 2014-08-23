## Two functions that are used to create a 
## special matrix object and cache's its inverse value.

## Creates a sepecial matrix object, which is a list
## containing function to set the value of the matrix,
## get the value of the matrix,
## set the inverse value of the matrix,
## get the inverse value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list (set = set, get = get, setsolve=setsolve, getsolve=getsolve)
}


## Calculates the inverse of the matrix created by 
## makeCacheMatrix function.  
## It first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse 
## value from the cache and skips the computation. 
## Otherwise, it calculates it and sets the value 
## in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getsolve()
  if (!is.null(i)) {
    message ("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}
