## The functions create a matrix out of an original matrix, that is carrying functions to store
## an inverse matrix of the original matrix. Before the inverse is calculated, the functions are checking if the inverse has already
## been calculated and, if yes, take the inverse from the cache.

## makeVector creates a special "matrix", which is really a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the solve
##get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## This function calculates the inverse of a matrix, but before calculating checks, if the inverse is already stored in the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}

