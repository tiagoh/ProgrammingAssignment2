## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    # writes values of x and m to a new enviroment, 
    # in order to maintain it to the parent enviroment
    x <<- y
    m <<- NULL
  }
  # defines the get function, that returns the local value of x
  get <- function() x
  # defines a function that calculates the inverse matrix of m
  # and stores it into a new enviroment
  setsolve <- function(solve) m <<- solve
  ## return a list with function local values
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
  # stores in local m variable the
  # value returned by the function getsolve()
  # which is part of the x parameter
  m <- x$getsolve()
  if(!is.null(m)) { # verifies if m is not null
    message("getting cached data")
    return(m) # returns m, which contains the cached inverse matrix  
  }
  # call the function get() from the object x, stores it locally into variable data
  data <- x$get()
  # calculates the inverse matrix for the matrix data and stores it locally into m
  m <- solve(data, ...)
  # stores the value of m in the object x by calling its method setresole. in pratice it is caching.
  x$setsolve(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
