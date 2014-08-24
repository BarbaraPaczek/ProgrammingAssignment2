## The aim of these two functions is to use cached inverted matrix when possible.

## This function creates a special matrix. The result of thsi function will be used as an argument 
##to the next funxtion, cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  ## This function sets new value to a matrix.
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  ## This function returs the value of x.
  get <- function() x
  
  ## This function assigns the value of solve to s. 
  setsolve <- function(solve) s <<- solve
  
  ## This function returs the value of s, to which inverted matrix is assigned.
  getsolve <- function() s
  
  ## The function makeCacheMatrix returns the list of four functions defined above.
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function returns the inverse of the special matrix, created in the function makeCacheMatrix.
## The function will use the cache of the inverted matrix if possible.

cacheSolve <- function(x, ...) {
  
  ## The cached inverted matrix is returned if it exists.
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  ## If the cached inverted matrix does not exist, the function restores the soacial matrix and 
  ## inverts it.
  data <- x$get()
  s <- solve(data, ...)
  
  ## The result is cached so that it can be used in the future.
  x$setsolve(s)
  s
}
