## This pair of functions will take a matrix,  
## invert it and return the resulting matrix. 
## Additionally, the inverted matrix is cached
## and can be retrieved in subsequent calls.


## Takes an invertable matrix
## Returns a list of functions that:
##      set the value of the matrix and resets cache to NULL
##      get the value of the matrix
##      set the value of the inverted matrix
##      get the value of the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    return(list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve))
}


## Takes a matrix processed by makeCacheMatrix()
## Returns the cached value of the inverted matrix,
##  if available; otherwise, it solves the inversion,
##  caches the value, and returns the value.
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    return(m)
}
