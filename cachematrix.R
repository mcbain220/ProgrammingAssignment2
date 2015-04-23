## These functions create the getter/setter methods of caching and retrieving
## the inversion of a matrix.


## This function creates the list of functions to get/set the value of the
## original and inverted matrices.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvert <- function(solve) m <<- solve
    getinvert <- function() m
    list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
}


## This function checks to see if the inverted matrix is cached or has changed
## and stores the updated inverted matrx.

cacheSolve <- function(x, ...) {
        m <- x$getinvert()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinvert(m)
        m
}

