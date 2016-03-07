## These functions assist in caching the computationally compelex inverse operation on matricies.

## This function returns a list of functions that let you set/get a matrix and set/retreive the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## returns the calculated or cached mean for the matrix

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
