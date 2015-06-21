## These funtions are designed to solve the Caching of the Inverse of a Matrix
## problem as outlined in Assignment 2 of the R Progrramming course

## Create a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Compute the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed
## then cacheSolve will return the inverse from the cache

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
