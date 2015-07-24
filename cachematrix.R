
## The following code represents a pair of functions 
## that cache and compute the inverse of a matrix.

## This function makeCacheMatrix:
## creates a matrix object and will cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function cacheSolve:
## Computes the inverse of the matrix returned by makeCacheMatrix (above). 
## If the inverse matrix exists and and has not not changed, then
## cacheSolve will retrieve the inverse matrix from cache.

cacheSolve <- function(mtx, ...) {
    inverse <- mtx$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mtx$get()
    invserse <- solve(data, ...)
    mtx$setinv(inverse)
    return(inverse)
}