## Put comments here that give an overall description of what your
## functions do

#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{ 
    #begin by setting the mean to NULL as a placeholder for a future value
    inv <- NULL 
    set <- function(y) 
    { 
        x <<- y 
        inv <<- NULL 
    } 
    get <- function() x 
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv 
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
} 
#This function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above. 
#If the inverse has already been calculated (and the matrix has not changed), then  cacheSolve  should retrieve the inverse from the cache
cacheSolve <- function(x, ...) 
{ 
    inv <- x$getinverse()
    #Check if results exists then pull from cache else get new
    if(!is.null(inv)) 
    { 
        message("retrieving the cached data.") 
        return(inv) 
    } 
    data <- x$get()
    #Compute the inverse of a square matrix 
    inv <- solve(data) 
    x$setinverse(inv) 
    inv 
} 
