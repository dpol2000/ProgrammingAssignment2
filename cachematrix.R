## Put comments here that give an overall description of what your
## functions do

## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x    
    
    setinverse <- function(y) {
        inv <<- y
    }
    
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Compute the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv    
    
}

A = matrix( 
       c(2, 4, 3, 1), # the data elements 
       nrow=2,              # number of rows 
       ncol=2,              # number of columns 
       byrow = TRUE)        # fill matrix by rows 

A

m <- makeCacheMatrix(A)

cacheSolve(m)

cacheSolve(m)