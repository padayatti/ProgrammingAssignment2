## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly.
## Following is a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache 
## its inverse.


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
    
}


## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i    
    
    
}

set.seed(77)
m <- matrix(sample.int(100,size=9,replace=TRUE), nrow=3)
d <- makeCacheMatrix(m)
cacheSolve(d)
