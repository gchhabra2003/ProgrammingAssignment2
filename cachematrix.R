## This file contains 2 functions which are used to cache the matrix and its inverse so that on rerun of same matrix, inverse of matrix is fetched from the cache and not recomputed. 

## makeCacheMatrix saves the result of inverse matrix in the cache using sub functions set(), get(), setinverse() and getinverse().

makeCacheMatrix <- function(x = matrix()) {
        ic <- NULL
        set <- function(y) {
                x <<- y
                ic <<- NULL
        }
        get <- function() x
        setinverse <- function(solveic) ic <<- solveic
        getinverse <- function() ic
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function cacheSolve is a wrapper for Solve function which computes the inverse of matrix. The difference is that it first checks the cache memory and if result is found for same matrix then inverse is not recalculated to save computing time.
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ic <- x$getinverse()
        if(!is.null(ic)) {
                message("getting cached data")
                return(ic)
        }
        data <- x$get()
        ic <- solve(data, ...)
        x$setinverse(ic)
        ic
}
