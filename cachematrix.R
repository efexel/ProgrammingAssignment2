## $Id$
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will
## not discuss here). Your assignment is to write a pair of functions that
## cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # This is essentially a class parameter default
    m  <- NULL

    # 1: set the value of the vector
    set  <- function(y){
            x <<- y
            m <<- NULL
    }

    # 2: get the value of the vector
    get  <- function() {
        x
    }

    # 3: set the value of the inverse
    setinverse  <- function(inverse) {
        m <<- inverse
    }

    # 4: get the value of the inverse
    getinverse  <- function() {
        m
    }

    # Return a list of methods
    return(list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse))
}

## This function computes the inverse of the special "matrix" retValurned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
    ## return a matrix that is the inverse of 'x'
    retVal  <- x$getinverse()
    # Did we get a cached value?
    if (!is.null(retVal)){
        # Yes
        message("Using cached data.")
    } else {
        # No, do the heavy lifting...
        retVal <- solve(x$get(), ...)
        # Cache the result
        x$setinverse(retVal)
    }
    # Return the result.  Sure the keyword 'return' is not needed, but it
    # keeps me sane when I switch languages.
    return(retVal)
}
