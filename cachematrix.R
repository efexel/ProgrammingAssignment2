
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will
## not discuss here). Your assignment is to write a pair of functions that
## cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	m  <- NULL
	set  <- function(y){
	        x <<- y
	        m <<- NULL 
	}
	get  <- function() {
		x
	}
	setinverse  <- function(inverse) {
		m <<- inverse
	}
	getinverse  <- function() {
		m
	}
	list(set = set, 
	     get = get, 
	     setinverse = setinverse, 
	     getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" retValurned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retValrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
	## return a matrix that is the inverse of 'x'
	retVal  <- x$getinverse()
	if (!is.null(retVal)){
	    message("getting cached data")
	} else {
		retVal <- solve(x$get(), ...)
		x$setinverse(retVal)
	}
	return(retVal)
}
