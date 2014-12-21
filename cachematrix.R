## Cached Matrix Functions

## Routines for creating and manipulating cached matrix objects

## makeCacheMatrix takes a matrix and returns a cached matrix object
## currently implemented as a list of four functions with a shared
## environment storing both the matrix and possibly its cached inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
    setinverse <- function(i) inverse <<- i
	getinverse <- function() inverse
	list(set = set,
	     get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}

## cacheSolve takes a cached matrix object (as created by makeCacheMatrix)
## and returns the inverse. Upon the invocation, the inverted matrix is
## cache and returned directly on subsequent invocations.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if (is.null(i)) {
		i <- solve(x$get())
		x$setinverse(i)
	}
	i
}
