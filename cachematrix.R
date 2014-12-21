## Programming Assignment 2
## This file contains two functions:  makeCacheMatrix and cacheSolve


makeCacheMatrix <- function(x = matrix()) {    	# Input x will be a matrix
	
	# Reset to NULL every time makeCacheMatrix is called
	cache <- NULL
	
	# Set function
	set <- function(y) {    # Input a matrix
		x <<- y    # Save the input matrix
		cache <<- NULL   # Reset to NULL
	}
	
	get <- function() x   # Return the original matrix
	setInverse <- function(inverse) cache <<- inverse
	getInverse <- function() cache
	# List the internal functions so calling function can access.
	list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)

}

cacheSolve <- function(x, ...) {   # The input x is an object created by makeCacheMatrix
	
	cache <- x$getInverse()   # Access x and get the inverse matrix
	
	# Check for NULL to return cached data, if already cached then not NULL
	if(!is.null(cache)) {
		message("Getting cached data...")
		return(cache)
	}
	
	data <- x$get()    # This section is reached if x$getInverse() returns NULL
	
	# Solve data and save it
	cache <- solve(data)
	x$setInverse(cache)  
	cache    # Return the inverse matrix

}
