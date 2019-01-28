## The function "makeCacheMatrix" creates a special "matrix" that caches its inverse. The function "cacheSolve" solves for the inverse of the previous matrix. If the inverse was already solved previously, then it will just retrieve the inverse. Computing the inverse of a matrix can be done using the solve function.

## "makeCacheMatrix" will do four things, set the value of the matrix, get the value of the matrix, set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	a = NULL
	set <- function(y) {
		x <<- y
		a <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) a <<- inverse
	getinverse <- function() a
	list(set = set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}


## "cacheSolve" calculates the inverse of the matrix created above. It checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
	a <- x$getinverse()
	if(!is.null(a)) {
		message("getting cached data")
		return(a)
	}
	data <- x$get()
	a <- inverse(data, ...)
	x$setinverse(a)
	a
}
