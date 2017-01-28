## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	matrix <- NULL
	set <- function(y) {
		x <<- y
		matrix <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) matrix <<- solve
	getinverse <- function() matrix
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	matrix <- x$getinverse()
	if(!is.null(matrix)) {
		message("getting cached data")
		return(matrix)
	}
	data <- x$get()
	matrix <- solve(data, ...)
	x$setinverse(matrix)
	matrix
}
