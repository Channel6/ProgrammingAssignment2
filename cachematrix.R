## cachematrix.R:
## compute a matrix object's inverse, then store it in the cache for easier retrieval

## makeCacheMatrix: create a special "matrix" object that can cache its inverse.
## Eseentially, this function emulates a matrix object by generating a list with
## callable objects. These objects perform the following tasks:
## # Create and set the matrix for callback;
## # get the matrix;
## # compute and store the matrix inverse;
## # retrieve the matrix inverse.
makeCacheMatrix <- function(x = matrix()) {
	inverted <- NULL
	set <- function(y) {
		x <<- y
		inverted <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inverted <<- solve
	getinverse <- function() inverted
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve: Calculate the inverse of the special aforementioned "matrix".
## The methodology as follows: 
## # Check for previously-computed matrix inverse;
## # If alredy computed, retrieve inverse from cache and bypass computation;
## # If not, calculate inverse of this "matrix", and cache value of the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverted <- x$getinverse()
	if(!is.null(inverted)) {
		message("##getting cached data")
		return(inverted)
	}
	data <- x$get()
	inverted <- solve(data, ...)
	x$setinverse(inverted)
	inverted
}
## Testing:
# > test <- matrix(1:4, nrow = 2, ncol = 2)
# > storage <- makeCacheMatrix(test)
# > storage$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
#
# # get inverse from fresh matrix
# > storage$getinv()
# NULL
# # no previous inverse, so make one
# > cacheSolve(storage)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#  retrieve inverse from cache
# > cacheSolve(storage)
# ##getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

