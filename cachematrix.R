## The functions create a special matrix object when used in conjunction with
## the cacheSolve function stores the matrix inverse after the first time it 
## solved.

## Creates a matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## Solves the inverse of a matrix checking if it has a cached first.

cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	if (!is.null(i)) {
		message("Getting cached data.")
		return(i)
	}
	matrix <- x$get()
	i <- solve(matrix)
	x$setInverse(i)
	i	
}



