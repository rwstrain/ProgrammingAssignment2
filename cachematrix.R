## Wade Strain
## August 4, 2019
## Week 3 project assignment
## R programming -- coursera

## this function creates a special "matrix" object that can
## cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	## check if inverse is in cache, if so return
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	## get matrix
	m <- x$get()
	## solve the inverse of matrix 'm'
	i <- solve(m, ...)
	## set inverse matrix
	x$setinverse(i)
	i
}










