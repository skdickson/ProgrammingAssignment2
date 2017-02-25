## This R file contains two functions, makeCacheMatrix and cacheSolve, that can be used together to 
## cache the results of the time consuming activity of inverting a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inv <<- NULL

	## defines 'set' function for the special "matrix" object; cached inverse becomes NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}	

	## defines 'get' function for the special "matrix" object
	get <- function() x

	## defines 'setinverse' function for the special "matrix" object
	setinverse <- function(inverse) inv <<- inverse

	## defines 'getinverse' function for the special "matrix" object
	getinverse <- function() inv

	## returns the list containing the four functions: set, get, getinverse, setinverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## will retrieve the inverse from the cache.
## 	- this function makes use of the solve function for calcuating the inverse,
##	- this function assumes the given matrix is invertible
##	- this function assumes the given "matrix" x is a special object created by the function makeCacheMatrix

cacheSolve <- function(x, ...) {
	## First try to get the cached inverse from the "matrix" object
	inv <- x$getinverse()
	
	## If the cached version exists, return it
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	} else {  
		## Otherwise calculate and cache the matrix that is the inverse of 'x'
		data <- x$get()
		inv <- solve(data,...)
		x$setinverse(inv)
		return(inv)
	}
}
