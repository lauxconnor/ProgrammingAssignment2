## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv_x <- NULL
	
	set <- function(y){
		x <<- as.matrix(y)
		inv_x <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv_x <<- inverse
	getinv <- function() inv_x

	list(set = set, get = get, setinv = setinv, getinv = getinv)	
}


## Returns a list of functions, callable by their names. set allows for 
## changing the matrix being solved after the object is first defined by 
## assigning x and inv_x new values in the makeCacheMatrix enviorment. 
## get returns the value of the matrix. setinv caches the solved matrix 
## in the makeCacheMatrix enviorment. getinv returns the cached matrix.
## Both get functions find values for x/inv_x in the makeCacheMatrix enviorment.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inv_x <- x$getinv()
	## Check if the inverse of x is cached in inv_x, return if so
	if(!is.null(inv_x)){
		message("Getting cached data")
		return(inv_x)
	}
	## Inverse x if not cached, cache the inverse of x in inv_x
	data <- x$get()
	inv_x <- solve(data, ...)
	x$setinv(inv_x)
	inv_x
}