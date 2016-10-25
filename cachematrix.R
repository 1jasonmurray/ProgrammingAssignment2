## 'makeCacheMatrix' is used to create a matrix, calculate and cache the inverse.  
## 'cacheSolve' is used to either retrieve the inverted matrix from makeCacheMatrix
##  or to have makeCacheMatrix calculate and then cache it.

## Calculate and cache the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	# reset cached inverted matrix
	im <- NULL
	
	# functions to create matrix object and inverted matrix and retrieve them	
	set <- function (y) {
		x <<- y
		im <<- NULL
	}
	get <- function () x
	setim <- function (solved) im <<- solved
	getim <- function () im
	
	# Create list to allow $ form of extract operator
	list(set = set, get = get, setim = setim, getim = getim)
}


## Utilize 'makeCacheMatrix' object to provide inverse of matrix and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	im <- x$getim ()
	if(!is.null(im)) {
		message ("getting cached data")
		return(im)
	}
	data <- x$get()
	im <- solve(data, ...)
	x$setim(im)
	im
}
