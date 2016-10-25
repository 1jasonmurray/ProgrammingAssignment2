## 'makeCacheMatrix' is used to create a matrix object that stores the matrix and it's inverse.  
## 'cacheSolve' is used to either retrieve the cached inverted matrix from the makeCacheMatrix object or calculate and cache it

## Create matrix object that stores the original matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
	# reset cached inverted matrix
	im <- NULL
	
	# functions to store a matrix and it's inverted matrix and retrieve them	
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


## Looks for cached inverse matrix in 'makeCacheMatrix' object or caclulates and stores it there

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
