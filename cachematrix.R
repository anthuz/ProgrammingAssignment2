## makeCacheMatrixc reates a special "matrix" object that can cache its inverse.
## cacheSolve return the inverse of the matrix if it has already been calculated.
## Otherwise it computes the inverse, cache it and returns it.

# makeCacheMatrix, special "matrix" object that can cache its inverse.
# set = set a value of the matrix
# get = get a value of the matrix
# setinverse = set the inverse of the matrix
# getinverse = get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv

	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


# cacheSolve computes the inverse of the special "matrix". It first checks if the 
# inverse has already been calculated and returns it if it has.
# Otherwise it computes the inverse, store/cache it with the setinverse function
# in the object makeCacheMatrix and returns it.
cacheSolve <- function(x, ...) {
	# Get the inverse of the matrix 'x'
	inverse <- x$getinverse()

	# Check if the inverse has already been calculated
	if(!is.null(inverse)) {
		message("getting cached data")

		# return cached inversed matrix and exit the function
		return(inverse)
	}

	# get the matrix
	data <- x$get()

	# compute the inverse of the matrix
	inverse <- solve(data, ...)

	# save/store the inverse of the matrix
	x$setinverse(inverse)

	# return the inverse of the matrix
	inverse
}
