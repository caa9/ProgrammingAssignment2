## Define functions that calculate inverse of matrix, 
## and cache for future requests for same matrix.
##
## Assumes that input matrix is always invertible.

## Given a matrix,
## return a list containing setter and getter for both original matrix
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse matrix
	i <- NULL

  # define (input) matrix setter/getter
  set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
  
  # define inverse matrix setter/getter
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
  
  # return setters/getters
	list(
		set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse
  )
}


## Given matrix,
## return cached inverse matrix OR
## calculate (and cache!) and return inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  # return cached inverse, if available
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
  
  # calculate inverse, cache it and return it
	m <- x$get() # make input data into matrix
	i <- solve(m)
	x$setinverse(i)
	i
}
