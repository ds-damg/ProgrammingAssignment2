## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that creates a matrix that contains function
## to set/get value of the matrix, set.get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
 set <- function(y) {
 	x <<- y
 	m <<- NULL
 }
 ## get/display matrix
 get <- function() x
 ## sets the inverse of the matrix
 setinv <- function(inverse) m <<- inverse
 ## get the inverse. returns NULL if not yet computed
 getinv <- function() m
 list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Computes the inverse of the special matrix (from makeCacheMatrix). 
## If matrix is already calculated, cacheSolve retrieves the inverse

cacheSolve <- function(x, ...) {
        m <- x$getinv()
	##indicates if the return is a cached data or not
	if(!is.null(m)) {
		message("getting cached data.")
	        return(m)
	}
	## get the matrix
	data <- x$get()
	## computes for the inverse of square matrix
	m <- solve(data, ...)
	## sets the inverse 
	x$setinv(m)
	m
}
