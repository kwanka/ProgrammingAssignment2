################################################################
##      Assignment 2 - Caching the Inverse of a Matrix        ##
################################################################

############# General Overview - What the functions do ##########

## The makeCacheMatrix function creates a special "matrix" object which can be used to cache the inverse of a matrix.
	# Example of how to run the function;
	# new_matrix<-makeCacheMatrix(matrix(1:4,2,2))

## The cacheSolve function finds the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse was previously stored it will be returned from the cache with the message "getting cached data", if the inverse has not been previously caclulated the inverse matrix will be generated.
	# Example of how to run the function;
	# cacheSolve(new_matrix)


##################### makeCacheMatrix Function ##################

## makeCacheMatrix: when supplied with an invertable matrix this function will return the original matrix when get() is called. The set() variable allows the matrix to be updated with new values while the set_inverse and get_inverse variables are required and called by the cacheSolve function.


makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y){
		x <<- y
		s <<- NULL
	}
	get<-function()x
	set_inverse<-function(solve) s <<-solve
	get_inverse<-function() s
	list(set=set, get=get, set_inverse=set_inverse,
	get_inverse=get_inverse )
}


######################## cacheSolve Function #####################

## cacheSolve: when supplied with the special "matrix" object from makeCacheMatrix this function will return the inverted matrix from the cache if previously calculated, i.e. if "s" is not NULL than the cached data is returned. If "s" has value of NULL than the inverse is calculated.


cacheSolve <- function(x, ...) {
	s <- x$get_inverse()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data<-x$get()
	s<-solve(data,...)
	x$set_inverse(s)
	s
}
