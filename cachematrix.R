## cachematrix.R
##
## Programming Assignment 2 for R Programming
## Student - davidsaintruby
##

## changelog
## 
## date		change	
## -------	-------------------
## 082104	initialversion
##


## These two functions demonstrate the use of scoping within R
##
## makeCacheMatrix	this function instances a matrix that can optionally
##	 		store its own inverse
##
## cacheSolve		this function returns the inverse of a matrix, retrieving it
##			from the cache if available



## makeCacheMatrix
## this function creates a list to store a matrix, and compute or store its inverse
## note the <<- operator to store in an outside environment

makeCacheMatrix <- function(x = matrix()) {

	mi <- NULL		#init mi to NULL - stores matrix inverse

	set <- function(y) {	#set the matrix value
		x <<- y
		mi <<- NULL
	}

	get <- function() x	#return the matrix value

	setinverse <- function(solve) mi <<- solve	#set the inverse

	getinverse <- function() mi 			#return the inverse

	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)	

}


## cacheSolve
## this function returns an inverse of a matrix
## it uses the cached value if available
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	mi <- x$getinverse()	#try to get the cached value

	if(!is.null(mi)) {				#test for the cached value - if so return it
		message("getting cached data")
		return(mi)
	}

	data <- x$get()			#does not exist in cache - so solve and store
	mi <- solve(data, ...)
	x$setinverse(mi)
	mi				#return the value
		
}
