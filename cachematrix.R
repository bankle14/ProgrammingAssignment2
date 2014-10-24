## This is a pair of functions that cache the inverse of a matrix.  The first creates a special "matrix" object that can cache its inverse.  
##The second computes the inverse of the special "matrix" returned by the first. If the inverse has already been calculated (and the matrix has not changed), it retrieve the inverse from the cache. 


##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL  		
        set <- function(y) {  		
	##function to set the value of the matrix
                x <<- y
                invs <<- NULL
        }
        get <- function() {x} 		
	#function to get the value of the matrix
        setinverse <- function(solve) {invs <<- solve} 		
	#function to set the inverse of the matrix 
        getinverse <- function() {invs} 		
	#function to get the inverse of the matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  		
	# returns a list of the functions to be used by cacheSolve function

}
## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix`. If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
 	invs <- x$getinverse() 
	# Looks for a cached inverse of x
        if(!is.null(invs)) {
	# If cached inverse is found, returns it and exits function
                message("getting cached data")
                return(invs)
        }
        data <- x$get()
        # gets the matrix without cahced inverse
	invs <- solve(data, ...)
	#calculates inverse
        x$setinverse(invs)
        invs
	#sets the inverse and then returns the value
}

