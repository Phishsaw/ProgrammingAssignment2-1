##Write the following functions:
##makeCacheMatrix: This function creates a special "matrix" 
##object that can cache its inverse.



makeCacheMatrix <- function(x = matrix()){
	inv <- NULL
	set <- function(y) { 						##defines a function to set the matrix 
                x <<- y							##to a new matrix , y, and resets the inverse, inv, to NULL
                inv <<- NULL
        }
        get <- function() x  					        ##returns the matrix, x
        setinverse <- function(solve) inv <<- solve	                ##sets the inverse, inv, to solve
        getinverse <- function() inv 				        ##returns the inverse, inv
        list(set = set, get = get, setinverse = setinverse,  getinverse = getinverse)##returns the 'special matrix'
	
}


##cacheSolve: This function computes the inverse of the 
##special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix 
##has not changed), then the cachesolve should retrieve the 
##inverse from the cache.


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()				##checks for cached inverse 
        if(!is.null(inv)) {				##if inverse exists then get cached data
                message("getting cached data")	        ##Prints "getting cached data"
                return(inv)				##return the cached inverse data
        }
	else{					        ##if cached data doesn't exist
        data <- x$get()					##get current matrix data
        inv <- solve(data, ...)				##perform the inverse for the matrix			
        x$setinverse(inv)				##set the inverse data in cache
	   }
	   return(inv)					##return the inverse matrix
}
