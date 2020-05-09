## Functions that create and compute the inverse of special "matrix

##------------------------------------------------------------------------------------------

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        
        # function that set the value of the matrix
        set <- function(y) {     
                
                # set the value
                x <<- y          
                
                # Clear the cache
                m <<- NULL       
        
        }
        
        # function that get the value of the matrix
        get <- function() x      
        
        # function that set the inverse of the matrix
        setInverse <- function(inverse) m <<- inverse         
        
        # function that get the inverse of the matrix
        getInverse <- function() m         
        
        # Return a list of functions 
                list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        # extract the cached value for the inverse
        m <- x$getInverse()
        
        # Condition that return the cached value if it is not null
        if(!is.null(m)) {
                
                message("getting cached data")
                return(m)
                
        }
        
        # When the chached value is null, it is cached and return it
        
        # Get the value of the matrix
        data <- x$get()
        
        # Calculate inverse
        m <- solve(data, ...)
        
        # Cache the result
        x$setInverse(m)
        
        #return the inverse
        m
}



