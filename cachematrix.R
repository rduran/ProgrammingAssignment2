## JHDS Certification - R Programming
##
## Rodrigo Duran :: https://github.com/rduran/ProgrammingAssignment2
##
## Assignment: Caching the Inverse of a Matrix
##
## Computing the inverse of a square matrix can be done with the solve function
## in R. For example, if X is a square invertible matrix, then solve(X) returns
## its inverse.
##
## For this assignment, assume that the matrix supplied is always invertible.


##
## makeCacheMatrix: This function creates a special "matrix" object that can cache
##                  its inverse
##
makeCacheMatrix <- function(x = matrix()) {
        cached <<- NULL
        
        ## makes sure that cached element is empty
        set <- function(y) {
                x <<- y
                cached <<- NULL
        }
        
        ## Retreives the original matrix
        get <- function() x
        
        ## Caches the inverse
        setInverse <- function(x) cached <<- x
        
        ## Retreives the cached value
        getInverse <- function() cached
        
        ## Expose the API for caching
        list( set = set, get = get, setInverse = setInverse, 
              getInverse = getInverse)
}

##
## cacheSolve: This function computes the inverse of the special "matrix"
##             returned by makeCacheMatrix above. If the inverse has already
##             been calculated (and the matrix has not changed), then the
##             cachesolve should retrieve the inverse from the cache.
##
cacheSolve <- function(x, ...) {
        ## Retrieves the cached inverse matrix
        inv_m <- x$getInverse()
        if( !is.null(inv_m) ) {
                message("getting cached data")
                return(inv_m)
        }
        
        ## If the cached inverse matrix is null, proceed with calculation
        m <- x$get()            # Retrieves the orignal matrix
        inv_m <- solve(m, ...)  # Calculates the inverse matrix
        x$setInverse(inv_m)     # Caches the inverse matrix
        
        inv_m
}
