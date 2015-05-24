## The following functions provide a way to calculate
## a matrix inverse (cacheSolve) and cache the results 
## (makeCacheMatrix) for later use. To use them first
## call makeCacheMatrix with an invertible matrix and 
## return the result to a variable. Pass the variable to 
## cacheSolve.
##
## Example:
## m <- makeCacheMatrix(m)
## i <- cacheSolve(m)

## This function is a helper function to
## cache the value of a matix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## Holds matric inverse
    i <- NULL
    
    ## Set matrix value
    set <- function(y) {
        ## Assign value to object in different environment
        ## (from the current one). Sets the matrix value
        ## and seeds the inverse of the matric to NULL
        x <<- y
        i <<- NULL
    }
    
    ## Return the previously set matrix
    get <- function() x
    
    ## Set the inverse of the matrix
    setInverse <- function(inverse) i <<- inverse
    
    ## Return the previously set inverse or NULL
    getInverse <- function() i
    
    ## Return a list of functions to:
    ## set: Set matrix
    ## get: Return the  matrix
    ## setInverse: Set the matrix inverse
    ## getInverse: Return the matrix inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Return the inverse of a matrix. If it has already been
## calculated then the cached inverse will be returned.
## Note: Works only for invertible matrixes.
cacheSolve <- function(x, ...) {
    ## Get the cached matrix inverse
    i <- x$getInverse()
    
    ## If the matrix inverse was previously cached
    ## return it
    if(!is.null(i)) {
        message("getting inverse of matrix")
        return(i)
    }
    
    ## If not previous cached calculate it
    ## and the cache to result
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    
    ## Return the matrix inverse
    i
}
