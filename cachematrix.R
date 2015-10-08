## Utility functions that allow to compute and cache the inverse
## of a matrix.
##
## Example of usage:
##    x = cbind(c(4,3),c(3,2))
##    c = makeCacheMatrix(x)
##    cacheSolve(c)

## Wrapper object that act as a cache for the inverse of a matrix.
##
## This is implemented has a list whose components are accessor
## functions to the original matrix and its inverse.
##
## arg dataMatrix: an inversable matrix
## return a list with 4 components (get, set, getInverse 
##        and setInverse)
makeCacheMatrix <- function(dataMatrix = matrix()) {
    # The matrix to be inversed
    matrixInverse <- NULL
    
    # Set the matrix to be inversed
    set <- function(y) {
        dataMatrix <<- y
        matrixInverse <<- NULL
    }
    # Get the original matrix
    get <- function() dataMatrix
    
    # Set the inverse of the matrix
    setInverse <- function(matrixInverse) matrixInverse <<- matrixInverse
    
    # Get the inverse of the matrix
    getInverse <- function() matrixInverse
    
    # Wrapper object for functions sharing a same
    # environment.
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Function that compute the inverse of a matrix and cache
## its result for later use.
##
## The inverse is computed using the R 'solve' function.
## No check is performed if the matrix is inversable!
##
## arg  cacheMatrix: an object of type 'list' obtained from the 
##                   'makeCacheMatrix' function
## arg ... : additional arguments to be passed to the
##           'solve' function
## return the inverse of the matrix, either from the cache or
##        computed
cacheSolve <- function(cacheMatrix, ...) {
    ## Return a matrix that is the inverse the matrix
    matrixInverse <- cacheMatrix$getInverse()
    if(!is.null(matrixInverse)) {
        message("Getting cached matrix inverse")
        return(matrixInverse)
    }
    # The underlying matrix
    dataMatrix <- cacheMatrix$get()
    # Actual inverse operation
    matrixInverse <- solve(dataMatrix, ...)
    # Set inverse into the cache
    cacheMatrix$setInverse(matrixInverse)
    matrixInverse
}
