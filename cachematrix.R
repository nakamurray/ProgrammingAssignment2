## Programming Assignment 2
## This is straightforward code following the example given in the
## programming assignment 2.

## Given a matrix, creates a "api" to store and cache the matrix and its
## inverse. The api is a list containing:
##   A set function to change the matrix
##   A get function to return the matrix
##   A setinverse function to store the matrix inverse
##   A getinverse function to return the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) { x <<- y; I <<- NULL }
    get <- function() x
    setinverse <- function(inverse) I <<- inverse
    getinverse <- function() I
    list(set = set,
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

## Given the list returned by the makeCacheMatrix function, computes
## the inverse of the matrix if the inverse is not already cached.
cacheSolve <- function(x, ...) {
    I <- x$getinverse()
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    I <- solve(x$get())
    x$setinverse(I, ...)
    I
}
