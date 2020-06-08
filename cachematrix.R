## Our aim in this assignment is to write a pair of functions, namely, 
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix


## makeCacheMatrix is a function which creates a special "matrix" object that 
## can cache its inverse for the input (which is an invertible square matrix, 
## obviously)

makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        set <- function(y) {
                x <<- y
                invr <<- NULL
        }
        get <- function() x
        setinvr <- function(inverse) invr <<- inverse
        getinvr <- function() invr
        list(set = set, get = get, setinvr = setinvr, getinvr = getinvr)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        invr <- x$getinvr()
        if(!is.null(invr)) {
                message("getting cached result")
                return(invr)
        }
        data <- x$get()
        invr <- solve(data, ...)
        x$setinvr(invr)
        invr
}
