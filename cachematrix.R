## These functions provide a way to store a matrix and its inverse in a cache
## so they can be easily called again without having to go through the inverse
## matrix computation.

## makeCacheMatrix takes a matrix as its input and creates a list of 4 functions.
## The functions are "set", "get", "setinv", and "getinv".

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve expects an output from makeCacheMatrix in the form of a list
## of functions. If cacheSolve has already been used to calculate the inverse
## of the matrix passed to makeCacheMatrix, cacheSolve returns the saved 
## inverse matrix. Otherwise, it calculates the inverse matrix, stores it
## for future use with setinv(), and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
