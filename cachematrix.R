## These functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse. 
## inv stands for the inverse
## solve() finds an inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve(x)) inv <<- solve(x)
        getinv <- function() inv
        matrix(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv ## Return a matrix that is the inverse of 'x'
}
