## These two functions are used for caching the matrix inverse so that when we need it again, it can be looked up in the cache rather than recomputed

## This function returns a list of four functions to be called in the next function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## This function first check if a inverse is already been calculated. If not, it will compute the reverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cacheSolve <- function(x, ...) {
            m <- x$getinverse()
            if(!is.null(m)) {
                message("getting cached data")
                return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
        }
}
