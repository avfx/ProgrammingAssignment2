## Matrix inversion can be computationally costly. This program caches
## the computed inverse of a matrix, so no recomputation required.

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## retrieves the inverse of the matrix from the cache if it's already computed
## else computes the inverse of the matrix and adds its inverse into the cache

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (!is.null(m)) {
        message('getting cached data')
        return (m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
 
