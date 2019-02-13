## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y  #set matrix directly
                m <<- NULL
        }
        get <- function() x
        
        setInverse <- function(z) m <<- z #set the Inverse directly
        getInverse <- function() m
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("data from the cache")
                return(m)
        }
        matr <- x$get()
        m <- solve(matr)
        x$setInverse(m)
        m
}