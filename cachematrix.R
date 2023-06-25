## Inverse Matrix

## Getting a matrix, Save it and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## Check there is a inverse matrix in cache or not

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if (!is.null(m)) {
                message("Getting Cache Data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}