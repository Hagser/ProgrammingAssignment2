# "makeCacheMatrix" creates a special "matrix" object and saves it in the cache.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}

# "cacheSolve" solves the inverse of the matrix object returned from makeCacheMatrix.
# If the inverse has previously been solved and the matrix has not been changed, it returns the cached value. 

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()        
        m <- solve(data)
        x$setinv(m)
        m
}
