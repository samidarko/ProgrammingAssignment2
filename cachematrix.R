## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
    # i represents the cached inverse matrix -> initialized at NULL
    i <- NULL
    
    # when set is used to set a new matrix, the previous matrix is replaced (m) and previous inverse (i) reinitilized
    set <- function(y) {
        m <<- y
        i <<- NULL
    }
    
    # return the current cached matrix (m)
    get <- function() m
    # set the inverse
    setinverse <- function(inverse) i <<- inverse
    # get the inverse
    getinverse <- function() i
    
    # create properties
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(cm, ...) {
    
    ## Return a matrix that is the inverse of 'x
    i <- cm$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- cm$get()
    i <- solve(data, ...)
    cm$setinverse(i)
    i
}
