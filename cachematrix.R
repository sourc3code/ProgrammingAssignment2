## These function provide you with the means to call solve on a matrix,
## without having to recalculate the inverse if matrix has not changed


## This function creates a special "matrix" object that can cache
## its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        ##Super Assignment of matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    ##Function to Super assign matrix inverse
    setinverse <- function(solve) inv <<- solve 
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    ##Cache var empty recompute inverse
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
