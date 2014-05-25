## These two functions handle the creation of a matrix object which includes the calculation and caching of an
## inverse matrix

## This function contains the get and set operations for the original matrix and the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setinv <- function(inv) mi <<- inv
        getinv <- function() mi
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function checks if the inverse matrix has laready been cached. If so, it returns the cached matrix
## otherwise it calculates the inverse, stores it in the cache and returns it to the calling functtion

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
	message("calculating inverse")
        mat <- x$get()
        inv <- solve(mat)
        x$setinv(inv)
        inv
}
