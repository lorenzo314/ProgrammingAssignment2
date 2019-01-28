## Compute the inverse of a given matrix and caches it so that
## it is not necessary to recompute it every time the inverse is needed
## It is assumed that the matrix is invertible: no check is done

## Store a given matrix x so that its inverse may be later computed

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inv <<- inv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## If the inverse of x is already cached return it
## otherwise compute the inverse, cache the result and return it

cacheSolve <- function(x, ...) {
	m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
