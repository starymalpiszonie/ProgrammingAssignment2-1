
## The function create the "special" matrix with an associated cache for inversion
## it uses the generalized matrix inversion function and requieres MASS library
## this is due to the fact that "solve" only works with square matrices
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(ginv) m <<- ginv
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## the function either calculates the matrix inversion, or returns the cached inversion
## if it already has been calculated
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
		# if the inverse has already been calculated, return the cached version
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		# otherwise calculate the inversion, store it in the cache and return the result
        data <- x$get()
        m <- ginv(data, ...)
        x$setsolve(m)
        m
}
