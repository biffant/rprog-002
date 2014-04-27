## makeCacheMatrix() prepares container for caching inversed matrixes
## cacheSolve() inverses matrix, with possible reusage of previous calls for the same matrix

## This function should be called BEFORE any call of cacheSolve() within current environment
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) { 		## Init warehouse for source & inversed matrixes
                x <<- y
                m <<- NULL
        }
        get <- function() x 		## Just return source matrix x
        setinversed <- function(inversed) m <<- inversed ## Save given inversed matrix
        getinversed <- function() m	## Return cached inversed matrix
        list(set = set, get = get,
             setinversed = setinversed,
             getinversed = getinversed) ## Return list of four aux functions - for 
					## setting/getting matrix value, setting/getting inversed matrix
}


## This function will inverse given matrix 1st time - and all further calls for the same 
##	matrix x will return saved inversed matrix rapidly
cacheSolve <- function(x, ...) {
	m <- x$getinversed() ## Try to get inversed matrix from a cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m) ## Return cached matrix that is the inverse of 'x'
        }
        data <- x$get() 	## Save matrix x to temp variable "data"
        m <- solve(data, ...) 	## Inverse "data" matrix
        x$setinversed(m) 	## Save inversed matrix in a cache
        m ## Return calculated matrix that is the inverse of 'x'
}
