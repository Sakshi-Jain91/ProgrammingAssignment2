## A pair of functions that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( m = matrix() ) {

	## Initializing the inverse property
    i <- NULL

    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## getting the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## CacheSolve: This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

   
    data <- x$get()

    m <- solve(data) %*% data

    x$setInverse(m)

    m
}
