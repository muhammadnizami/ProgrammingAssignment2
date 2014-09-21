## this function is slightly modified from the caching vector exam
## ple.

## b is a boolean indicating whether it is cached. Testing whether
## the matrix is a NULL matrix is a waste of time.
## all mean are replaced with matrix.

makeCacheMatrix <- function(x = matrix()) {
	m=matrix(nrow=nrow(x),ncol=ncol(x))
	b <- FALSE
        set <- function(y) {
                x <<- y
		b <<- FALSE
		m=matrix(nrow=nrow(x),ncol=ncol(x))
        }
        get <- function() x
        setmatrix <- function(matsolv) m <<- matsolv
        getmatrix <- function() m
	getiscached <- function() b
	itiscached <- function() b<<-TRUE
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix,
	     getiscached = getiscached,
	     itiscached = itiscached)

}


## this function is slightly modified from the example:
## - mean() is replaced with solve()
## - !is.null(m) is replaced with x$getiscached()
## - x$getmatrix() is only executed if it is already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(x$getiscached()) {
                message("getting cached data")
                return(x$getmatrix())
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
	x$itiscached()
        m

}
