## two functions that cache the inverse of a matrix(assumed invertiable)

## makeCacheMatrix creates a special "matrix" object which cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


##cacheSolve calculates the inverse of the special "matrix" above, and it checks first whether the invese
##has been calculated. If so "gets" from cache, otherwise calculates and sets via setsolve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m

}
