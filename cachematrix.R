## We are defining two functions below, the first makeCacheMatrix returns a specialized matrix which can be cached


## makeCacheMatrix function performs the inverse of a matrix and caches the result

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x 
        setinv <- function(solve) m <<- solve 
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve function invokes getinv() function within makeCacheMatrix function 
## and returns the cache value if it exists else it calculates and returns the inverse values

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached values for inverse of matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
